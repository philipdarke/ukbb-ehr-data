# Diabetes phenotyping
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

library(data.table)
library(lubridate)
library(zoo)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants.rds"))
data_period <- readRDS(paste0(output_path, "data_period.rds"))
diagnoses_raw <- readRDS(paste0(output_path, "diagnoses.rds"))
glucose_raw <- readRDS(paste0(output_path, "blood_glucose.rds"))
prescriptions <- readRDS(paste0(output_path, "prescriptions.rds"))

# DIABETES / DIABETES REMISSION ------------------------------------------------

# Initial data cleaning --------------------------------------------------------

# EHR diabetes diagnoses
diagnoses <- diagnoses_raw[variable == "diabetes" & source == "ehr"]

# Drop duplicate codes on same date
diagnoses <- diagnoses[, head(.SD, 1), by = c("eid", "date", "level")]

# Drop unknown diabetes type if recorded on same day as a known type
drop_events <- dcast(diagnoses, eid + date ~ level, fun.aggregate = length)
drop_events[, known := rowSums(.SD), .SDcols = 4:7]
diagnoses <- merge(diagnoses,
                   drop_events[, .(eid, date, drop = `NA` >= 1 & known >= 1)],
                   by = c("eid", "date"))
diagnoses <- diagnoses[drop == FALSE | (drop == TRUE & !is.na(level))]
diagnoses[, drop := NULL]

# Sort by date
diagnoses <- diagnoses[order(eid, date, level)]

# Diabetes diagnosis -----------------------------------------------------------

# Date of first diabetes diagnosis
diabetes_codes <- diagnoses[is.na(level) | level %in% c("type1", "type2")]
first_diabetes <- diabetes_codes[, .(date = head(date, 1)), by = eid]

# Add diabetes type
diabetes_type <- diabetes_codes[!is.na(level),
                                .(type1 = all(level == "type1"),
                                  type2 = all(level == "type2"),
                                  other = all(level == "other")),
                                by = eid]
diabetes_type <- melt(diabetes_type, id.vars = "eid")
diabetes_type <- diabetes_type[value == TRUE, .(eid, type = variable)]
diabetes <- merge(first_diabetes, diabetes_type, by = "eid", all = TRUE)

# Impute unknown diabetes types ------------------------------------------------

# Age at diagnosis
diabetes <- merge(diabetes, participants[, .(eid, dob)], by = "eid", all.x = TRUE)
diabetes[, age := time_length(dob %--% date, unit = "years")]

# Time to first insulin prescription (if any)
insulin_prescriptions <- prescriptions[category == "diabetes" & 
                                         type == "insulin"][order(eid, from)]
first_insulin <- insulin_prescriptions[,
                                       .(first_insulin = head(from, 1)),
                                       by = eid]
diabetes <- merge(diabetes, first_insulin, by = "eid", all.x = TRUE)
diabetes[, time_to_insulin := time_length(date %--% first_insulin, unit = "years")]

# Estimate diabetes type
diabetes[is.na(type) & (age + time_to_insulin) < 36, type := "type1"]  # started insulin before age 36
diabetes[is.na(type) & age >= 35 & is.na(time_to_insulin), type := "type2"]  # 35+ at diagnosis and no insulin prescription
diabetes[is.na(type), type := "type2"]  # otherwise assume type 2 diabetes

# Subsequent periods of type 2 diabetes/remission (if any) ---------------------

# Drop tests before type 2 diabetes diagnosis
glucose <- merge(glucose_raw,
                 diabetes[type == "type2", .(eid, diagnosis = date)],
                 by = "eid")
glucose <- glucose[date > diagnosis]

# Drop glucose tests during periods of anti-diabetes medication
glucose <- merge(glucose,
                 prescriptions[category == "diabetes" & type == "any"],
                 by = "eid",
                 all.x = TRUE,
                 allow.cartesian = TRUE)
glucose <- glucose[is.na(from) | !(date %within% (from %--% to)), .(eid, date, result)]
glucose <- unique(glucose)

# Add dummy diabetic glucose test result on date of first occurrence of diabetes
glucose <- rbind(glucose,
                 diabetes[type == "type2", .(eid, date, result = "diabetic")])

# Add dummy diabetic glucose test result at start of each period of
# anti-diabetes medication after diagnosis
diabetes_prescriptions <- prescriptions[eid %in% diabetes[type == "type2", eid] &
                                          category == "diabetes" & type == "any"]
diabetes_prescriptions <- merge(diabetes_prescriptions, first_diabetes, by = "eid")
diabetes_prescriptions[date %within% (from %--% to), from := date]
diabetes_prescriptions <- diabetes_prescriptions[from >= date]
glucose <- rbind(glucose,
                 diabetes_prescriptions[,
                                        .(eid,
                                          date = from,
                                          result = "diabetic")])
glucose <- glucose[order(eid, date)]

# Handle edge cases e.g. start diabetes medication on date of normoglycaemic test
glucose <- unique(glucose)
glucose[, result := factor(result,
                                 levels = c("normo", "pre", "diabetic"),
                                 labels = 1:3)]
glucose[, result := as.numeric(result)]
glucose <- glucose[, .(result = max(result)), by = c("eid", "date")]  # take worst result if a conflict
glucose[, result := factor(result,
                                 levels = 1:3,
                                 labels = c("normo", "pre", "diabetic"))]

# Split glucose data into periods starting on each diabetic test result
glucose[result == "diabetic", period := 1:.N, by = eid]
glucose[, period := na.locf(period), by = eid]
glucose[, N := .N, by = c("eid", "period")]  # tests in each period

# Months since first non-diabetic test in each period
glucose[N > 1,
        gap := c(NA, time_length(date[2] %--% date[-1], unit = "months")),
        by = c("eid", "period")]

# Identify date of remission
glucose[gap >= remission_gap, remission := head(date, 1), by = c("eid", "period")]
glucose[date != remission, remission := NA_Date_]

# Identify date of recurrence of diabetes (if any)
glucose <- glucose[!is.na(remission) | result == "diabetic"]
glucose[, type2 := c(date[1], remission[-.N]), by = eid]
glucose[!is.na(type2), type2 := date]
glucose <- glucose[!is.na(type2) | !is.na(remission), .(eid, type2, remission)]

# Add from and to dates for periods of type 2 diabetes and remission
diabetes_pheno <- melt(glucose,
                       id.vars = "eid",
                       na.rm = TRUE,
                       variable.name = "state",
                       value.name = "from")[order(eid, from)]
diabetes_pheno[, to := c(from[-1] - 1, NA_Date_), by = eid]

# Add other types of diabetes
diabetes_pheno <- rbind(diabetes_pheno,
                        diabetes[type != "type2",
                                 .(eid, state = type, from = date)],
                        fill = TRUE)

# End final period on censor date
censor_date <- data_period[order(eid, to)][, .(censor_date = tail(to, 1)), by = eid]
diabetes_pheno <- merge(diabetes_pheno, censor_date, by = "eid")
diabetes_pheno[is.na(to), to := censor_date]
diabetes_pheno[, censor_date := NULL]

# Gestational diabetes ---------------------------------------------------------

# Date of each gestational code
gestational_codes <- diagnoses[level == "gestational",
                               .(eid, code_start = date, code_end = date %m+% gestational_duration)]

# Add date of subsequent diabetes diagnosis (if any)
gestational_codes <- merge(gestational_codes,
                           first_diabetes[, .(eid, diagnosis = date)],
                           by = "eid", all.x = TRUE)
gestational_codes <- gestational_codes[is.na(diagnosis) | diagnosis >= code_start]  # ignore gestational codes after diagnosis

# Add dates(s) of first subsequent normoglycaemic test (if any)
gestational_codes <- merge(gestational_codes,
                           glucose_raw[result == "normo", .(eid, normo_test = date)],
                           by = "eid", all.x = TRUE)
glucose_eids <- gestational_codes[, .(glucose = any(normo_test %within% (code_start %--% code_end))), by = eid][glucose == TRUE, eid]
glucose_tests <- gestational_codes[eid %in% glucose_eids & normo_test >= code_start & normo_test <= code_end]
glucose_tests <- glucose_tests[, head(.SD, 1), by = c("eid", "code_start")]  # first test
gestational_codes <- rbind(glucose_tests,
                           gestational_codes[!(eid %in% glucose_eids),
                                             .(eid, code_start, code_end, diagnosis, normo_test = NA_Date_)])

# Assumed gestational diabetes period is `gestational_duration` months from first code (end at subsequent diabetes diagnosis or normoglycaemic test if earlier)
gestational_codes[, end := pmin(code_end, diagnosis, normo_test, na.rm = TRUE)]
gestational_codes <- unique(gestational_codes[, .(eid, code_start, end)])
gestational_pheno <- data.table()
while(gestational_codes[, .N] > 0) {
  first_period <- gestational_codes[, .(from = head(code_start, 1), to = head(end, 1)), by = eid]
  gestational_pheno <- rbind(gestational_pheno, first_period)
  gestational_codes <- merge(gestational_codes, first_period, by = "eid")
  gestational_codes <- gestational_codes[!(code_start %within% (from %--% to)), 1:3]
}

# Final periods of gestational diabetes
gestational_pheno[, state := "gestational"]

# Finalise results
diabetes_pheno <- rbind(diabetes_pheno, gestational_pheno)[order(eid, from)]

# PRE-DIABETES -----------------------------------------------------------------

# First occurrence of pre-diabetes ---------------------------------------------

# Drop glucose tests after first diabetes diagnosis
glucose <- merge(glucose_raw,
                 diabetes_pheno[state != "gestational", .(diagnosis = head(from, 1)), by = eid],
                 by = "eid", all.x = TRUE)
glucose <- glucose[is.na(diagnosis) | date < diagnosis %m-% diagnosis_lag, .(eid, date, result)]

# Date of first elevated blood glucose test
glucose <- glucose[order(eid, date)]
pre_diabetes <- glucose[result != "normo", .(pre_diabetes = head(date, 1)), by = eid]

# Subsequent periods of pre-diabetes/remission (if any) ------------------------

# Tests after pre-diabetes onset
glucose <- merge(glucose[, .(eid, date, result)], pre_diabetes, by = "eid")
glucose <- glucose[date > pre_diabetes]

# Drop glucose tests during periods of anti-diabetes medication
glucose <- merge(glucose,
                 prescriptions[category == "diabetes" & type == "any", .(eid, drug_from = from, drug_to = to)],
                 by = "eid", all.x = TRUE, allow.cartesian = TRUE)
glucose <- glucose[is.na(drug_from) | !(date %within% (drug_from %--% drug_to)),
                   .(eid, date, result)]

# Add dummy pre-diabetic glucose test result on date of first occurrence of pre-diabetes
glucose <- rbind(glucose,
                 pre_diabetes[, .(eid, date = pre_diabetes, result = "pre")])

# Add dummy diabetic glucose test result at start of each period of anti-diabetes medication
glucose <- rbind(glucose,
                 prescriptions[eid %in% pre_diabetes$eid & category == "diabetes" & type == "any",
                               .(eid, date = from, result = "pre")])
glucose <- glucose[order(eid, date)]

# Handle edge cases e.g. start diabetes medication on date of normoglycaemic test
glucose <- unique(glucose)
glucose[, result := factor(result,
                           levels = c("normo", "pre", "diabetic"),
                           labels = 1:3)]
glucose[, result := as.numeric(result)]
glucose <- glucose[, .(result = max(result)), by = c("eid", "date")]  # take worst result if a conflict
glucose[, result := factor(result,
                           levels = 1:3,
                           labels = c("normo", "pre", "diabetic"))]

# Split glucose data into periods starting on each non-normoglycaemic test result
glucose[result != "normo", period := 1:.N, by = eid]
glucose[, period := na.locf(period), by = eid]
glucose[, N := .N, by = c("eid", "period")]  # tests in each period

# Months since first non-diabetic test in each period
glucose[N > 1,
        gap := c(NA, time_length(date[2] %--% date[-1], unit = "months")),
        by = c("eid", "period")]

# Identify date of return to normoglycaemia
glucose[gap >= remission_gap, normo := head(date, 1), by = c("eid", "period")]
glucose[date != normo, normo := NA_Date_]

# Identify date of recurrence of pre-diabetes (if any)
glucose <- glucose[!is.na(normo) | result != "normo"]
glucose[, pre := c(date[1], normo[-.N]), by = eid]
glucose[!is.na(pre), pre := date]
glucose <- glucose[!is.na(pre) | !is.na(normo), .(eid, pre, normo)]

# Add from and to dates for periods of pre-diabetes
pre_pheno <- melt(glucose,
                  id.vars = "eid",
                  na.rm = TRUE,
                  variable.name = "state",
                  value.name = "from")[order(eid, from)]
pre_pheno[, to := c(from[-1] - 1, NA_Date_), by = eid]

# Finalise ---------------------------------------------------------------------

# Add pre-diabetes to results
diabetes_pheno <- rbind(diabetes_pheno, pre_pheno[state == "pre"])[order(eid, from)]

# End final period on censor date
censor_date <- data_period[order(eid, to)][, .(censor_date = tail(to, 1)), by = eid]
diabetes_pheno <- merge(diabetes_pheno, censor_date, by = "eid")
add_censor_date <- diabetes_pheno[, .(missing_date = is.na(tail(to, 1))), by = eid][missing_date == TRUE, eid]
diabetes_pheno[eid %in% add_censor_date & is.na(to), to := censor_date]

# End pre-diabetes period at subsequent diabetes diagnosis
add_missing_date <- diabetes_pheno[is.na(to), unique(eid)]
if (length(add_missing_date) != 0) {
  diabetes_date <- diabetes_pheno[eid %in% add_missing_date &
                                    state %in% c("type1", "type2", "other"),
                                  .(diabetes_date = head(from, 1)),
                                  by = eid]
  diabetes_pheno <- merge(diabetes_pheno, diabetes_date, by = "eid", all.x = TRUE)
  diabetes_pheno[eid %in% add_missing_date & is.na(to), to := diabetes_date - 1]
}

# Save data --------------------------------------------------------------------

diabetes_pheno <- diabetes_pheno[, .(eid, state, from, to)]
saveRDS(diabetes_pheno, paste0(output_path, "diabetes_phenotype.rds"))
