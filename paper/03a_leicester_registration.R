# Evaluate Leicester score - as https://doi.org/10.1111/j.1464-5491.2010.03037.x
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

library(data.table)
library(lubridate)
library(stringr)
library(plyr)
library(caret)
library(ukbbhelpr)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants.rds"))
data_period <- readRDS(paste0(output_path, "gp_reg_raw.rds"))
visit_data <- readRDS(paste0(output_path, "visit_data.rds"))
demographic <- readRDS(paste0(output_path, "demographic.rds"))
diagnoses <- readRDS(paste0(output_path, "diagnoses.rds"))
biomarkers <- readRDS(paste0(output_path, "biomarkers.rds"))
family_history <- readRDS(paste0(output_path, "family_history.rds"))
prescriptions_raw <- readRDS(paste0(output_path, "prescriptions_raw.rds"))
ehr_presc_codes <- readRDS(paste0(codeset_path, "drugs/prescriptions.rds"))
visit_presc_codes <- get_coding(4)

# Parameters
study_length <- 5  # years

# Study design -----------------------------------------------------------------

# Select participants registered with a GP at first visit
first_visit <- visit_data[, .(eid, first_visit = `53-0.0`)]
data_period <- merge(data_period, first_visit, by = "eid")
study_data <- data_period[first_visit %within% (reg_date %--% deduct_date)]

# Select longest registration period of more than one in force at study start
study_data[, follow_up := time_length(first_visit %--% deduct_date, unit = "years")]
study_data <- study_data[order(eid, -follow_up)]
study_data <- study_data[, head(.SD, 1), by = eid]

# Add study start date, sex, dob and date of 25th birthday
study_data[, study_start := first_visit]
study_data <- merge(study_data, participants[, .(eid, sex, dob)], by = "eid")
study_data[, age_25 := dob + years(25)]
study_data[, sex := factor(sex, levels = 0:1, labels = c("Female", "Male"))]

# Entry date for each participant
study_data[, entry_date := pmax(study_start, reg_date + years(1), age_25)]
study_data <- study_data[entry_date < deduct_date,
                         .(eid, sex, dob, entry_date, censor_date = deduct_date)]

# Add follow-up
study_data[, followup := time_length(entry_date %--% censor_date, unit = "years")]

# Outcomes ---------------------------------------------------------------------

# First diabetes disclosure/code of any type
diagnoses <- merge(diagnoses, study_data[, .(eid, entry_date)], by = "eid")
diabetes_outcomes <- diagnoses[variable == "diabetes" & date > entry_date,
                               .(diagnosis = head(date, 1), event = TRUE),
                               by = eid]

# Age at diagnosis
diabetes_outcomes <- merge(diabetes_outcomes, participants[, .(eid, dob)], by = "eid")
diabetes_outcomes[, age := time_length(dob %--% diagnosis, unit = "years")]

# Insulin within a year of diagnosis (self-reported)
visit_insulin <- visit_extract(visit_data, 2986)
diabetes_outcomes[eid %in% visit_insulin[value == 1, eid], time_to_insulin := 0]

# Date of first insulin prescription (EHRs)
prescriptions_raw <- prescriptions_raw[order(type, issue_date)]
first_insulin <- prescriptions_raw[category == "diabetes" & type == "insulin",
                                   .(insulin_date = head(issue_date, 1)),
                                   by = eid]
diabetes_outcomes <- merge(diabetes_outcomes, first_insulin, by = "eid", all.x = TRUE)

# Estimate diabetes type
diabetes_outcomes[!is.na(insulin_date) & is.na(time_to_insulin),
                  time_to_insulin := time_length(diagnosis %--% insulin_date, unit = "years")]
diabetes_outcomes[(age + time_to_insulin) < 36, type := "type1"]  # started insulin before age 36
diabetes_outcomes[is.na(type) & age >= 35 & is.na(time_to_insulin), type := "type2"]  # 35+ at diagnosis and no insulin prescription
diabetes_outcomes[is.na(type), type := "type2"]  # otherwise assume type 2 diabetes

# Add to data
study_data <- merge(study_data,
                    diabetes_outcomes[type == "type2", .(eid, diagnosis, event = TRUE)],
                    by = "eid", all.x = TRUE)
study_data[is.na(event), event := FALSE]

# Ignore diagnoses after censor date e.g. those made in a later period of data collection
study_data[diagnosis > censor_date, c("diagnosis", "event") := .(NA_Date_, FALSE)]

# Add time to event
study_data[event == FALSE, time := followup]
study_data[event == TRUE, time := time_length(entry_date %--% diagnosis, unit = "years")]

# Predictors -------------------------------------------------------------------

# Ethnicity (from UK Biobank visit data)
ethn <- demographic[!is.na(ethnicity), .(eid, date, ethn = ethnicity)][order(eid, date)]
ethn <- ethn[, .(ethn = tail(ethn, 1)), by = eid]  # take most recent ethnicity if multiple records
ethn[, ethn := as.character(ethn)]
ethn[substr(ethn, 1, 1) == "1", ethn := "White"]
ethn[ethn != "White", ethn := "Other"]
study_data <- merge(study_data, ethn, by = "eid", all.x = TRUE)

# Family history of diabetes (disclosed at any point in data)
study_data[, fhdm := eid %in% family_history[variable == "diabetes", unique(eid)]]

# Hypertension prescriptions (EHR)
prescriptions_raw <- merge(prescriptions_raw, study_data[, .(eid, entry_date)], by = "eid")
htn_eids <- prescriptions_raw[category == "anti_hypertensives" & issue_date <= entry_date, unique(eid)]  # on/before study entry

# Hypertension prescriptions (self-reported)
hypertension_search <- sapply(ehr_presc_codes$hypertension, function (codes) {
  codes$search
})
hypertension_search <- unique(unlist(hypertension_search))
hypertension_search <- regex(paste(hypertension_search, collapse = "|"), ignore_case = TRUE)
hypertension_codes <- visit_presc_codes[str_detect(meaning, hypertension_search), unique(coding)]
visit_prescs <- visit_extract(visit_data, 20003)
visit_prescs <- merge(visit_prescs, study_data[, .(eid, entry_date)], by = "eid")
htn_eids <- union(htn_eids, visit_prescs[date <= entry_date & value %in% hypertension_codes, unique(eid)])  # on/before study entry

# Hypertension diagnoses
htn_eids <- union(htn_eids, diagnoses[variable == "hypertension" & date <= entry_date, unique(eid)])  # at study entry
study_data[, htn := eid %in% htn_eids]

# BMI
biomarkers <- merge(biomarkers,
                    study_data[, .(eid, entry_date)],
                    by = "eid")[order(eid, date)]
bmi <- biomarkers[variable == "bmi" & date <= entry_date,
                  .(bmi = tail(value, 1)),
                  by = eid]
study_data <- merge(study_data, bmi, by = "eid", all.x = TRUE)

# Waist circumference
waist <- biomarkers[variable == "waist" & date <= entry_date,
                    .(waist = tail(value, 1)),
                    by = eid]
study_data <- merge(study_data, waist, by = "eid", all.x = TRUE)

# Apply exclusion criteria -----------------------------------------------------

# Drop participants with pre-existing diabetes (self-reported or EHR data)
study_data[, include := !(eid %in% diagnoses[date <= entry_date & variable == "diabetes", eid])]

# Drop participants outside of age criteria at start of study
study_data[, age := time_length(dob %--% entry_date, unit = "years")]
study_data[age < 40 | age > 75, include := FALSE]

# Drop rows with missing data
data_mask <- complete.cases(study_data[, .(age, sex, ethn, fhdm, waist, bmi, htn)])
study_data[, complete := data_mask]

# Leicester score --------------------------------------------------------------

# Drop rows with missing data
scores <- study_data[include & complete]

# Participants with study_length years follow-up (or diagnosis within study_length years)
scores <- scores[(followup >= study_length) | (event == TRUE & time <= study_length)]

# Score elements
scores[, age := cut(age,
                   breaks = c(40, 50, 60, 70, 75), right = FALSE,
                   labels = FALSE)]
scores[, age := mapvalues(age, from = 1:4, to = c(0, 5, 9, 13))]
scores[, bmi := cut(bmi,
                   breaks = c(0, 25, 30, 35, Inf), right = FALSE,
                   labels = FALSE)]
scores[, bmi := mapvalues(bmi, from = 1:4, to = c(0, 3, 5, 8))]
scores[, waist := cut(waist,
                     breaks = c(0, 90, 100, 110, Inf), right = FALSE,
                     labels = FALSE)]
scores[, waist := mapvalues(waist, from = 1:4, to = c(0, 4, 6, 9))]
scores[, sex := as.numeric(mapvalues(as.character(sex), from = c("Female", "Male"), to = 0:1))]
scores[, ethn := as.numeric(mapvalues(ethn, from = c("White", "Other"), to = c(0, 6)))]
scores[, fhdm := 5 * fhdm]
scores[, htn := 5 * htn]

# Calculate score
scores[,
      score := rowSums(.SD),
      .SDcols = c("sex", "age", "ethn", "fhdm", "waist", "bmi", "htn")]
scores[, prediction := score >= 16]

# Evaluate performance ---------------------------------------------------------

confusionMatrix(table(as.numeric(scores$prediction), as.numeric(scores$event)), positive = "1")
