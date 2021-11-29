# Evaluate QDiabetes - as https://www.bmj.com/content/359/bmj.j5019
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

library(data.table)
library(lubridate)
library(stringr)
library(QDiabetes)
library(survival)
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
phenotype <- readRDS(paste0(output_path, "diabetes_phenotype.rds"))
ehr_presc_codes <- readRDS(paste0(codeset_path, "drugs/prescriptions.rds"))
visit_presc_codes <- get_coding(4)

# Parameters
study_start <- dmy("01/01/2005")

# Study design -----------------------------------------------------------------

# Select participants that had started data collection at study start
study_data <- data_period[study_start %within% (reg_date %--% deduct_date)]

# Select longest registration period of more than one in force at study start
study_data[, followup := time_length(study_start %--% deduct_date, unit = "years")]
study_data <- study_data[order(eid, -followup)]
study_data <- study_data[, head(.SD, 1), by = eid]

# Add participants who subsequently start data collection
late_entrants <- data_period[!(eid %in% study_data$eid) & reg_date > study_start,
                             head(.SD, 1),
                             by = eid]
study_data <- rbind(study_data, late_entrants, fill = TRUE)

# Add study start date, sex, dob and date of 25th birthday
study_data[, study_start := study_start]
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

# Only use outcomes in primary care EHR data
diagnoses <- diagnoses[source == "ehr"]

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
ethn[, ethn := factor(ethn,
                      levels = c("1", "1001", "1002", "1003",
                                 "2", "2001", "2002", "2003", "2004",
                                 "3", "3001", "3002", "3003", "3004",
                                 "4", "4001", "4002", "4003",
                                 "5",
                                 "6" ,
                                 "-1", "-3"),
                      labels = c(rep("WhiteNA", 4),
                                 "Other", "BlackCaribbean", "BlackAfrican", "OtherAsian", "Other",
                                 "OtherAsian", "Indian", "Pakistani", "Bangladeshi", "OtherAsian",
                                 "Other", "BlackCaribbean", "BlackAfrican", "Other",
                                 "Chinese",
                                 "Other",
                                 "WhiteNA", "WhiteNA"))]
study_data <- merge(study_data, ethn, by = "eid", all.x = TRUE)

# Smoking status
smoke <- merge(demographic, study_data[, .(eid, entry_date)], by = "eid")[order(eid, date)]
smoke <- smoke[date <= entry_date & !is.na(smoking),
               tail(.SD, 1),
               by = eid, .SDcols = c("smoking", "smoking_level")]
smoke[smoking == "never", smoke := "Non"]
smoke[smoking == "former", smoke := "Ex"]
smoke[smoking == "non", smoke := "Non"]
smoke[smoking == "current" & smoking_level %in% c("trivial", "light"), smoke := "Light"]
smoke[smoking == "current" & smoking_level %in% "moderate", smoke := "Moderate"]
smoke[smoking == "current" & smoking_level %in% c("heavy", "very_heavy"), smoke := "Heavy"]
smoke[smoking == "current" & is.na(smoking_level), smoke := "Moderate"]  # assume moderate if unknown
study_data <- merge(study_data, smoke[, .(eid, smoke)], by = "eid", all.x = TRUE)

# Family history of diabetes (disclosed at any point in data)
study_data[, fhdm := eid %in% family_history[variable == "diabetes", unique(eid)]]

# CVD
cvd_eids <- diagnoses[variable %in% c("mi", "angina", "stroke", "tia") &
                        date <= entry_date, unique(eid)]
study_data[, cvd := eid %in% cvd_eids]

# Hypertension prescriptions (EHR)
prescriptions_raw <- merge(prescriptions_raw, study_data[, .(eid, entry_date)], by = "eid")
prescriptions <- prescriptions_raw[, time_to_entry := time_length(issue_date %--% entry_date, unit = "days")]
prescriptions <- prescriptions[time_to_entry > 0 & time_to_entry <= 28]  # within 28 days of study entry
htn_eids <- prescriptions[category == "anti_hypertensives", unique(eid)]

# Hypertension prescriptions (self-reported)
hypertension_search <- sapply(ehr_presc_codes$hypertension, function (codes) {
  codes$search
})
hypertension_search <- unique(unlist(hypertension_search))
hypertension_search <- regex(paste(hypertension_search, collapse = "|"), ignore_case = TRUE)
hypertension_codes <- visit_presc_codes[str_detect(meaning, hypertension_search), unique(coding)]
visit_prescs <- visit_extract(visit_data, 20003)
visit_prescs <- merge(visit_prescs, study_data[, .(eid, entry_date)], by = "eid")
htn_eids <- union(htn_eids, visit_prescs[date <= entry_date & value %in% hypertension_codes, unique(eid)])  # at study entry

# Add hypertension treatment indicator
study_data[, htn := eid %in% htn_eids]

# Learning disabilities
learn_eids <- diagnoses[variable == "learning_disabilities" & date <= entry_date, unique(eid)]
study_data[, learn := eid %in% learn_eids]

# Manic depression/schizophrenia
psy_eids <- diagnoses[variable %in% c("bipolar", "schizophrenia") & date <= entry_date, unique(eid)]
study_data[, psy := eid %in% psy_eids]

# Steroid prescriptions (EHR)
ster_eids <- prescriptions[category == "steroids", unique(eid)]

# Steroid prescriptions (self-reported)
steroid_search <- ehr_presc_codes$steroids$all$search
steroid_search <- regex(paste(steroid_search, collapse = "|"), ignore_case = TRUE)
steroid_codes <- visit_presc_codes[str_detect(meaning, steroid_search) &
                                     str_detect(meaning, "eye|ear|cream", negate = TRUE), unique(coding)]
ster_eids <- union(ster_eids, visit_prescs[date <= entry_date & value %in% steroid_codes, unique(eid)])  # at study entry

# Add steroid treatment indicator
study_data[, ster := eid %in% ster_eids]

# Statin prescriptions (EHR)
stat_eids <- prescriptions[category == "statins", unique(eid)]

# Statin prescriptions (self-reported)
statin_search <- c("simvastatin", "atorvastatin", "fluvastatin", "pravastatin", "rosuvastatin")
statin_search <- regex(paste(statin_search, collapse = "|"), ignore_case = TRUE)
statin_codes <- visit_presc_codes[str_detect(meaning, statin_search), unique(coding)]
stat_eids <- union(stat_eids, visit_prescs[date <= entry_date & value %in% statin_codes, unique(eid)])  # at study entry

# Add statin treatment indicator
study_data[, stat := eid %in% stat_eids]

# Atypical antipsychotic prescriptions (EHR)
apsy_eids <- prescriptions[category == "antipsychotics" & type == "atypical", unique(eid)]

# Atypical antipsychotic prescriptions (self-reported)
antipsy_search <- ehr_presc_codes$antipsy$all$search
antipsy_search <- regex(paste(antipsy_search, collapse = "|"), ignore_case = TRUE)
antipsy_codes <- visit_presc_codes[str_detect(meaning, antipsy_search), unique(coding)]
apsy_eids <- union(apsy_eids, visit_prescs[date <= entry_date & value %in% antipsy_codes, unique(eid)])  # at study entry

# Add atypical antipsychotic treatment indicator
study_data[, apsy := eid %in% apsy_eids]

# PCOS
pcos_eids <- diagnoses[variable == "pcos" & date <= entry_date, unique(eid)]
study_data[, pcos := eid %in% pcos_eids]
study_data[sex == "Male", pcos := FALSE]

# Gestational diabetes
gdm_eids <- diagnoses[variable == "diabetes" & level == "gestational" & date <= entry_date, unique(eid)]
study_data[, gdm := eid %in% gdm_eids]
study_data[sex == "Male", gdm := FALSE]

# BMI
biomarkers <- merge(biomarkers,
                    study_data[, .(eid, entry_date)],
                    by = "eid")[order(eid, date)]
bmi <- biomarkers[variable == "bmi" & date <= entry_date,
                  .(bmi = tail(value, 1)),
                  by = eid]
study_data <- merge(study_data, bmi, by = "eid", all.x = TRUE)

# Apply exclusion criteria -----------------------------------------------------

# Drop participants with pre-existing diabetes
study_data[, include := is.na(diagnosis) | diagnosis > entry_date]

# Drop participants without a Townsend score
study_data <- merge(study_data,
                    demographic[!is.na(townsend), .(eid, tds = townsend)],
                    by = "eid", all.x = TRUE)
study_data[is.na(tds), include := FALSE]

# Most recent FPG prior to study entry
fpg <- biomarkers[variable == "fpg" & date <= entry_date,
                  .(fpg = tail(value, 1)),
                  by = eid]
study_data <- merge(study_data, fpg, by = "eid", all.x = TRUE)

# Most recent HbA1c prior to study entry
hba1c <- biomarkers[variable == "hba1c" & date <= entry_date,
                    .(hba1c = tail(value, 1)),
                    by = eid]
study_data <- merge(study_data, hba1c, by = "eid", all.x = TRUE)

# Drop participants with elevated blood sugar
study_data[fpg >= 7, include := FALSE]
study_data[hba1c >= 48, include := FALSE]

# Drop participants outside of age criteria at start of study
study_data[, age := time_length(dob %--% entry_date, unit = "years")]
study_data[age < 25 | age >= 85, include := FALSE]

# Drop rows with missing data
data_mask <- complete.cases(study_data[,
                                       .(sex, tds, age, ethn, smoke, fhdm, cvd,
                                         htn, learn, psy, ster, stat, apsy,
                                         pcos, gdm, bmi)])
study_data[, complete := data_mask]

# QDiabetes score --------------------------------------------------------------

# Drop rows with missing data
study_data_final <- study_data[include & complete]

# Add QDiabetes scores
study_data_final[, score_a := QDR2018A(sex = sex,
                                       age = age,
                                       bmi = bmi,
                                       ethn = ethn,
                                       smoke = smoke,
                                       tds = tds,
                                       fhdm = fhdm,
                                       htn = htn,
                                       cvd = cvd,
                                       gdm = gdm,
                                       pcos = pcos,
                                       learn = learn,
                                       psy = psy,
                                       ster = ster,
                                       stat = stat,
                                       apsy = apsy) / 100]
study_data_final[!is.na(fpg),
                 score_b := QDR2018B(sex = sex,
                                     age = age,
                                     bmi = bmi,
                                     ethn = ethn,
                                     smoke = smoke,
                                     tds = tds,
                                     fhdm = fhdm,
                                     htn = htn,
                                     cvd = cvd,
                                     gdm = gdm,
                                     pcos = pcos,
                                     learn = learn,
                                     psy = psy,
                                     ster = ster,
                                     stat = stat,
                                     apsy = apsy,
                                     fpg = fpg) / 100]
study_data_final[!is.na(hba1c),
                 score_c := QDR2018C(sex = sex,
                                     age = age,
                                     bmi = bmi,
                                     ethn = ethn,
                                     smoke = smoke,
                                     tds = tds,
                                     fhdm = fhdm,
                                     htn = htn,
                                     cvd = cvd,
                                     gdm = gdm,
                                     pcos = pcos,
                                     learn = learn,
                                     psy = psy,
                                     ster = ster,
                                     stat = stat,
                                     apsy = apsy,
                                     hba1c = hba1c) / 100]

# Evaluate performance ---------------------------------------------------------

# For example...to calculate score A for males 
eval_scores(
  study_data_final[!is.na(score_a) & sex == "Male", score_a],
  study_data_final[!is.na(score_a) & sex == "Male", time],
  study_data_final[!is.na(score_a) & sex == "Male", event]
)

# For example...to calculate score C for females
eval_scores(
  study_data_final[!is.na(score_c) & sex == "Female", score_c],
  study_data_final[!is.na(score_c) & sex == "Female", time],
  study_data_final[!is.na(score_c) & sex == "Female", event]
)
