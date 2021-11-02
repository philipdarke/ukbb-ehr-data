# Evaluate QDiabetes - our algorithm and phenotyping approach
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# See https://www.bmj.com/content/359/bmj.j5019

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
data_period <- readRDS(paste0(output_path, "data_period.rds"))
visit_data <- readRDS(paste0(output_path, "visit_data.rds"))
demographic <- readRDS(paste0(output_path, "demographic.rds"))
diagnoses <- readRDS(paste0(output_path, "diagnoses.rds"))
biomarkers <- readRDS(paste0(output_path, "biomarkers.rds"))
family_history <- readRDS(paste0(output_path, "family_history.rds"))
prescriptions <- readRDS(paste0(output_path, "prescriptions.rds"))
phenotype <- readRDS(paste0(output_path, "diabetes_phenotype.rds"))

# Parameters
study_start <- dmy("01/01/2005")

# Study design -----------------------------------------------------------------

# Select participants that had started data collection at study start
study_data <- data_period[study_start %within% (from %--% to)]

# Add participants who subsequently start data collection
late_entrants <- data_period[!(eid %in% study_data$eid) & from > study_start,
                             head(.SD, 1),
                             by = eid]
study_data <- rbind(study_data, late_entrants)

# Add study start date, sex, dob and date of 25th birthday
study_data[, study_start := study_start]
study_data <- merge(study_data, participants[, .(eid, sex, dob)], by = "eid")
study_data[, age_25 := dob + years(25)]
study_data[, sex := factor(sex, levels = 0:1, labels = c("Female", "Male"))]

# Entry date for each participant
study_data[, entry_date := pmax(study_start, from + years(1), age_25)]
study_data <- study_data[entry_date < to,
                         .(eid, sex, dob, entry_date, censor_date = to)]

# Add follow-up
study_data[, followup := time_length(entry_date %--% censor_date, unit = "years")]

# Outcomes ---------------------------------------------------------------------

# Add outcomes
diabetes_outcomes <- phenotype[state == "type2",
                               .(diagnosis = head(from, 1), event = TRUE),
                               by = eid]
study_data <- merge(study_data, diabetes_outcomes, by = "eid", all.x = TRUE)
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
diagnoses <- merge(diagnoses, study_data[, .(eid, entry_date)], by = "eid")[order(eid, date)]
cvd_eids <- diagnoses[variable %in% c("mi", "angina", "stroke", "tia") &
                        date <= entry_date, unique(eid)]
study_data[, cvd := eid %in% cvd_eids]

# Hypertension
prescriptions <- merge(prescriptions, study_data[, .(eid, entry_date)], by = "eid")
htn_eids <- intersect(prescriptions[category == "anti_hypertensives" & entry_date %within% (from %--% to), unique(eid)],
                      diagnoses[variable == "hypertension" & date <= entry_date, unique(eid)])
study_data[, htn := eid %in% htn_eids]

# Learning disabilities
learn_eids <- diagnoses[variable == "learning_disabilities" & date <= entry_date, unique(eid)]
study_data[, learn := eid %in% learn_eids]

# Manic depression/schizophrenia
psy_eids <- diagnoses[variable %in% c("bipolar", "schizophrenia") & date <= entry_date, unique(eid)]
study_data[, psy := eid %in% psy_eids]

# Steroids
ster_eids <- prescriptions[category == "steroids" & entry_date %within% (from %--% to), unique(eid)]
study_data[, ster := eid %in% ster_eids]

# Statins
stat_eids <- prescriptions[category == "statins" & entry_date %within% (from %--% to), unique(eid)]
study_data[, stat := eid %in% stat_eids]

# Atypical antipsychotics
apsy_eids <- prescriptions[category == "antipsychotics" & entry_date %within% (from %--% to), unique(eid)]
study_data[, apsy := eid %in% apsy_eids]

# PCOS
pcos_eids <- diagnoses[variable == "pcos" & date <= entry_date, unique(eid)]
study_data[, pcos := eid %in% pcos_eids]

# Gestational diabetes
gdm <- merge(phenotype[state == "gestational"], study_data[, .(eid, entry_date)], by = "eid")
gdm_eids <- gdm[from <= entry_date, unique(eid)]
study_data[, gdm := eid %in% gdm_eids]

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
