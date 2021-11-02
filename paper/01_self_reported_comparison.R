# Comparison with self-reported data
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

library(data.table)
library(lubridate)
library(stringr)
library(ukbbhelpr)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants.rds"))
visit_data <- readRDS(paste0(output_path, "visit_data.rds"))
registration <- readRDS(paste0(output_path, "gp_reg_raw.rds"))
data_period <- readRDS(paste0(output_path, "data_period.rds"))
diagnoses <- readRDS(paste0(output_path, "diagnoses.rds"))
prescriptions <- readRDS(paste0(output_path, "prescriptions_raw.rds"))
visit_presc_codes <- fread("coding4.tsv")
ehr_presc_codes <- readRDS(paste0(codeset_path, "primary_care/prescriptions.rds"))

# Select participants registered with GP at first visit
first_visit <- visit_data[, .(eid, first_visit = `53-0.0`)]
registration <- merge(registration, first_visit, by = "eid")
registration[, time_pre_visit := time_length(reg_date %--% first_visit, unit = "years")]
reg_eids <- registration[first_visit %within% (reg_date %--% deduct_date) &
                           time_pre_visit >= 1, unique(eid)]

# Select participants that had started data collection at first visit
data_period <- merge(data_period, first_visit, by = "eid")
data_period[, time_pre_visit := time_length(from %--% first_visit, unit = "years")]
data_eids <- data_period[first_visit %within% (from %--% to) &
                           time_pre_visit >= 1, unique(eid)]

# A. Conditions reported at first visit/in EHR data prior to first visit

diagnoses <- merge(diagnoses, first_visit, by = "eid")
diagnoses <- diagnoses[(source == "ukbb" & reported == first_visit) |
                         (source == "ehr" & date <= first_visit)]

# B. Prescription data

# Prescriptions reported at first visit
visit_prescs <- visit_extract(visit_data, 20003)
visit_prescs <- merge(visit_prescs, first_visit, by = "eid")
visit_prescs <- visit_prescs[date == first_visit]

# Prescriptions in EHR data in 90 days prior to first visit
ehr_prescs <- merge(prescriptions, first_visit, by = "eid")
ehr_prescs <- ehr_prescs[issue_date <= first_visit, ][order(eid, category, type, issue_date)]
ehr_prescs <- ehr_prescs[, tail(.SD, 1), by = c("eid", "category", "type")]
ehr_prescs[, time_pre_visit := time_length(issue_date %--% first_visit, unit = "days")]
ehr_prescs <- ehr_prescs[time_pre_visit <= 90]

# C. De-code UK Biobank self-reported prescriptions

# Anti-hypertensives
hypertension_search <- sapply(ehr_presc_codes$hypertension, function (codes) {
  codes$search
})
hypertension_search <- unique(unlist(hypertension_search))
hypertension_search <- regex(paste(hypertension_search, collapse = "|"), ignore_case = TRUE)
hypertension_codes <- visit_presc_codes[str_detect(meaning, hypertension_search), unique(coding)]

# Statins
statin_search <- c("simvastatin", "atorvastatin", "fluvastatin", "pravastatin", "rosuvastatin")
statin_search <- regex(paste(statin_search, collapse = "|"), ignore_case = TRUE)
statin_codes <- visit_presc_codes[str_detect(meaning, statin_search), unique(coding)]

# Steroids
steroid_search <- ehr_presc_codes$steroids$search
steroid_search <- regex(paste(steroid_search, collapse = "|"), ignore_case = TRUE)
steroid_codes <- visit_presc_codes[str_detect(meaning, steroid_search) &
                                     str_detect(meaning, "eye|ear|cream", negate = TRUE), unique(coding)]

# Atypical anti-psychotics
antipsy_search <- ehr_presc_codes$antipsy$search
antipsy_search <- regex(paste(antipsy_search, collapse = "|"), ignore_case = TRUE)
antipsy_codes <- visit_presc_codes[str_detect(meaning, antipsy_search), unique(coding)]

# D. Helper functions to compare self-reported and EHR data

format_percent <- function (results) {
  format(100 * results, digits = 3, nsmall = 1)
}

compare_condition <- function (eids, condition) {
  out <- data.table(eid = eids)
  out[, sr := eid %in% diagnoses[source == "ukbb" & variable == condition, eid]]
  out[, ehr := eid %in% diagnoses[source == "ehr" & variable == condition, eid]]
  c("sensitivity" = out[ehr == TRUE & sr == TRUE, .N] / out[sr == TRUE, .N],
    "specificity" = out[ehr == FALSE & sr == FALSE, .N] / out[sr == FALSE, .N],
    "precision" = out[ehr == TRUE & sr == TRUE, .N] / out[ehr == TRUE, .N])
}

compare_prescription <- function (eids, prescription, visit_coding) {
  out <- data.table(eid = eids)
  out[, sr := eid %in% visit_prescs[value %in% visit_coding, eid]]
  out[, ehr := eid %in% ehr_prescs[category == prescription, eid]]
  c("sensitivity" = out[ehr == TRUE & sr == TRUE, .N] / out[sr == TRUE, .N],
    "specificity" = out[ehr == FALSE & sr == FALSE, .N] / out[sr == FALSE, .N],
    "precision" = out[ehr == TRUE & sr == TRUE, .N] / out[ehr == TRUE, .N])
}

results_table <- function (eids) {
  out <- data.frame(diabetes = format_percent(compare_condition(eids, "diabetes")),
                    hypertension = format_percent(compare_condition(eids, "hypertension")),
                    mi = format_percent(compare_condition(eids, "mi")),
                    angina = format_percent(compare_condition(eids, "angina")),
                    stroke = format_percent(compare_condition(eids, "stroke")),
                    tia = format_percent(compare_condition(eids, "tia")),
                    bipolar = format_percent(compare_condition(eids, "bipolar")),
                    schizophrenia = format_percent(compare_condition(eids, "schizophrenia")),
                    pcos = format_percent(compare_condition(eids, "pcos")),
                    antihyp = format_percent(compare_prescription(eids, "anti_hypertensives", hypertension_codes)),
                    statins = format_percent(compare_prescription(eids, "statins", statin_codes)),
                    steroids = format_percent(compare_prescription(eids, "steroids", steroid_codes)),
                    antipsy = format_percent(compare_prescription(eids, "antipsychotics", antipsy_codes)))
  t(out)
}

# E. Compare results

# Registration data
results_table(reg_eids)

# Data collection algorithm
results_table(data_eids)
