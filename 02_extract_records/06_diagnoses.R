# Diagnoses
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Extract diagnoses for:
#  - diabetes
#  - heart attack/MI
#  - angina
#  - stroke
#  - TIA
#  - PCOS
#  - bipolar disorder
#  - schizophrenia
#  - learning disabilities

library(data.table)
library(lubridate)
library(ukbbhelpr)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants.rds"))
gp_event <- readRDS(paste0(output_path, "gp_event.rds"))
visit_data <- readRDS(paste0(output_path, "visit_data.rds"))
pc_codes <- readRDS(paste0(codeset_path, "primary_care/conditions.rds"))
sc_codes <- readRDS(paste0(codeset_path, "secondary_care/conditions.rds"))
sc_data <- tryCatch({
  readRDS(paste0(output_path, "hes_diagnoses.rds"))
}, error = function (e) {
  # Handle missing HES data
  NULL
})

# Self-reported conditions
sr_data <- visit_conditions(visit_data)

# Primary care diagnoses -------------------------------------------------------

diagnoses_ehr <- rbind(merge(gp_event, pc_codes[value == "diagnosis", -c("read_3", "N")], by = "read_2"),
                       merge(gp_event, pc_codes[value == "diagnosis", -c("read_2", "N")], by = "read_3"))
diagnoses_ehr <- unique(diagnoses_ehr)
diagnoses_ehr <- diagnoses_ehr[,
                               .(eid,
                                 date = event_dt,
                                 source = "ehr",
                                 data_provider,
                                 variable,
                                 level)]

# HES diagnoses ----------------------------------------------------------------

if (!is.null(sc_data)) {
  diagnoses_hes <- rbind(merge(sc_data, sc_codes[value == "diagnosis", -c("icd10")], by = "icd9"),
                         merge(sc_data, sc_codes[value == "diagnosis", -c("icd9")], by = "icd10"))
  diagnoses_hes <- unique(diagnoses_hes)
  diagnoses_hes <- diagnoses_hes[,
                                 .(eid,
                                   date,
                                   source = "hes",
                                   variable,
                                   level)]
}

# Self-reported ----------------------------------------------------------------

diagnoses_sr <- rbindlist(list(get_condition(sr_data, 1220, "diabetes"),
                               get_condition(sr_data, 1221, "diabetes", "gestational"),
                               get_condition(sr_data, 1222, "diabetes", "type1"),
                               get_condition(sr_data, 1223, "diabetes", "type2"),
                               get_condition(sr_data, 1075, "mi"),
                               get_condition(sr_data, 1074, "angina"),
                               get_condition(sr_data, 1081, "stroke"),
                               get_condition(sr_data, 1583, "stroke", "ischaemic"),
                               get_condition(sr_data, 1082, "tia"),
                               get_condition(sr_data, 1065, "hypertension"),
                               get_condition(sr_data, 1072, "hypertension", "essential"),
                               get_condition(sr_data, 1350, "pcos"),
                               get_condition(sr_data, 1291, "bipolar"),
                               get_condition(sr_data, 1289, "schizophrenia")))

# Save results -----------------------------------------------------------------

# Combine data and drop duplicates
diagnoses <- rbind(diagnoses_ehr, diagnoses_hes, diagnoses_sr,
                   fill = TRUE)[order(eid, date, source, data_provider, variable, level, reported)]
diagnoses <- unique(diagnoses)

# Save data
saveRDS(diagnoses, paste0(output_path, "diagnoses.rds"))

