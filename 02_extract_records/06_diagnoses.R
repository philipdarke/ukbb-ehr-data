# Diagnoses
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Extract diagnoses for diabetes, heart attack/MI, angina, stroke, TIA, PCOS,
# bipolar, schizophrenia and learning disabilities.

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
tryCatch({
  sc_data <- readRDS(paste0(output_path, "hes_diagnoses.rds"))
}, error = function (e) {
  # Handle missing HES data
  sc_data <- NULL
})


# Self-reported conditions
sr_data <- visit_conditions(visit_data)

# Diabetes ---------------------------------------------------------------------

# A. UK Biobank assessment centre data

diabetes_ukbb <- sr_data[condition %in% 1220:1223]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
diabetes_ukbb <- diabetes_ukbb[,
                               .(eid,
                                 date,
                                 source = "ukbb",
                                 variable = "diabetes",
                                 level = as.character(condition),
                                 reported)]
diabetes_ukbb[level == "1220", level := NA]
diabetes_ukbb[level == "1221", level := "gestational"]
diabetes_ukbb[level == "1222", level := "type1"]
diabetes_ukbb[level == "1223", level := "type2"]

# B. Primary care data

diabetes_ehr <- rbind(merge(gp_event,
                      pc_codes[variable == "diabetes" & value == "diagnosis", -c("read_3", "N")],
                      by = "read_2"),
                merge(gp_event,
                      pc_codes[variable == "diabetes" & value == "diagnosis", -c("read_2", "N")],
                      by = "read_3"))
diabetes_ehr <- diabetes_ehr[,
                             .(eid,
                               date = event_dt,
                               source = "ehr",
                               data_provider,
                               variable,
                               level,
                               reported = NA_Date_)]

# C. Secondary care data

# Extract codes
if (!is.null(sc_data)) {
  diabetes_hes <- rbind(merge(sc_data, sc_codes[, -c("icd10")], by = "icd9"),
                        merge(sc_data, sc_codes[, -c("icd9")], by = "icd10"))
  diabetes_hes <- diabetes_hes[,
                               .(eid,
                                 date,
                                 source = "hes",
                                 variable = "diabetes",
                                 value,
                                 level,
                                 reported = NA_Date_)]
}


# D. Combine EHR and UK Biobank values

if (!is.null(sc_data)) {
  diabetes <- rbind(diabetes_ukbb, diabetes_ehr, diabetes_hes, fill = TRUE)[order(eid, date)]
} else {
  diabetes <- rbind(diabetes_ukbb, diabetes_ehr, fill = TRUE)[order(eid, date)]
}
diabetes <- diabetes[, .(eid, date, source, data_provider, variable, level, reported)]

# Heart attack/MI --------------------------------------------------------------

# A. UK Biobank assessment centre data

mi_ukbb <- sr_data[condition == 1075]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
mi_ukbb <- mi_ukbb[,
                   .(eid,
                     date,
                     source = "ukbb",
                     variable = "mi",
                     level = NA,
                     reported)]

# B. Primary care data

mi_ehr <- rbind(merge(gp_event,
                      pc_codes[variable == "mi" & value == "diagnosis", -c("read_3", "N")],
                      by = "read_2"),
                merge(gp_event,
                      pc_codes[variable == "mi" & value == "diagnosis", -c("read_2", "N")],
                      by = "read_3"))
mi_ehr <- mi_ehr[,
                 .(eid,
                   date = event_dt,
                   source = "ehr",
                   data_provider,
                   variable,
                   level,
                   reported = NA_Date_)]

# C. Combine EHR and UK Biobank values

mi <- rbind(mi_ukbb, mi_ehr, fill = TRUE)

# Angina -----------------------------------------------------------------------

# A. UK Biobank assessment centre data

angina_ukbb <- sr_data[condition == 1074]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
angina_ukbb <- angina_ukbb[,
                           .(eid,
                             date,
                             source = "ukbb",
                             variable = "angina",
                             level = NA,
                             reported)]

# B. Primary care data

angina_ehr <- rbind(merge(gp_event,
                          pc_codes[variable == "angina" & value == "diagnosis", -c("read_3", "N")],
                          by = "read_2"),
                    merge(gp_event,
                          pc_codes[variable == "angina" & value == "diagnosis", -c("read_2", "N")],
                          by = "read_3"))
angina_ehr <- angina_ehr[,
                         .(eid,
                           date = event_dt,
                           source = "ehr",
                           data_provider,
                           variable,
                           level,
                           reported = NA_Date_)]

# C. Combine EHR and UK Biobank values

angina <- rbind(angina_ukbb, angina_ehr, fill = TRUE)

# Stroke -----------------------------------------------------------------------

# A. UK Biobank assessment centre data

stroke_ukbb <- sr_data[condition %in% c(1081, 1583)]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
stroke_ukbb <- stroke_ukbb[,
                           .(eid,
                             date,
                             source = "ukbb",
                             variable = "stroke",
                             level = NA,
                             reported)]

# B. Primary care data

stroke_ehr <- rbind(merge(gp_event,
                          pc_codes[variable == "stroke" & value == "diagnosis", -c("read_3", "N")],
                          by = "read_2"),
                    merge(gp_event,
                          pc_codes[variable == "stroke" & value == "diagnosis", -c("read_2", "N")],
                          by = "read_3"))
stroke_ehr <- stroke_ehr[,
                         .(eid,
                           date = event_dt,
                           source = "ehr",
                           data_provider,
                           variable,
                           level,
                           reported = NA_Date_)]

# C. Combine EHR and UK Biobank values

stroke <- rbind(stroke_ukbb, stroke_ehr, fill = TRUE)

# TIA --------------------------------------------------------------------------

# A. UK Biobank assessment centre data

tia_ukbb <- sr_data[condition == 1082]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
tia_ukbb <- tia_ukbb[,
                     .(eid,
                       date,
                       source = "ukbb",
                       variable = "tia",
                       level = NA,
                       reported)]

# B. Primary care data

tia_ehr <- rbind(merge(gp_event,
                       pc_codes[variable == "tia" & value == "diagnosis", -c("read_3", "N")],
                       by = "read_2"),
                 merge(gp_event,
                       pc_codes[variable == "tia" & value == "diagnosis", -c("read_2", "N")],
                       by = "read_3"))
tia_ehr <- tia_ehr[,
                   .(eid,
                     date = event_dt,
                     source = "ehr",
                     data_provider,
                     variable,
                     level,
                     reported = NA_Date_)]

# C. Combine EHR and UK Biobank values

tia <- rbind(tia_ukbb, tia_ehr, fill = TRUE)

# Hypertension -----------------------------------------------------------------

# A. UK Biobank assessment centre data

hypert_ukbb <- sr_data[condition %in% c(1065, 1072)]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
hypert_ukbb <- hypert_ukbb[,
                           .(eid,
                             date,
                             source = "ukbb",
                             variable = "hypertension",
                             level = NA,
                             reported)]

# B. Primary care data

hypert_ehr <- rbind(merge(gp_event,
                          pc_codes[variable == "hypertension" & value == "diagnosis", -c("read_3", "N")],
                          by = "read_2"),
                    merge(gp_event,
                          pc_codes[variable == "hypertension" & value == "diagnosis", -c("read_2", "N")],
                          by = "read_3"))
hypert_ehr <- hypert_ehr[,
                         .(eid,
                           date = event_dt,
                           source = "ehr",
                           data_provider,
                           variable,
                           level,
                           reported = NA_Date_)]

# C. Combine EHR and UK Biobank values

hypert <- rbind(hypert_ukbb, hypert_ehr, fill = TRUE)

# PCOS -------------------------------------------------------------------------

# A. UK Biobank assessment centre data

pcos_ukbb <- sr_data[condition == 1350]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
pcos_ukbb <- pcos_ukbb[,
                       .(eid,
                         date,
                         source = "ukbb",
                         variable = "pcos",
                         level = NA,
                         reported)]

# B. Primary care data

pcos_ehr <- rbind(merge(gp_event,
                        pc_codes[variable == "pcos" & value == "diagnosis", -c("read_3", "N")],
                        by = "read_2"),
                  merge(gp_event,
                        pc_codes[variable == "pcos" & value == "diagnosis", -c("read_2", "N")],
                        by = "read_3"))
pcos_ehr <- pcos_ehr[,
                     .(eid,
                       date = event_dt,
                       source = "ehr",
                       data_provider,
                       variable,
                       level,
                       reported = NA_Date_)]

# C. Combine EHR and UK Biobank values

pcos <- rbind(pcos_ukbb, pcos_ehr, fill = TRUE)

# Bipolar ----------------------------------------------------------------------

# A. UK Biobank assessment centre data

bipolar_ukbb <- sr_data[condition == 1291]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
bipolar_ukbb <- bipolar_ukbb[,
                             .(eid,
                               date,
                               source = "ukbb",
                               variable = "bipolar",
                               level = NA,
                               reported)]

# B. Primary care data

bipolar_ehr <- rbind(merge(gp_event,
                           pc_codes[variable == "bipolar" & value == "diagnosis", -c("read_3", "N")],
                           by = "read_2"),
                     merge(gp_event,
                           pc_codes[variable == "bipolar" & value == "diagnosis", -c("read_2", "N")],
                           by = "read_3"))
bipolar_ehr <- bipolar_ehr[,
                           .(eid,
                             date = event_dt,
                             source = "ehr",
                             data_provider,
                             variable,
                             level,
                             reported = NA_Date_)]

# C. Combine EHR and UK Biobank values

bipolar <- rbind(bipolar_ukbb, bipolar_ehr, fill = TRUE)

# Schizophrenia ----------------------------------------------------------------

# A. UK Biobank assessment centre data

schizp_ukbb <- sr_data[condition == 1289]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6
schizp_ukbb <- schizp_ukbb[,
                           .(eid,
                             date,
                             source = "ukbb",
                             variable = "schizophrenia",
                             level = NA,
                             reported)]

# B. Primary care data

schizp_ehr <- rbind(merge(gp_event,
                          pc_codes[variable == "schizophrenia" & value == "diagnosis", -c("read_3", "N")],
                          by = "read_2"),
                    merge(gp_event,
                          pc_codes[variable == "schizophrenia" & value == "diagnosis", -c("read_2", "N")],
                          by = "read_3"))
schizp_ehr <- schizp_ehr[,
                         .(eid,
                           date = event_dt,
                           source = "ehr",
                           data_provider,
                           variable,
                           level,
                           reported = NA_Date_)]

# C. Combine EHR and UK Biobank values

schizp <- rbind(schizp_ukbb, schizp_ehr, fill = TRUE)

# Learning disabilities (EHR only) ---------------------------------------------

learn <- rbind(merge(gp_event,
                     pc_codes[variable == "learning_disabilities" & value == "diagnosis", -c("read_3", "N")],
                     by = "read_2"),
               merge(gp_event,
                     pc_codes[variable == "learning_disabilities" & value == "diagnosis", -c("read_2", "N")],
                     by = "read_3"))
learn <- learn[,
               .(eid,
                 date = event_dt,
                 source = "ehr",
                 data_provider,
                 variable,
                 level,
                 reported = NA_Date_)]

# Save results -----------------------------------------------------------------

# Combine data and drop duplicates
diagnoses <- rbind(diabetes, mi, angina, stroke, tia, hypert, pcos, bipolar, schizp, learn)[order(eid, date)]
diagnoses <- unique(diagnoses)

# Save data
saveRDS(diagnoses, paste0(output_path, "diagnoses.rds"))
