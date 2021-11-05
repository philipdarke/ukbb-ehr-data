# Blood test results
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Extract blood glucose test results.

library(data.table)
library(lubridate)
library(dplyr)
library(ukbbhelpr)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants.rds"))
gp_event <- readRDS(paste0(output_path, "gp_event.rds"))
visit_data <- readRDS(paste0(output_path, "visit_data.rds"))
code_sets <- readRDS(paste0(codeset_path, "primary_care/biomarkers.rds"))
biomarkers <- readRDS(paste0(output_path, "biomarkers.rds"))

# Accepted range of values
min_hba1c <- 20  # mmol/mol
max_hba1c <- 180  # mmol/mol
min_fpg <- 2  # mol/L
max_fpg <- 30  # mol/L
min_ogtt <- 2  # mol/L
max_ogtt <- 30  # mol/L

# Glycated haemoglobin (HbA1c) -------------------------------------------------

# A. Primary care data

# Extract values and drop outliers
hba1c_raw_ehr <- ehr_extract(gp_event, code_sets[value == "hba1c"])
hba1c_ehr <- hba1c_raw_ehr[(value >= min_hba1c & value <= max_hba1c) |
                             (value >= conv_hba1c(min_hba1c, out_unit = "dcct") &
                                value <= conv_hba1c(max_hba1c, out_unit = "dcct"))]

# Convert DCCT to IFCC units
hba1c_ehr[value >= conv_hba1c(min_hba1c, out_unit = "dcct") &
            value <= conv_hba1c(max_hba1c, out_unit = "dcct"),
          value := conv_hba1c(value, out_unit = "ifcc")]

# Take median where multiple values are recorded on same date
hba1c_ehr <- hba1c_ehr[, .(eid, date = event_dt, value)]
hba1c_ehr <- hba1c_ehr[,
                       .(variable = "hba1c",
                         source = "ehr",
                         value = median(value)),
                       by = c("eid", "date")]

# B. UK Biobank assessment centre data

# Extract values and drop outliers
hba1c_raw_ukbb <- visit_extract(visit_data, 30750)
hba1c_ukbb <- hba1c_raw_ukbb[value >= min_hba1c & value <= max_hba1c]
hba1c_ukbb[, c("variable", "source") := .("hba1c", "ukbb")]

# C. Combine EHR and UK Biobank values

# Raw
hba1c_raw_ehr <- hba1c_raw_ehr[,
                               .(eid,
                                 date = event_dt,
                                 source = "ehr",
                                 data_provider,
                                 variable = "hba1c",
                                 value,
                                 unit)]
hba1c_raw_ukbb <- hba1c_raw_ukbb[,
                                 .(eid,
                                   date,
                                   source = "ukbb",
                                   variable = "hba1c",
                                   value,
                                   unit = "mmol/mol")]
hba1c_raw <- rbind(hba1c_raw_ehr, hba1c_raw_ukbb, fill = TRUE)

# Clean
hba1c <- combine_data(hba1c_ukbb, hba1c_ehr)

# Fasting plasma glucose (EHR only) --------------------------------------------

# Extract values and drop outliers
fpg_raw <- ehr_extract(gp_event, code_sets[value == "fpg"])
fpg <- fpg_raw[(value >= min_fpg & value <= max_fpg)]

# Take median where multiple results are recorded on same date
fpg <- fpg[, .(eid, date = event_dt, value)]
fpg <- fpg[,
           .(variable = "fpg", source = "ehr", value = median(value)),
           by = c("eid", "date")]

# Raw data
fpg_raw <- fpg_raw[,
                   .(eid,
                     date = event_dt,
                     source = "ehr",
                     data_provider,
                     variable = "fpg",
                     value,
                     unit)]

# 2-hour oral glucose tolerance tests (EHR only) -------------------------------

# Extract values
ogtt_raw <- ehr_extract(gp_event, code_sets[value == "ogtt" & level == "2hour"])
ogtt <- ogtt_raw[(value >= min_ogtt & value <= max_ogtt)]

# Take median where multiple results are recorded on same date
ogtt <- ogtt[, .(eid, date = event_dt, value)]
ogtt <- ogtt[,
             .(variable = "ogtt", source = "ehr", value = median(value)),
             by = c("eid", "date")]

# Raw data
ogtt_raw <- ogtt_raw[,
                     .(eid,
                       date = event_dt,
                       source = "ehr",
                       data_provider,
                       variable = "ogtt",
                       value,
                       unit)]

# Save results -----------------------------------------------------------------

# Combine data
biomarkers <- rbind(biomarkers, hba1c, fpg, ogtt)[order(eid, date)]

# Save
saveRDS(biomarkers, paste0(output_path, "biomarkers.rds"))

# Apply NICE criteria ----------------------------------------------------------

# Label each test are normoglycaemic/pre-diabetic/diabetic
biomarkers[variable == "hba1c",
           result := case_when(value < nice_hba1c$pre ~ 0L,
                               value < nice_hba1c$diabetes ~ 1L,
                               TRUE ~ 2L)]
biomarkers[variable == "fpg",
           result := case_when(value < nice_fpg$pre ~ 0L,
                               value < nice_fpg$diabetes ~ 1L,
                               TRUE ~ 2L)]
biomarkers[variable == "ogtt",
           result := case_when(value < nice_ogtt$pre ~ 0L,
                               value < nice_ogtt$diabetes ~ 1L,
                               TRUE ~ 2L)]

# Take "worst" result where multiple tests on same day
blood_glucose <- biomarkers[!is.na(result), .(result = max(result)), by = c("eid", "date")]
blood_glucose[, result := factor(result,
                                 levels = 0:2,
                                 labels = c("normo", "pre", "diabetic"))]

# Save
saveRDS(blood_glucose, paste0(output_path, "blood_glucose.rds"))
