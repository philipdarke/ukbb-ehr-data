# Anthropomorphic features
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Extract height, weight, BMI and waist circumference.

library(data.table)
library(lubridate)
library(zoo)
library(ukbbhelpr)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants.rds"))
gp_event <- readRDS(paste0(output_path, "gp_event.rds"))
visit_data <- readRDS(paste0(output_path, "visit_data.rds"))
code_sets <- readRDS(paste0(codeset_path, "primary_care/biomarkers.rds"))

# Accepted range of values
min_height <- 1.1  # m
max_height <- 2.1  # m
min_weight <- 25  # kg
max_weight <- 200  # kg
min_bmi <- 10  # kg/m^2
max_bmi <- 80  # kg/m^2
min_waist <- 50  # cm
max_waist <- 175  # cm

# Height -----------------------------------------------------------------------

# A. Primary care data

# Extract values and drop outliers
height_raw_ehr <- ehr_extract(gp_event, code_sets[value == "height"])
height_ehr <- height_raw_ehr[(value >= min_height & value <= max_height) |
                               (value >= 100 * min_height & value <= 100 * max_height)]

# Convert units
height_ehr[value >= 100 * min_height & value <= 100 * max_height,
           value := value / 100]

# Take median where multiple values are recorded on same date
height_ehr <- setnames(height_ehr, "event_dt", "date")
height_ehr <- height_ehr[,
                         .(variable = "height",
                           source = "ehr",
                           value = median(value)),
                         by = c("eid", "date")]

# B. UK Biobank assessment centre data

# Extract values and drop outliers
height_raw_ukbb <- visit_extract(visit_data, 50)
height_ukbb <- copy(height_raw_ukbb)
height_ukbb[, value := value / 100]
height_ukbb <- height_ukbb[value >= min_height & value <= max_height]
height_ukbb[, c("variable", "source") := .("height", "ukbb")]

# C. Combine EHR and UK Biobank values

# Raw
height_raw_ehr <- height_raw_ehr[,
                                 .(eid,
                                   date = event_dt,
                                   source = "ehr",
                                   data_provider,
                                   variable = "height",
                                   value,
                                   unit)]
height_raw_ukbb <- height_raw_ukbb[,
                                   .(eid,
                                     date,
                                     source = "ukbb",
                                     variable = "height",
                                     value,
                                     unit = "cm")]
height_raw <- rbind(height_raw_ehr, height_raw_ukbb, fill = TRUE)

# Clean
height <- combine_data(height_ukbb, height_ehr)

# Weight -----------------------------------------------------------------------

# A. Primary care data

# Extract values and drop outliers
weight_raw_ehr <- ehr_extract(gp_event, code_sets[value == "weight"])
weight_ehr <- weight_raw_ehr[value >= min_weight & value <= max_weight]

# Take median where multiple values are recorded on same date
weight_ehr <- setnames(weight_ehr, "event_dt", "date")
weight_ehr <- weight_ehr[,
                         .(variable = "weight",
                           source = "ehr",
                           value = median(value)),
                         by = c("eid", "date")]

# B. UK Biobank assessment centre data

# Extract values and drop outliers
weight_raw_ukbb <- visit_extract(visit_data, 21002)
weight_ukbb <- weight_raw_ukbb[value >= min_weight & value <= max_weight]
weight_ukbb[, c("variable", "source") := .("weight", "ukbb")]

# C. Combine EHR and UK Biobank values

# Raw
weight_raw_ehr <- weight_raw_ehr[,
                                 .(eid,
                                   date = event_dt,
                                   source = "ehr",
                                   data_provider,
                                   variable = "weight",
                                   value,
                                   unit)]
weight_raw_ukbb <- weight_raw_ukbb[,
                                   .(eid,
                                     date,
                                     source = "ukbb",
                                     variable = "weight",
                                     value,
                                     unit = "kg")]
weight_raw <- rbind(weight_raw_ehr, weight_raw_ukbb, fill = TRUE)

# Clean
weight <- combine_data(weight_ukbb, weight_ehr)

# Body mass index (BMI) --------------------------------------------------------

# A. Primary care data (BMI recorded in value3 under a weight code)

# Extract values
bmi_weight_ehr <- ehr_extract(gp_event, code_sets[value == "weight"])
bmi_weight_ehr <- bmi_weight_ehr[!is.na(as.numeric(unit)),
                                 .(eid,
                                   event_dt,
                                   value = as.numeric(unit))]

# B. Primary care data (BMI codes)

# Extract values, combine with BMIs under weight codes and drop outliers
bmi_raw_ehr <- ehr_extract(gp_event, code_sets[value == "bmi"])
bmi_ehr <- rbind(bmi_weight_ehr, bmi_raw_ehr[, .(eid, event_dt, value)])
bmi_ehr <- bmi_ehr[value >= min_bmi & value <= max_bmi]

# Take median where multiple values are recorded on same date
bmi_ehr <- setnames(bmi_ehr, "event_dt", "date")
bmi_ehr <- bmi_ehr[,
                   .(variable = "bmi",
                     source = "ehr",
                     value = median(value)),
                   by = c("eid", "date")]

# C. UK Biobank assessment centre data

# Calculate BMI where height and weight available at same visit
bmi_ukbb <- merge(height_ukbb[, .(eid, date, height = value)],
                  weight_ukbb[, .(eid, date, weight = value)],
                  by = c("eid", "date"))
bmi_ukbb[, bmi := weight / height ^ 2]

# Drop outliers
bmi_ukbb <- bmi_ukbb[, .(eid, date, value = bmi, variable = "bmi", source = "ukbb")]
bmi_ukbb <- bmi_ukbb[value >= min_bmi & value <= max_bmi]

# D. Combine EHR and UK Biobank values

# Raw
bmi_raw <- bmi_raw_ehr[,
                       .(eid,
                         date = event_dt,
                         source = "ehr",
                         data_provider,
                         variable = "bmi",
                         value,
                         unit)]

# Clean
bmi <- combine_data(bmi_ukbb, bmi_ehr)

# E. Calculate BMI from height at each weight observation (if no BMI available)

# Impute missing heights by linear interpolation between measurements
bmi_final <- rbind(height, weight, bmi)
bmi_final <- dcast(bmi_final,
                   eid + date ~ variable,
                   value.var = "value")[order(eid, date)]
bmi_final[, height := na.approx(zoo(height, date), na.rm = FALSE), by = eid]

# Carry forward/backward remaining missing heights
bmi_final[, height := na.locf(height, na.rm = FALSE), by = eid]
bmi_final[, height := na.locf(height, na.rm = FALSE, fromLast = TRUE), by = eid]

# Impute missing BMI from height and weight
bmi_final[is.na(bmi) & !is.na(height) & !is.na(weight),
          bmi := weight / height ^ 2]

# Finalise BMI data
bmi <- merge(bmi,
             bmi_final[bmi >= min_bmi & bmi <= max_bmi,
                       .(eid, date, new_bmi = bmi)],
             by = c("eid", "date"), all = TRUE)
bmi[is.na(value), c("variable", "source", "value") := .("bmi", "ehr", new_bmi)]
bmi[, new_bmi := NULL]

# F. Calculate weight from height at each BMI observation (if no weight available)

# Impute missing weights from BMI
bmi_final[!is.na(bmi) & !is.na(height) & is.na(weight),
          weight := bmi * height ^ 2]

# Finalise weight data
weight <- merge(weight,
                bmi_final[weight >= min_weight & weight <= max_weight,
                          .(eid, date, new_weight = weight)],
                by = c("eid", "date"), all = TRUE)
weight[is.na(value), c("variable", "source", "value") := .("weight", "ehr", new_weight)]
weight[, new_weight := NULL]

# Waist circumference ----------------------------------------------------------

# A. Primary care data

# Extract values and drop outliers
waist_raw_ehr <- ehr_extract(gp_event, code_sets[value == "waist"])
waist_ehr <- waist_raw_ehr[value >= min_waist / 2.54 & value <= max_waist]

# Convert units
waist_ehr[value < min_waist, value := value * 2.54]

# Take median where multiple values are recorded on same date
waist_ehr <- setnames(waist_ehr, "event_dt", "date")
waist_ehr <- waist_ehr[,
                       .(variable = "waist",
                         source = "ehr",
                         value = median(value)),
                       by = c("eid", "date")]

# B. UK Biobank assessment centre data

# Extract results and drop outliers
waist_raw_ukbb <- visit_extract(visit_data, 48)
waist_ukbb <- waist_raw_ukbb[value >= min_waist & value <= max_waist]
waist_ukbb[, c("variable", "source") := .("waist", "ukbb")]

# C. Combine EHR and UK Biobank values

# Raw
waist_raw_ehr <- waist_raw_ehr[,
                               .(eid,
                                 date = event_dt,
                                 source = "ehr",
                                 data_provider,
                                 variable = "waist",
                                 value,
                                 unit)]
waist_raw_ukbb <- waist_raw_ukbb[,
                                 .(eid,
                                   date,
                                   source = "ukbb",
                                   variable = "waist",
                                   value,
                                   unit = "cm")]
waist_raw <- rbind(waist_raw_ehr, waist_raw_ukbb, fill = TRUE)

# Clean
waist <- combine_data(waist_ukbb, waist_ehr)

# Save results -----------------------------------------------------------------

# Combine data
biomarkers <- rbind(height, weight, bmi, waist)[order(eid, date)]
biomarkers[, c("variable", "source") := .(factor(variable), factor(source))]

# Save
saveRDS(biomarkers, paste0(output_path, "biomarkers.rds"))
