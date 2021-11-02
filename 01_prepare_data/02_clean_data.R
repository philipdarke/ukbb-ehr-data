# Initial data cleaning
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Load raw data and handle data issues including file encoding, missing/
# placeholder dates, incomplete records and harmonisation of prescription drug
# coding.

library(data.table)
library(lubridate)
library(stringr)
library(ukbbhelpr)
source("setup.R")

# Participant data -------------------------------------------------------------

# Load data and extract variables
visit_data <- fread(visit_path)
participants <- visit_fields(visit_data, c(31, 34, 52, 40000))
setnames(participants,
         old = c("31-0.0", "34-0.0", "52-0.0", "40000-0.0"),
         new = c("sex", "yob", "mob", "dod"))

# Date of birth and death
participants[, dob := dmy(paste0("15/", mob, "/", yob))]  # estimated
participants[, dod := ymd(dod)]

# Save data
participants <- participants[, .(eid, sex, dob, dod)]
saveRDS(participants, paste0(output_path, "participants_raw.rds"))

# Load EHR data ----------------------------------------------------------------

# Registration data
gp_reg <- fread(reg_path)
gp_reg[, c("reg_date", "deduct_date") := .(dmy(reg_date), dmy(deduct_date))]

# Clinical event data
gp_event <- fread(events_path)
gp_event[, event_dt := dmy(event_dt)]

# Prescription data
gp_presc <- fread(scripts_path, integer64 = "character")
gp_presc[, issue_date := dmy(issue_date)]

# Registration records ---------------------------------------------------------

# Add data extract, birth and death dates
censor_date <- merge(unique(gp_reg[, .(eid, data_provider)]),
                     extract_date,
                     by = "data_provider")
censor_date <- merge(censor_date, participants[, .(eid, dob, dod)], by = "eid")
gp_reg <- merge(gp_reg, censor_date, by = c("eid", "data_provider"), all = TRUE)

# Censor participants at earlier of death and start of extract range
gp_reg[, censor_date := pmin(extract_start, dod, na.rm = TRUE)]

# End practice registration periods at censor date
gp_reg[is.na(deduct_date) | deduct_date > censor_date,
       deduct_date := censor_date]

# Drop registration periods that do not meet data criteria
gp_reg <- gp_reg[!is.na(reg_date) &  # missing registration dates
                   reg_date <= censor_date &  # starting after the censor date
                   reg_date != na_dates$pre_dob &  # pre-birth registrations
                   reg_date != na_dates$future &  # future registrations
                   deduct_date != na_dates$pre_dob &  # pre-birth deduction dates
                   reg_date < deduct_date]  # registration after deduction

# Update DOB and YOB registration dates...
gp_reg[reg_date == na_dates$dob, reg_date := dob]
gp_reg[reg_date == na_dates$yob, reg_date := dob %m+% months(6)]

# ...and deduction dates
gp_reg[deduct_date == na_dates$dob, deduct_date := dob]
gp_reg[deduct_date == na_dates$yob, deduct_date := dob %m+% months(6)]

# Drop duplicate registration records
gp_reg <- unique(gp_reg)

# Save data
gp_reg <- gp_reg[, .(eid, data_provider, reg_date, deduct_date, censor_date)]
saveRDS(gp_reg, paste0(output_path, "gp_reg_raw.rds"))

# Clinical event records -------------------------------------------------------

# Drop records with missing, pre-birth or future dates
gp_event <- gp_event[!is.na(event_dt) &
                       event_dt != na_dates$pre_dob &
                       event_dt != na_dates$future]

# Update DOB and YOB dates
gp_event <- merge(gp_event, participants[, .(eid, dob)], by = "eid")
gp_event[event_dt == na_dates$dob, event_dt := dob]
gp_event[event_dt == na_dates$yob, event_dt := dob %m+% months(6)]
gp_event[, dob := NULL]

# Handle mu encoding issue in units
gp_event[, value3 := gsub("\xb5", "\xc2\b5", value3)]

# Drop duplicate records and save data
gp_event <- unique(gp_event)
saveRDS(gp_event, paste0(output_path, "gp_event_raw.rds"))

# Prescription records ---------------------------------------------------------

# Drop records with missing, pre-birth or future dates
gp_presc <- gp_presc[!is.na(issue_date) &
                       issue_date != na_dates$pre_dob &
                       issue_date != na_dates$future]

# Update DOB and YOB dates
gp_presc <- merge(gp_presc, participants[, .(eid, dob)], by = "eid")
gp_presc[issue_date == na_dates$dob, issue_date := dob]
gp_presc[issue_date == na_dates$yob, issue_date := dob %m+% months(6)]
gp_presc[, dob := NULL]

# Convert TPP BNF codes
gp_presc[data_provider == 3, bnf_code := str_replace_all(bnf_code, "\\.", "")]

# Set all Read v2 codes to length 5
gp_presc[str_length(read_2) == 7, read_2 := substr(read_2, 1, 5)]

# Add count indicator if multiple prescriptions of same drug on a date
gp_presc <- rbind(gp_presc[drug_name != "", .N, by = names(gp_presc)],
                  gp_presc[drug_name == "", .N, by = c("eid", "data_provider", "issue_date", "read_2")],
                  fill = TRUE)
gp_presc[is.na(bnf_code), bnf_code := ""]
gp_presc[is.na(dmd_code), dmd_code := ""]
gp_presc[is.na(drug_name), drug_name := ""]
gp_presc[is.na(quantity), quantity := ""]

# Save data
saveRDS(gp_presc, paste0(output_path, "gp_presc_raw.rds"))

# Dates of all clinical events, observation/tests and prescription records -----

# Combine dates and add type column
gp_event[, type := "event"]
gp_event[value1 != "" | value2 != "" | value3 != "", type := "test"]
all_records <- rbind(gp_event[, .(eid,
                                  data_provider,
                                  date = event_dt,
                                  type)],
                     gp_presc[, .(eid,
                                  data_provider,
                                  date = issue_date,
                                  type = "presc")])[order(eid, date)]
all_records[, type := as.factor(type)]

# Save data
saveRDS(all_records, paste0(output_path, "all_records.rds"))
