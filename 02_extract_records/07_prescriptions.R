# Extract prescription records
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

library(data.table)
library(lubridate)
library(stringr)
library(ukbbhelpr)
source("setup.R")

# Load data
gp_presc <- readRDS(paste0(output_path, "gp_presc.rds"))
presc_codes <- readRDS(paste0(codeset_path, "primary_care/prescriptions.rds"))

# Diabetes ---------------------------------------------------------------------

diabetes <- lapply(1:length(presc_codes$diabetes), function (i) {
  drugs <- presc_codes$diabetes[[i]]
  drug_type <- names(presc_codes$diabetes)[i]
  out <- gp_presc[substr(read_2, 1, 2) %in% drugs$read[str_length(drugs$read) == 2] |
                    substr(read_2, 1, 3) %in% drugs$read[str_length(drugs$read) == 3] |
                    substr(read_2, 1, 4) %in% drugs$read[str_length(drugs$read) == 4] |
                    substr(read_2, 1, 5) %in% drugs$read[str_length(drugs$read) == 5] |
                    substr(bnf_code, 1, 4) %in% drugs$bnf[str_length(drugs$bnf) == 4] |
                    substr(bnf_code, 1, 6) %in% drugs$bnf[str_length(drugs$bnf) == 6] |
                    substr(bnf_code, 1, 7) %in% drugs$bnf[str_length(drugs$bnf) == 7] |
                    substr(bnf_code, 1, 9) %in% drugs$bnf[str_length(drugs$bnf) == 9] |
                    substr(bnf_code, 1, 10) %in% drugs$bnf[str_length(drugs$bnf) == 10]]
  out[, c("category", "type") := .("diabetes", drug_type)]
})
diabetes <- rbindlist(diabetes)

# Steroids ---------------------------------------------------------------------

steroids <- gp_presc[substr(read_2, 1, 2) %in% presc_codes$steroids$read[str_length(presc_codes$steroids$read) == 2] |
                       substr(read_2, 1, 3) %in% presc_codes$steroids$read[str_length(presc_codes$steroids$read) == 3] |
                       substr(read_2, 1, 4) %in% presc_codes$steroids$read[str_length(presc_codes$steroids$read) == 4] |
                       substr(read_2, 1, 5) %in% presc_codes$steroids$read[str_length(presc_codes$steroids$read) == 5] |
                       substr(bnf_code, 1, 4) %in% presc_codes$steroids$bnf[str_length(presc_codes$steroids$bnf) == 4] |
                       substr(bnf_code, 1, 6) %in% presc_codes$steroids$bnf[str_length(presc_codes$steroids$bnf) == 6] |
                       substr(bnf_code, 1, 7) %in% presc_codes$steroids$bnf[str_length(presc_codes$steroids$bnf) == 7] |
                       substr(bnf_code, 1, 9) %in% presc_codes$steroids$bnf[str_length(presc_codes$steroids$bnf) == 9] |
                       substr(bnf_code, 1, 10) %in% presc_codes$steroids$bnf[str_length(presc_codes$steroids$bnf) == 10]]
steroids[, c("category", "type") := .("steroids", NA)]

# Statins ----------------------------------------------------------------------

statins <- gp_presc[substr(read_2, 1, 2) %in% presc_codes$statins$read[str_length(presc_codes$statins$read) == 2] |
                      substr(read_2, 1, 3) %in% presc_codes$statins$read[str_length(presc_codes$statins$read) == 3] |
                      substr(read_2, 1, 4) %in% presc_codes$statins$read[str_length(presc_codes$statins$read) == 4] |
                      substr(read_2, 1, 5) %in% presc_codes$statins$read[str_length(presc_codes$statins$read) == 5] |
                      substr(bnf_code, 1, 4) %in% presc_codes$statins$bnf[str_length(presc_codes$statins$bnf) == 4] |
                      substr(bnf_code, 1, 6) %in% presc_codes$statins$bnf[str_length(presc_codes$statins$bnf) == 6] |
                      substr(bnf_code, 1, 7) %in% presc_codes$statins$bnf[str_length(presc_codes$statins$bnf) == 7] |
                      substr(bnf_code, 1, 9) %in% presc_codes$statins$bnf[str_length(presc_codes$statins$bnf) == 9] |
                      substr(bnf_code, 1, 10) %in% presc_codes$statins$bnf[str_length(presc_codes$statins$bnf) == 10]]
statins[, c("category", "type") := .("statins", NA)]

# Atypical antipsychotics ------------------------------------------------------

# Records with matching codes
antipsy <- gp_presc[substr(read_2, 1, 2) %in% presc_codes$antipsy$read[str_length(presc_codes$antipsy$read) == 2] |
                      substr(read_2, 1, 3) %in% presc_codes$antipsy$read[str_length(presc_codes$antipsy$read) == 3] |
                      substr(read_2, 1, 4) %in% presc_codes$antipsy$read[str_length(presc_codes$antipsy$read) == 4] |
                      substr(read_2, 1, 5) %in% presc_codes$antipsy$read[str_length(presc_codes$antipsy$read) == 5] |
                      substr(bnf_code, 1, 4) %in% presc_codes$antipsy$bnf[str_length(presc_codes$antipsy$bnf) == 4] |
                      substr(bnf_code, 1, 6) %in% presc_codes$antipsy$bnf[str_length(presc_codes$antipsy$bnf) == 6] |
                      substr(bnf_code, 1, 7) %in% presc_codes$antipsy$bnf[str_length(presc_codes$antipsy$bnf) == 7] |
                      substr(bnf_code, 1, 9) %in% presc_codes$antipsy$bnf[str_length(presc_codes$antipsy$bnf) == 9] |
                      substr(bnf_code, 1, 10) %in% presc_codes$antipsy$bnf[str_length(presc_codes$antipsy$bnf) == 10]]

# Search drug names for TPP records
antipsy_addl <- gp_presc[data_provider == 3 & substr(bnf_code, 1, 4) == "0402"]
antipsy_addl <- antipsy_addl[str_detect(drug_name, regex(paste0("\\b", presc_codes$antipsy$search, "\\b", collapse = "|"), ignore_case = TRUE))]

# Finalise records
antipsy_final <- rbind(antipsy, antipsy_addl)
antipsy_final[, c("category", "type") := .("antipsychotics", "atypical")]

# Anti-hypertensives -----------------------------------------------------------

hypertension <- lapply(1:length(presc_codes$hypertension), function (i) {
  drugs <- presc_codes$hypertension[[i]]
  drug_type <- names(presc_codes$hypertension)[i]
  out <- gp_presc[substr(read_2, 1, 2) %in% drugs$read[str_length(drugs$read) == 2] |
                    substr(read_2, 1, 3) %in% drugs$read[str_length(drugs$read) == 3] |
                    substr(read_2, 1, 4) %in% drugs$read[str_length(drugs$read) == 4] |
                    substr(read_2, 1, 5) %in% drugs$read[str_length(drugs$read) == 5] |
                    substr(bnf_code, 1, 4) %in% drugs$bnf[str_length(drugs$bnf) == 4] |
                    substr(bnf_code, 1, 6) %in% drugs$bnf[str_length(drugs$bnf) == 6] |
                    substr(bnf_code, 1, 7) %in% drugs$bnf[str_length(drugs$bnf) == 7] |
                    substr(bnf_code, 1, 9) %in% drugs$bnf[str_length(drugs$bnf) == 9] |
                    substr(bnf_code, 1, 10) %in% drugs$bnf[str_length(drugs$bnf) == 10]]
  out[, c("category", "type") := .("anti_hypertensives", drug_type)]
})
hypertension <- rbindlist(hypertension)

# Save records -----------------------------------------------------------------

all_records <- rbind(diabetes, steroids, statins, antipsy_final, hypertension)
all_records <- all_records[order(category, type, eid, data_provider, issue_date)]
saveRDS(all_records, paste0(output_path, "prescriptions_raw.rds"))

# Periods of medication --------------------------------------------------------

# Periods by category
presc_periods <- lapply(unique(all_records$category), function (cat) {
  presc_history(all_records[category == cat], prescription_duration, category = cat)
})
presc_periods <- rbindlist(presc_periods)
presc_periods[, type := "any"]

# Periods by type of diabetes drug
diabetes_periods <- lapply(unique(all_records[category == "diabetes", unique(type)]), function (typ) {
  presc_history(all_records[category == "diabetes" & type == typ], prescription_duration, category = typ)
})
diabetes_periods <- rbindlist(diabetes_periods)
diabetes_periods[, type := category]
diabetes_periods[, category := "diabetes"]

# Combine periods and save
presc_periods <- rbind(presc_periods, diabetes_periods)
saveRDS(presc_periods, paste0(output_path, "prescriptions.rds"))
