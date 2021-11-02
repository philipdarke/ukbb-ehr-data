# Family history
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Extract family history from UK Biobank data and family history of diabetes
# only from EHR data.

library(data.table)
library(lubridate)
library(ukbbhelpr)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants.rds"))
gp_event <- readRDS(paste0(output_path, "gp_event.rds"))
visit_data <- readRDS(paste0(output_path, "visit_data.rds"))
code_sets <- readRDS(paste0(codeset_path, "primary_care/conditions.rds"))

# Identify conditions ----------------------------------------------------------

# From https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=1010
conditions <- c("heart_disease" = 1,
                "stroke" = 2,
                "cancer_lung" = 3,
                "cancer_bowel" = 4,
                "cancer_breast" = 5,
                "emphysema" = 6,  # chronic bronchitis/emphysema
                "hypertension" = 8,  # high blood pressure
                "diabetes" = 9,
                "dementia" = 10,  # Alzheimer's disease/dementia
                "parkinsons" = 11,  # Parkinson's disease
                "depression" = 12,  # severe depression
                "cancer_prostate" = 13)

# Extract history for all conditions
family_history <- lapply(conditions, function (cond) {
  fh <- visit_family_history(visit_data, c(20107, 20110, 20111), cond, collapse = FALSE)
  fh[, .(eid,
         reported = date,
         variable = names(conditions[conditions == cond]),
         source = "ukbb",
         value = history)]
})
family_history <- rbindlist(family_history)[order(eid, reported)]

# Diabetes (primary care EHR data) ---------------------------------------------

# Codes
diabetes_codes <- code_sets[variable == "diabetes" & value == "family_history"]

# Extract data
diabetes_raw_ehr <- rbind(merge(gp_event, diabetes_codes[, -c("read_3", "N")], by = "read_2"),
                          merge(gp_event, diabetes_codes[, -c("read_2", "N")], by = "read_3"))[order(eid, event_dt)]
diabetes_raw_ehr <- diabetes_raw_ehr[,
                                     .(eid,
                                       reported = event_dt,
                                       variable = "diabetes",
                                       source = "ehr",
                                       data_provider,
                                       value = TRUE)]

# Remove duplicates
diabetes_ehr <- copy(diabetes_raw_ehr)
diabetes_ehr <- diabetes_ehr[, head(.SD, 1), by = "eid"]

# Save results -----------------------------------------------------------------

# Combine data
combined <- rbind(family_history, diabetes_ehr, fill = TRUE)[order(eid, reported)]
combined <- combined[value == TRUE, .(eid, source, data_provider, variable, value, reported)]

# Save data
saveRDS(combined, paste0(output_path, "family_history.rds"))
