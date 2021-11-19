# Filter data
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Drop participants without at least one clinical event record.

library(data.table)
library(lubridate)
source("setup.R")

# Participants with EHR data
gp_event <- readRDS(paste0(output_path, "gp_event_raw.rds"))
eids <- unique(gp_event$eid)

# Load data
gp_presc <- readRDS(paste0(output_path, "gp_presc_raw.rds"))
participants <- readRDS(paste0(output_path, "participants_raw.rds"))
visit_data <- fread(visit_path)

# Filter data
gp_presc <- gp_presc[eid %in% eids]
participants <- participants[eid %in% eids]
visit_data <- visit_data[eid %in% eids]

# Save data
saveRDS(gp_event, paste0(output_path, "gp_event.rds"))
saveRDS(gp_presc, paste0(output_path, "gp_presc.rds"))
saveRDS(participants, paste0(output_path, "participants.rds"))
saveRDS(visit_data, paste0(output_path, "visit_data.rds"))

# Hospital episode statistics
tryCatch({
  hes_diagnoses <- readRDS(paste0(output_path, "hes_diagnoses.rds"))
  hes_procedures <- readRDS(paste0(output_path, "hes_procedures.rds"))
  hes_diagnoses <- hes_diagnoses[eid %in% eids]
  hes_procedures <- hes_procedures[eid %in% eids]
  saveRDS(hes_diagnoses, paste0(output_path, "hes_diagnoses.rds"))
  saveRDS(hes_procedures, paste0(output_path, "hes_procedures.rds"))
}, error = function (e) {
  message("Hospital episode statistics unavailable.")
})

