# Filter data
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Drop participants without at least one clinical event record.

library(data.table)
library(lubridate)
source("setup.R")

# Load data
gp_event <- readRDS(paste0(output_path, "gp_event_raw.rds"))
gp_presc <- readRDS(paste0(output_path, "gp_presc_raw.rds"))
participants <- readRDS(paste0(output_path, "participants_raw.rds"))
data_period <- readRDS(paste0(output_path, "data_period.rds"))
visit_data <- fread(visit_path)
hes_diagnoses <- readRDS(paste0(output_path, "hes_diagnoses.rds"))
hes_procedures <- readRDS(paste0(output_path, "hes_procedures.rds"))

# Participants with EHR data
eids <- unique(gp_event$eid)

# Filter data
gp_presc <- gp_presc[eid %in% eids]
participants <- participants[eid %in% eids]
data_period <- data_period[eid %in% eids]
visit_data <- visit_data[eid %in% eids]
hes_diagnoses <- hes_diagnoses[eid %in% eids]
hes_procedures <- hes_procedures[eid %in% eids]

# Save data
saveRDS(gp_event, paste0(output_path, "gp_event.rds"))
saveRDS(gp_presc, paste0(output_path, "gp_presc.rds"))
saveRDS(participants, paste0(output_path, "participants.rds"))
saveRDS(data_period, paste0(output_path, "data_period.rds"))
saveRDS(visit_data, paste0(output_path, "visit_data.rds"))
saveRDS(hes_diagnoses, paste0(output_path, "hes_diagnoses.rds"))
saveRDS(hes_procedures, paste0(output_path, "hes_procedures.rds"))
