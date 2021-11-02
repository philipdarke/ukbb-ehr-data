# Prepare secondary care data
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Extract secondary care diagnoses and procedures from UK Biobank data.

library(data.table)
library(lubridate)
library(ukbbhelpr)
source("setup.R")

# Load raw secondary care data
sc_data <- fread(sc_path)

# Extract diagnoses and procedure fields
sc_data <- visit_fields(sc_data, c(41270:41273, 41280:41283))

# Diagnoses --------------------------------------------------------------------

diag_data <- list(icd9 = melt(sc_data,
                              id.vars = "eid",
                              measure = patterns("^41271-0.", "^41281-0."),
                              value.name = c("code", "date"),
                              na.rm = TRUE),
                  icd10 = melt(sc_data,
                               id.vars = "eid",
                               measure = patterns("^41270-0.", "^41280-0."),
                               value.name = c("code", "date"),
                               na.rm = TRUE))

# Format data
setnames(diag_data$icd9, old = "code", new = "icd9")
setnames(diag_data$icd10, old = "code", new = "icd10")
diag_data <- rbindlist(diag_data, fill = TRUE)
diag_data <- diag_data[, .(eid, date, icd9, icd10)][order(eid, date)]
diag_data <- unique(diag_data)

# Fix date format
diag_data[, date := ymd(date)]

# Procedures -------------------------------------------------------------------

proc_data <- list(opcs3 = melt(sc_data,
                               id.vars = "eid",
                               measure = patterns("^41273-0.", "^41283-0."),
                               value.name = c("code", "date"),
                               na.rm = TRUE),
                  opcs4 = melt(sc_data,
                               id.vars = "eid",
                               measure = patterns("^41272-0.", "^41282-0."),
                               value.name = c("code", "date"),
                               na.rm = TRUE))

# Format data
setnames(proc_data$opcs3, old = "code", new = "opcs3")
setnames(proc_data$opcs4, old = "code", new = "opcs4")
proc_data <- rbindlist(proc_data, fill = TRUE)
proc_data <- proc_data[, .(eid, date, opcs3, opcs4)][order(eid, date)]
proc_data <- unique(proc_data)

# Fix date format
proc_data[, date := ymd(date)]

# Save data --------------------------------------------------------------------

saveRDS(diag_data, paste0(output_path, "hes_diagnoses.rds"))
saveRDS(proc_data, paste0(output_path, "hes_procedures.rds"))
