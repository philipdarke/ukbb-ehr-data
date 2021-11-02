# Clean registration history
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Prepare data for the algorithm used to estimate periods of data collection.

library(data.table)
library(lubridate)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants_raw.rds"))
gp_reg_raw <- readRDS(paste0(output_path, "gp_reg_raw.rds"))

# Process registration periods -------------------------------------------------

# Keep longest period where there are co-incident registration periods
gp_reg <- copy(gp_reg_raw)
gp_reg[, length := time_length(reg_date %--% deduct_date, unit = "days")]
gp_reg <- gp_reg[order(eid, data_provider, reg_date, -length)]
gp_reg <- gp_reg[,
                 head(.SD, 1),
                 by = c("eid", "data_provider", "reg_date"),
                 .SDcols = c("deduct_date", "length")]

# Combine overlapping periods of registration
continue <- TRUE
while (continue) {
  initial_rows <- nrow(gp_reg)
  gp_reg[, reg_date_next := c(reg_date[-1], NA), by = c("eid", "data_provider")]
  gp_reg[,
         period := as.numeric(deduct_date < reg_date_next - 1),
         by = c("eid", "data_provider")]
  gp_reg[, period := cumsum(c(1, period[-.N])), by = c("eid", "data_provider")]
  gp_reg <- gp_reg[,
                   .(reg_date = min(reg_date), deduct_date = max(deduct_date)),
                   by = c("eid", "data_provider", "period")]
  continue <- initial_rows != nrow(gp_reg)
}

# Add gaps between registration periods ----------------------------------------

# Records for participants with multiple registration periods
gp_reg_mult <- merge(gp_reg,
                     gp_reg[, .N, by = c("eid", "data_provider")][N > 1],
                     by = c("eid", "data_provider"))

# Gaps between registration periods
reg_gaps <- gp_reg_mult[,
                        add_gaps(.SD),
                        by = c("eid", "data_provider"),
                        .SDcols = c("reg_date", "deduct_date")]

# Add to data
data_period <- rbind(gp_reg[, .(eid,
                                data_provider,
                                reg_date,
                                deduct_date,
                                reg = TRUE)],
                     reg_gaps)

# Add period from birth to start of first registration period ------------------

# Participants with a registration period starting at birth
gp_reg <- merge(gp_reg, participants[, .(eid, dob)], by = "eid")
gp_reg[, birth_reg := reg_date == dob]
gp_reg[, birth_reg := !all(!birth_reg), by = c("eid", "data_provider")]

# Period from birth to first registration
first_period <- gp_reg[birth_reg == FALSE,
                       .(reg_date = head(dob, 1),
                         deduct_date = min(reg_date) - 1,
                         reg = FALSE),
                       by = c("eid", "data_provider")]
data_period <- rbind(data_period, first_period)

# Add period from end of final registration period to censor date --------------

# Participants with a registration period ending on censor date
gp_reg <- merge(gp_reg,
                unique(gp_reg_raw[, .(eid, data_provider, censor_date)]),
                by = c("eid", "data_provider"))
gp_reg[, censor_reg := deduct_date == censor_date]
gp_reg[, censor_reg := !all(!censor_reg), by = c("eid", "data_provider")]

# Period from final registration to censor date
final_period <- gp_reg[censor_reg == FALSE,
                       .(reg_date = tail(deduct_date, 1) + 1,
                         deduct_date = tail(censor_date, 1),
                         reg = FALSE),
                       by = c("eid", "data_provider")]
data_period <- rbind(data_period,
                     final_period)[order(eid, data_provider, reg_date)]

# Save data --------------------------------------------------------------------

gp_reg <- gp_reg[, .(eid, data_provider, reg_date, deduct_date)]
saveRDS(gp_reg, paste0(output_path, "gp_reg.rds"))
saveRDS(data_period, paste0(output_path, "data_period_raw.rds"))
