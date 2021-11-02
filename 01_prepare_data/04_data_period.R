# Estimate period of data collection for each participant
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Apply algorithm to estimate periods of data collection.

library(data.table)
library(lubridate)
library(zoo)
source("setup.R")

# Load data
data_period <- readRDS(paste0(output_path, "data_period_raw.rds"))
all_records <- readRDS(paste0(output_path, "all_records.rds"))

# Steps 1-3: start of data collection ------------------------------------------

all_records_wide <- dcast(unique(all_records),
                          eid + data_provider + date ~ type,
                          value.var = "date")
all_records_wide[,
                 c("event", "test_presc") := .(na.locf(event, na.rm = FALSE),
                                               pmin(presc, test, na.rm = TRUE)),
                 by = c("eid", "data_provider")]
all_records_wide[date > record_start & date == test_presc,
                 event_gap := time_length(event %--% test_presc, unit = "days")]
d_1 <- all_records_wide[event_gap <= 31,
                        .(start = head(date, 1)),
                        by = c("eid", "data_provider")]

# Set up for subsequent steps --------------------------------------------------

# Number periods from start of data collection
data_period <- merge(data_period, d_1, by = c("eid", "data_provider"))
data_period[, period := as.numeric(start %within% (reg_date %--% deduct_date))]
data_period[, period := cumsum(cumsum(period)), by = c("eid", "data_provider")]

# Add date of first/last record within each un-registered period
all_dates <- unique(all_records[, .(eid, data_provider, date)])
first_last <- merge(all_dates,
                    data_period[reg == FALSE & period >= 1,
                                .(eid, data_provider, reg_date, deduct_date, period)],
                    by = c("eid", "data_provider"),
                    allow.cartesian = TRUE)
first_last[, in_period := date %within% (reg_date %--% deduct_date)]
first_last <- first_last[in_period == TRUE,
                         .(first_record = min(date),
                           last_record = max(date)),
                         by = c("eid", "data_provider", "period")]
data_period <- merge(data_period,
                     first_last,
                     by = c("eid", "data_provider", "period"),
                     all = TRUE)

# Un-registered periods with a non-prescription record
non_presc_dates <- unique(all_records[type != "presc",
                                      .(eid, data_provider, date)])
non_presc <- merge(non_presc_dates,
                   data_period[reg == FALSE & period > 1,
                               .(eid, data_provider, reg_date, deduct_date, period)],
                   by = c("eid", "data_provider"))
non_presc[, in_period := date %within% (reg_date %--% deduct_date)]
non_presc <- unique(non_presc[in_period == TRUE,
                              .(eid, data_provider, period, non_presc = TRUE)])
data_period <- merge(data_period,
                     non_presc,
                     by = c("eid", "data_provider", "period"),
                     all = TRUE)

# Steps 4-6: periods of data collection ----------------------------------------

# First period of data collection
data_period[period == 1 & reg == TRUE, c("from", "to") := .(start, deduct_date)]
data_period[period == 1 & reg == FALSE, c("from", "to") := .(start, last_record)]

# Subsequent registered periods
data_period[period > 1 & reg == TRUE, c("from", "to") := .(reg_date, deduct_date)]

# Gaps between registered periods
data_period[period > 1 & reg == FALSE & non_presc == TRUE,
            c("from", "to") := .(first_record, last_record)]

# Final periods
data_period <- data_period[!is.na(from), .(eid, data_provider, from, to)]

# Step 7: join gaps in registration history of less than 1 year ----------------

continue <- TRUE
while (continue) {
  initial_rows <- nrow(data_period)
  data_period[, from_next := c(from[-1], NA), by = c("eid", "data_provider")]
  data_period[,
              period := as.numeric(to < from_next %m-% months(12)),
              by = c("eid", "data_provider")]
  data_period[, period := cumsum(c(1, period[-.N])), by = c("eid", "data_provider")]
  data_period <- data_period[,
                             .(from = min(from), to = max(to)),
                             by = c("eid", "data_provider", "period")]
  continue <- initial_rows != nrow(data_period)
}

# Check at least one record during each period ---------------------------------

record_check <- merge(all_dates,
                      data_period,
                      allow.cartesian = TRUE)
record_check <- unique(record_check[date %within% (from %--% to),
                                    .(eid, data_provider, period)])
data_period <- merge(data_period,
                     record_check,
                     by = c("eid", "data_provider", "period"))

# Save periods of data collection by data provider
saveRDS(data_period, paste0(output_path, "data_period_raw.rds"))

# Combine data providers -------------------------------------------------------

# Keep longest period where there are co-incident registration periods
data_period[, length := time_length(from %--% to, unit = "days")]
data_period <- data_period[order(eid, from, -length)]
data_period <- data_period[,
                           head(.SD, 1),
                           by = c("eid", "from"),
                           .SDcols = c("to", "length")]

# Combine overlapping periods
continue <- TRUE
while (continue) {
  initial_rows <- nrow(data_period)
  data_period[, from_next := c(from[-1], NA), by = eid]
  data_period[,
              period := as.numeric(to < from_next %m-% months(12)),
              by = eid]
  data_period[, period := cumsum(c(1, period[-.N])), by = eid]
  data_period <- data_period[,
                             .(from = min(from), to = max(to)),
                             by = c("eid", "period")]
  continue <- initial_rows != nrow(data_period)
}

# Save periods of data collection across data providers
saveRDS(data_period, paste0(output_path, "data_period.rds"))
