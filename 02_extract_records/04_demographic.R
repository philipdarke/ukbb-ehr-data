# Demographic features
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Extract ethnicity, smoking status and Townsend deprivation.

library(data.table)
library(lubridate)
library(ukbbhelpr)
source("setup.R")

# Load data
participants <- readRDS(paste0(output_path, "participants.rds"))
gp_event <- readRDS(paste0(output_path, "gp_event.rds"))
visit_data <- readRDS(paste0(output_path, "visit_data.rds"))
code_sets <- readRDS(paste0(codeset_path, "primary_care/other.rds"))

# Ethnicity (UK Biobank) -------------------------------------------------------

# Extract data
ethnicity <- visit_extract(visit_data, 21000)

# Update coding (https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=1001)
ethnicity <- ethnicity[value > 0]  # drop unknown

# Format data
ethnicity <- ethnicity[, .(eid, date, source = "ukbb", variable = "ethnicity", value)]

# Smoking status ---------------------------------------------------------------

# A. UK Biobank assessment centre data

# Extract data
smoking_ukbb <- visit_extract(visit_data,
                              c("status" = 20116,
                                "history" = 1249,
                                "n_current" = 3456,
                                "n_former" = 2887))
smoking_ukbb <- dcast(smoking_ukbb, eid + date ~ field)

# Handle missing values
smoking_ukbb[status < 0, status := NA]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=90
smoking_ukbb[history < 0, history := NA]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=100348
smoking_ukbb[n_current %in% c(-1, -3), n_current := NA]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=100355
smoking_ukbb[n_former == -1, n_former := NA]  # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=100353

# Categorise cigarettes/day
smoking_ukbb[!is.na(n_former),
             level := cut(n_former,
                          c(-10, 1, 10, 20, 40, Inf),
                          right = FALSE,
                          labels = c("trivial", "light", "moderate", "heavy", "very_heavy"))]
smoking_ukbb[!is.na(n_current),
             level := cut(n_current,
                          c(-10, 1, 10, 20, 40, Inf),
                          right = FALSE,
                          labels = c("trivial", "light", "moderate", "heavy", "very_heavy"))]

# Identify smoking status
smoking_ukbb[is.na(status) & history == 1, c("category", "sub_category") := .("non_smoker", "former")]
smoking_ukbb[is.na(status) & history == 2, c("category", "sub_category", "level") := .("non_smoker", "former", "trival")]
smoking_ukbb[(is.na(status) | status == 0) & history %in% c(3, 4), c("category", "sub_category") := .("non_smoker", "never")]
smoking_ukbb[status == 1 & history == 2, c("category", "sub_category", "level") := .("non_smoker", "former", "trival")]
smoking_ukbb[status == 1 & history == 1, c("category", "sub_category") := .("non_smoker", "former")]
smoking_ukbb[status == 2, c("category", "sub_category") := .("smoker", "current")]

# Format data
smoking_ukbb <- smoking_ukbb[,
                             .(eid,
                               date,
                               source = "ukbb",
                               data_provider = NA,
                               variable = "smoking",
                               value = sub_category,
                               level)]

# B. Primary care data

# Extract data
smoking_raw_ehr <- rbind(merge(gp_event,
                               code_sets[variable == "smoking", -c("read_3", "N")],
                               by = "read_2"),
                         merge(gp_event,
                               code_sets[variable == "smoking", -c("read_2", "N")],
                               by = "read_3"))[order(eid, event_dt)]

# Include consumption codes where value > 0
smoking_raw_ehr <- smoking_raw_ehr[value != "consumption" |
                                     (value == "consumption") & (value1 > 0 | value2 > 0)]
smoking_raw_ehr[value == "consumption", value := "current"]

# Smoking status codes
smoking_raw_ehr <- smoking_raw_ehr[value %in% c("non", "never", "former", "current"),
                                   .(eid,
                                     date = event_dt,
                                     source = "ehr",
                                     data_provider = data_provider,
                                     variable = "smoking",
                                     value,
                                     level)]

# C. Combine EHR and UK Biobank values and clean history

# Combine EHR and UKBB data
smoking_raw <- rbind(smoking_ukbb, smoking_raw_ehr)[order(eid, date)]

# Can only be a former smoker after first current/former smoking record
smoking <- copy(smoking_raw)
smoking <- merge(smoking,
                 smoking[value == "current", .(current_first = head(date, 1)), by = "eid"],
                 by = "eid", all.x = TRUE)
smoking <- merge(smoking,
                 smoking[value == "former", .(former_first = head(date, 1)), by = "eid"],
                 by = "eid", all.x = TRUE)
smoking[date >= current_first & value == "non", value := "former"]
smoking[date >= former_first & value == "non", value := "former"]

# Cannot have smoked before final "never" record
smoking <- merge(smoking,
                 smoking[value == "never", .(never_last = tail(date, 1)), by = "eid"],
                 by = "eid", all.x = TRUE)
smoking[date <= never_last & value == "non", value := "never"]

# Assume value = current if contradictory status on the same date
flag_current <- smoking[,
                        .(flag_current = any(value == "current") &
                            any(value %in% c("never", "former", "non"))),
                        by = c("eid", "date")]
smoking <- merge(smoking, flag_current, by = c("eid", "date"))
smoking <- smoking[flag_current == FALSE | (flag_current == TRUE & value == "current")]

# Assume value = former if contradictory status (excluding current) on the same date
flag_former <- smoking[,
                       .(flag_former = any(value == "former") &
                           any(value %in% c("never", "non"))),
                       by = c("eid", "date")]
smoking <- merge(smoking, flag_former, by = c("eid", "date"))
smoking <- smoking[flag_former == FALSE | (flag_former == TRUE & value == "former")]

# Assume value = never if also "non" on the same date
flag_never <- smoking[,
                      .(flag_never = any(value == "never") & any(value == "non")),
                      by = c("eid", "date")]
smoking <- merge(smoking, flag_never, by = c("eid", "date"))
smoking <- smoking[flag_never == FALSE | (flag_never == TRUE & value == "never")]

# Drop duplicates
smoking <- smoking[, .(eid, date, source, data_provider, variable, value, level)]
smoking <- unique(smoking)

# Use highest level if multiple levels (e.g. moderate and heavy smoker) on same date
smoking <- smoking[order(eid, date, -level)]
smoking <- smoking[, head(.SD, 1), by = c("eid", "date")]

# D. Remove redundant records

# Identify state changes
smoking[, value_previous := c(NA, value[-.N]), by = "eid"]
smoking[, n := 1:.N, by = eid]
smoking[, state_change := n == 1 | value != value_previous]
smoking[, group := cumsum(state_change), by = "eid"]

# Identify level changes within states
smoking[, level_previous := c(NA, level[-.N]), by = c("eid", "group")]
smoking[, level_change := as.numeric(level) != as.numeric(level_previous) |
            is.na(level) & !is.na(level_previous) |
            !is.na(level) & is.na(level_previous)]
smoking[, state_change := state_change | level_change]

# Drop records that do not add new information
smoking <- smoking[state_change == TRUE]

# Format data
smoking <- smoking[, .(eid, date, source, data_provider, variable, value, level)]

# Townsend deprivation ---------------------------------------------------------

# Extract and format data
depriv <- visit_extract(visit_data, 189)
depriv <- depriv[, .(eid, date, source = "ukbb", variable = "townsend", value)]

# Save results -----------------------------------------------------------------

# Combine data
demographic <- rbind(ethnicity, smoking, depriv, fill = TRUE)[order(eid, date)]
demographic <- dcast(demographic,
                     eid + date + source + data_provider ~ variable,
                     value.var = "value")

# Add smoking value
demographic <- merge(demographic,
                     smoking[, .(eid, date, source, data_provider, smoking_level = level)],
                     by = c("eid", "date", "source", "data_provider"),
                     all = TRUE)

# Convert column types
demographic[, townsend := as.numeric(townsend)]
demographic[, ethnicity := as.numeric(ethnicity)]
demographic[, smoking_level := as.character(smoking_level)]

# Save data
demographic <- demographic[, .(eid, date, source, data_provider, ethnicity, townsend, smoking, smoking_level)]
saveRDS(demographic, paste0(output_path, "demographic.rds"))
