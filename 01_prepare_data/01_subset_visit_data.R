# Subset UK Biobank visit data
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Extracts fields from raw visit data and saves them at `visit_path`. This file
# will likely be smaller than the full data for your UK Biobank application.
# This helps with memory requirements.

library(data.table)
library(lubridate)
library(ukbbhelpr)
source("setup.R")

# Fields required to prepare data
required_fields <- c(31, 52, 53, 40000)

# All fields including optional fields (COMMENT OUT IF FIELDS ARE UNAVAILABLE)
required_fields <- c(
  31, 34, 52, 53, 189, 21000, 40000,  # demographics
  48, 50, 21002,  # anthropomorphic
  30750,  # blood test results
  2986, 20002, 20003, 20008,  # non-cancer medical history
  20107, 20110, 20111,  # family history
  1249, 2887, 3456, 20116,  # smoking history
  41270, 41271, 41272, 41273, 41280, 41281, 41282, 41283  # secondary care data
)

# Extract and save
visit_subset(visit_path_raw, required_fields, save = visit_path)
