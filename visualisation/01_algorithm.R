# Visualise data collection algorithm results
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Visualise algorithm used to determine periods of EHR data collection.

library(data.table)
library(lubridate)
library(ggplot2)
library(cowplot)
source("setup.R")

# Load data
reg_data <- readRDS(paste0(output_path, "gp_reg_raw.rds"))
data_period <- readRDS(paste0(output_path, "data_period.rds"))
records <- readRDS(paste0(output_path, "all_records.rds"))

# Plot random participant with data from multiple providers
eids <- intersect(reg_data[, .(n = length(unique(data_provider))), by = eid][n > 1, eid],
                  data_period[period > 1, eid])
id <- sample(eids, 1)
algo_plot(id)

# Plot participant and save
algo_plot(id, save = "visualisation/algo_plot.pdf")
