# Visualise diabetes phenotyping results
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>
#
# Visualise results of phenotyping algorithm.

library(data.table)
library(lubridate)
library(plyr)
library(ggplot2)
library(cowplot)
source("setup.R")

# Load data
data_period <- readRDS(paste0(output_path, "data_period.rds"))
phenotype <- readRDS(paste0(output_path, "diabetes_phenotype.rds"))
biomarkers <- readRDS(paste0(output_path, "biomarkers.rds"))
prescriptions <- readRDS(paste0(output_path, "prescriptions.rds"))
records <- readRDS(paste0(output_path, "all_records.rds"))

# Plot random participant in diabetes phenotyping data with 2+ prescription types
eids <- intersect(phenotype[, unique(eid)],
                  prescriptions[, .(n = length(unique(category))), by = eid][n > 2, eid])
id <- sample(eids, 1)
pheno_plot(id)

# Plot participant and save
pheno_plot(id, save = "visualisation/pheno_plot.pdf")
