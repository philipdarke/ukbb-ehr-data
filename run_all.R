# Run all scripts
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

#' Clear environment before sourcing script
#'
#' @param path Path to .R script
#'
#' @return NULL
#' 
#' @export
#'
source_gc <- function (path) {
  rm(list = ls())
  gc()
  source(path)
}

# 1. Infer periods of EHR data collection across data providers
source_gc("01_prepare_data/01_subset_visit_data.R")
source_gc("01_prepare_data/02_clean_data.R")
source_gc("01_prepare_data/03_reg_history.R")
source_gc("01_prepare_data/04_data_period.R")

# 2. Extract biomarkers etc from EHR and UK Biobank visit data
source_gc("02_extract_records/01_filter_data.R")
source_gc("02_extract_records/02_anthropomorphic.R")
source_gc("02_extract_records/03_blood_tests.R")
source_gc("02_extract_records/04_demographic.R")
source_gc("02_extract_records/05_family_history.R")
source_gc("02_extract_records/06_diagnoses.R")
source_gc("02_extract_records/07_prescriptions.R")

# 3. Diabetes phenotyping case study
source_gc("01_prepare_data/05_secondary_care.R")
source_gc("02_diabetes_phenotyping.R")
