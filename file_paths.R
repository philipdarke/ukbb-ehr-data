# File paths
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

# A. Raw data

# Path to participant data
visit_path_raw <- "UPDATE!"  # .csv or .txt

# Path to registration records (gp_registrations.txt)
reg_path <- "UPDATE!"

# Path to clinical event records (gp_clinical.txt)
events_path <- "UPDATE!"

# Path to prescription records (gp_scripts.txt)
scripts_path <- "UPDATE!"

# Path to summary secondary care data
sc_path <- "data/visit_data.csv"
# sc_path <- visit_path_raw  # uncomment if not using 01_prepare_data/01_subset_visit_data.R
# sc_path <- NULL  # uncomment if secondary data unavailable

# B. Code sets from https://github.com/philipdarke/ehr-codesets

codeset_path <- "UPDATE!"

# C. Output paths

visit_path <- "data/visit_data.csv"
# visit_path <- visit_path_raw  # uncomment if not using 01_prepare_data/01_subset_visit_data.R
output_path <- "data/"
