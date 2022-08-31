'''
Subset UK Biobank visit data
Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

Python equivalent of 01_subset_visit_data.R.

Extracts fields from raw visit data and saves them at `visit_path`. This file
will likely be smaller than the full data for your UK Biobank application.
This helps with memory requirements.
'''

import re
import pandas as pd

CHUNK_SIZE = 10000

# File paths
visit_path_raw = "/home/campus.ncl.ac.uk/b8046419/datasets/ukbb/62594/ukb48488.csv"
visit_path = "data/visit_data.csv"

# Columns to select
required_fields = [
  31, 34, 52, 53, 189, 21000, 40000,  # demographics
  48, 50, 21002,  # anthropomorphic
  30750,  # blood test results
  2986, 20002, 20003, 20008,  # non-cancer medical history
  20107, 20110, 20111,  # family history
  1249, 2887, 3456, 20116,  # smoking history
  41270, 41271, 41272, 41273, 41280, 41281, 41282, 41283  # secondary care data
]
required_fields = [str(field) for field in required_fields]
pattern = re.compile(r"^eid$|^" + "-\d\.\d+$|^".join(required_fields) + "-\d\.\d+$")
all_cols = pd.read_csv(visit_path_raw, nrows=1).columns.tolist()
keep_cols = list(filter(pattern.search, all_cols))

print("{} selected columns: {}".format(len(keep_cols), keep_cols))

# Write columns to file
for i, chunk in enumerate(pd.read_csv(visit_path_raw, usecols=keep_cols, dtype=str, chunksize=CHUNK_SIZE)):
    chunk.to_csv(visit_path, header=i==0, index=False, mode="a")
    print("Rows {}-{} written to {}".format(i * CHUNK_SIZE, (i + 1) * CHUNK_SIZE - 1, visit_path))
print("Complete!")
