# Mapping alt JSON

This repository provides R utilities to **map**, **compare**, and **clean** `alt` JSON files by checking consistency between:

- Main country JSON files (dict)
- `alt/` folders in the dict directory
- `alt/` folders (or root) in the results directory

The core function identifies `alt` files that are **not referenced** in the main JSON and can either **list** them or **delete** them safely.

---

## Usage

Source the script in R:

```r
source("altClean.R")

# # 1) Run full mapping and export CSVs
# out <- run_alt_mapping(cfg$dict_dir, cfg$result_dir, export_csv = TRUE)
View(out$dict_wide)
View(out$dict_long)
View(out$result_compare)
# 
# # 2) Single-country report + dry-run delete listing
altClean(630, dict_dir = cfg$dict_dir, result_dir = cfg$result_dir, delete = FALSE)
# 
# # 3) Actually delete (be careful)
altClean(630, dict_dir = cfg$dict_dir, result_dir = cfg$result_dir, delete = TRUE)


