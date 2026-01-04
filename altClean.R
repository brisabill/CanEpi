# ============================================================
# Alt mapping + comparison + cleanup (refactored)
# ============================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(stringr)
  library(purrr)
  library(data.table)
})

# -------------------------
# CONFIG (set once)
# -------------------------
cfg <- list(
  dict_dir   = normalizePath("C:/Users/mirandaa/Desktop/estican/estican/dict",   winslash = "/", mustWork = FALSE),
  result_dir = normalizePath("C:/Users/mirandaa/Desktop/estican/estican/result", winslash = "/", mustWork = FALSE)
)

# -------------------------
# Helpers: paths + IO
# -------------------------
get_region_dirs <- function(root_dir) {
  if (!dir.exists(root_dir)) return(character(0))
  dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
  dirs[str_detect(basename(dirs), "^\\d+$")]
}

safe_fromJSON <- function(path) {
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

as_cc <- function(x) as.character(x)

# Normalize alt strings (from JSON or filenames)
norm_alt <- function(x) {
  x <- unlist(x, recursive = TRUE, use.names = FALSE)
  x <- as.character(x)
  x <- str_trim(x)
  x <- x[!is.na(x) & x != ""]
  unique(x)
}

# -------------------------
# Extract alts from main JSON (recursive)
# -------------------------
extract_alts_from_main <- function(j) {
  if (is.null(j) || !is.list(j)) return(character(0))
  out <- character(0)
  
  walk_rec <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    nms <- names(x)
    
    if (!is.null(nms) && "alt" %in% nms) {
      out <<- c(out, norm_alt(x[["alt"]]))
    }
    purrr::walk(x, walk_rec)
  }
  
  walk_rec(j)
  unique(out)
}

# -------------------------
# List alt files in dict/alt for a country
# Accepts either: cc.json or cc_*.json
# Returns stems without country prefix and without .json
# -------------------------
list_alt_files_for_country <- function(alt_dir, country_code) {
  if (!dir.exists(alt_dir)) return(character(0))
  
  cc <- as_cc(country_code)
  files <- list.files(alt_dir, pattern = "\\.json$", full.names = FALSE)
  
  # keep only for this country code
  files <- files[startsWith(files, paste0(cc, "_")) | files == paste0(cc, ".json")]
  stems <- str_remove(files, "\\.json$")
  stems <- str_remove(stems, paste0("^", cc, "_"))
  stems <- stems[!is.na(stems) & stems != ""]
  unique(stems)
}

# -------------------------
# Dict scan: wide + long tables
# -------------------------
scan_dict <- function(dict_dir) {
  region_dirs <- get_region_dirs(dict_dir)
  
  wide <- rbindlist(lapply(region_dirs, function(rdir) {
    region  <- basename(rdir)
    alt_dir <- file.path(rdir, "alt")
    
    main_jsons <- list.files(rdir, pattern = "^\\d+\\.json$", full.names = TRUE)
    
    rbindlist(lapply(main_jsons, function(f) {
      cc <- str_remove(basename(f), "\\.json$")
      j  <- safe_fromJSON(f)
      
      country_label <- if (!is.null(j) && !is.null(j$country_label)) j$country_label else NA_character_
      
      alts_main     <- sort(extract_alts_from_main(j))
      alts_folder   <- sort(list_alt_files_for_country(alt_dir, cc))
      only_in_main  <- sort(setdiff(alts_main, alts_folder))
      only_in_fold  <- sort(setdiff(alts_folder, alts_main))
      
      data.table(
        region_folder = region,
        country_code  = suppressWarnings(as.integer(cc)),
        country_label = country_label,
        main_json_path = f,
        alt_dir = alt_dir,
        
        alts_in_main       = paste(alts_main, collapse = ", "),
        alt_files          = paste(alts_folder, collapse = ", "),
        only_in_main       = paste(only_in_main, collapse = ", "),
        only_in_alt_folder = paste(only_in_fold, collapse = ", "),
        
        n_alts_in_main       = length(alts_main),
        n_alt_files          = length(alts_folder),
        n_only_in_main       = length(only_in_main),
        n_only_in_alt_folder = length(only_in_fold)
      )
    }), fill = TRUE)
  }), fill = TRUE)
  
  long <- rbindlist(lapply(region_dirs, function(rdir) {
    region  <- basename(rdir)
    alt_dir <- file.path(rdir, "alt")
    
    main_jsons <- list.files(rdir, pattern = "^\\d+\\.json$", full.names = TRUE)
    
    rbindlist(lapply(main_jsons, function(f) {
      cc <- str_remove(basename(f), "\\.json$")
      j  <- safe_fromJSON(f)
      
      country_label <- if (!is.null(j) && !is.null(j$country_label)) j$country_label else NA_character_
      
      alts_main     <- extract_alts_from_main(j)
      alts_folder   <- list_alt_files_for_country(alt_dir, cc)
      all_alts      <- sort(unique(c(alts_main, alts_folder)))
      if (!length(all_alts)) return(NULL)
      
      dt <- data.table(
        region_folder = region,
        country_code  = suppressWarnings(as.integer(cc)),
        country_label = country_label,
        alt = all_alts,
        in_main   = all_alts %in% alts_main,
        in_folder = all_alts %in% alts_folder
      )
      
      dt[, status := fifelse(in_main & in_folder, "both",
                             fifelse(in_main & !in_folder, "only_in_main",
                                     fifelse(!in_main & in_folder, "only_in_alt_folder", "neither")))]
      dt
    }), fill = TRUE)
  }), fill = TRUE)
  
  list(region_dirs = region_dirs, wide = wide, long = long)
}

# -------------------------
# Result parsing: extract alt + type from filenames
# Expected pattern: cc_<alt>_[i|m]_...json
# -------------------------
parse_result_files <- function(files, cc) {
  if (!length(files)) return(data.table())
  
  file_noext <- str_remove(files, "\\.json$")
  
  type <- fifelse(str_detect(file_noext, "_i_"), "i",
                  fifelse(str_detect(file_noext, "_m_"), "m", NA_character_))
  keep <- !is.na(type)
  if (!any(keep)) return(data.table())
  
  files <- files[keep]
  file_noext <- file_noext[keep]
  type <- type[keep]
  
  after_cc <- str_remove(file_noext, paste0("^", cc, "_"))
  alt <- str_replace(after_cc, "(_[im]_.*)$", "")
  alt <- norm_alt(alt)
  
  alt_per_file <- str_replace(after_cc, "(_[im]_.*)$", "")
  alt_per_file <- str_trim(alt_per_file)
  
  data.table(file = files, alt = alt_per_file, type = type)[!is.na(alt) & alt != ""]
}

extract_result_alts_for_country <- function(region_dir_result, country_code) {
  cc <- as_cc(country_code)
  
  alt_dir <- file.path(region_dir_result, "alt")
  scan_dir <- if (dir.exists(alt_dir)) alt_dir else region_dir_result
  if (!dir.exists(scan_dir)) return(data.table())
  
  files <- list.files(scan_dir, pattern = "\\.json$", full.names = FALSE)
  files <- files[startsWith(files, paste0(cc, "_"))]
  if (!length(files)) return(data.table())
  
  dt <- parse_result_files(files, cc)
  unique(dt[, .(alt, type)])
}

scan_result <- function(result_dir, dict_region_dirs) {
  rbindlist(lapply(dict_region_dirs, function(rdir_dict) {
    region <- basename(rdir_dict)
    
    # get country codes from dict main jsons so result scan aligns
    main_jsons <- list.files(rdir_dict, pattern = "^\\d+\\.json$", full.names = TRUE)
    
    rdir_result <- file.path(result_dir, region)
    if (!dir.exists(rdir_result)) return(NULL)
    
    rbindlist(lapply(main_jsons, function(f) {
      cc <- str_remove(basename(f), "\\.json$")
      dt <- extract_result_alts_for_country(rdir_result, cc)
      if (!nrow(dt)) return(NULL)
      
      data.table(
        region_folder = region,
        country_code  = suppressWarnings(as.integer(cc)),
        alt  = dt$alt,
        type = dt$type
      )
    }), fill = TRUE)
  }), fill = TRUE)
}

# -------------------------
# Compare result alts vs dict main-json alts
# -------------------------
compare_result_to_dict_main <- function(res_result_long, dict_long, dict_wide) {
  dict_main <- unique(dict_long[in_main == TRUE, .(region_folder, country_code, alt)])
  dict_main[, in_dict_main := TRUE]
  setkey(dict_main, region_folder, country_code, alt)
  
  setkey(res_result_long, region_folder, country_code, alt)
  cmp <- merge(res_result_long, dict_main, by = c("region_folder", "country_code", "alt"), all.x = TRUE)
  
  cmp[, status := fifelse(!is.na(in_dict_main), "present_in_dict_main_json", "only_in_result")]
  cmp[, in_dict_main := NULL]
  
  # bring labels
  labels <- unique(dict_wide[, .(region_folder, country_code, country_label)])
  setkey(labels, region_folder, country_code)
  setkey(cmp, region_folder, country_code)
  labels[cmp]
}

# -------------------------
# Single-country report (replacement for jsonclean)
# -------------------------
report_country <- function(country_code, dict_dir) {
  cc <- as_cc(country_code)
  region_dirs <- get_region_dirs(dict_dir)
  
  main_path <- NULL
  region_hit <- NULL
  
  for (rdir in region_dirs) {
    candidate <- file.path(rdir, paste0(cc, ".json"))
    if (file.exists(candidate)) {
      main_path <- candidate
      region_hit <- basename(rdir)
      break
    }
  }
  if (is.null(main_path)) stop("Could not find main JSON for country ", cc)
  
  alt_dir <- file.path(dirname(main_path), "alt")
  j <- safe_fromJSON(main_path)
  country_label <- if (!is.null(j) && !is.null(j$country_label)) j$country_label else NA_character_
  
  alts_main   <- sort(extract_alts_from_main(j))
  alts_folder <- sort(list_alt_files_for_country(alt_dir, cc))
  
  list(
    country_code    = suppressWarnings(as.integer(cc)),
    country_label   = country_label,
    region_folder   = region_hit,
    main_json_path  = main_path,
    alt_dir         = alt_dir,
    alts_main       = alts_main,
    alts_folder     = alts_folder,
    only_in_main    = sort(setdiff(alts_main, alts_folder)),
    only_in_folder  = sort(setdiff(alts_folder, alts_main))
  )
}

print_country_report <- function(info) {
  cc <- as_cc(info$country_code)
  
  cat("\n============================\n")
  cat("Country:", ifelse(is.na(info$country_label), cc, paste0(info$country_label, " (", cc, ")")), "\n")
  cat("Region folder:", info$region_folder, "\n")
  cat("Main JSON:", info$main_json_path, "\n")
  cat("Alt folder:", info$alt_dir, "\n")
  cat("============================\n\n")
  
  cat("Alts referenced in MAIN JSON (", length(info$alts_main), "):\n", sep = "")
  if (!length(info$alts_main)) cat("  - none -\n") else cat("  - ", paste(info$alts_main, collapse = "\n  - "), "\n", sep = "")
  cat("\n")
  
  cat("Alt files present in ALT folder (", length(info$alts_folder), "):\n", sep = "")
  if (!length(info$alts_folder)) cat("  - none -\n") else cat("  - ", paste(info$alts_folder, collapse = "\n  - "), "\n", sep = "")
  cat("\n")
  
  cat("ONLY in MAIN JSON (missing files) (", length(info$only_in_main), "):\n", sep = "")
  if (!length(info$only_in_main)) cat("  - none -\n") else cat("  - ", paste(info$only_in_main, collapse = "\n  - "), "\n", sep = "")
  cat("\n")
  
  cat("ONLY in ALT folder (not referenced) (", length(info$only_in_folder), "):\n", sep = "")
  if (!length(info$only_in_folder)) cat("  - none -\n") else cat("  - ", paste(info$only_in_folder, collapse = "\n  - "), "\n", sep = "")
  cat("\n")
}

# -------------------------
# Delete helpers (dry-run supported)
# -------------------------
build_dict_delete_list <- function(dict_info) {
  cc <- as_cc(dict_info$country_code)
  if (!length(dict_info$only_in_folder)) return(data.table())
  
  data.table(
    where = "dict",
    region_folder = dict_info$region_folder,
    country_code  = dict_info$country_code,
    alt = dict_info$only_in_folder,
    file_path = file.path(dict_info$alt_dir, paste0(cc, "_", dict_info$only_in_folder, ".json"))
  )[, exists := file.exists(file_path)][]
}

build_result_delete_list <- function(country_code, region_folder, alts_to_delete, result_dir) {
  cc <- as_cc(country_code)
  rdir_result <- file.path(result_dir, region_folder)
  if (!dir.exists(rdir_result) || !length(alts_to_delete)) return(data.table())
  
  alt_dir <- file.path(rdir_result, "alt")
  scan_dir <- if (dir.exists(alt_dir)) alt_dir else rdir_result
  if (!dir.exists(scan_dir)) return(data.table())
  
  files <- list.files(scan_dir, pattern = "\\.json$", full.names = FALSE)
  files <- files[startsWith(files, paste0(cc, "_"))]
  if (!length(files)) return(data.table())
  
  parsed <- parse_result_files(files, cc)
  parsed <- parsed[alt %in% alts_to_delete]
  
  if (!nrow(parsed)) return(data.table())
  
  parsed[, `:=`(
    where = "result",
    region_folder = as.character(region_folder),
    country_code  = suppressWarnings(as.integer(cc)),
    file_path     = file.path(scan_dir, file)
  )][, exists := file.exists(file_path)][
    , .(where, region_folder, country_code, alt, type, file_path, exists)
  ]
}

delete_files <- function(paths, dry_run = TRUE) {
  paths <- unique(paths)
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(invisible(list(deleted = 0L, failed = 0L, paths = character(0))))
  
  if (isTRUE(dry_run)) {
    cat("DRY RUN (no deletion). Files:\n")
    cat(paste(paths, collapse = "\n"), "\n")
    return(invisible(list(deleted = 0L, failed = 0L, paths = paths)))
  }
  
  ok <- file.remove(paths)
  invisible(list(deleted = sum(ok), failed = sum(!ok), paths = paths))
}

# -------------------------
# MAIN: Run full pipeline (dict + result + exports)
# -------------------------
run_alt_mapping <- function(dict_dir, result_dir, export_csv = TRUE) {
  dict <- scan_dict(dict_dir)
  res_wide <- dict$wide
  res_long <- dict$long
  
  res_result_long <- scan_result(result_dir, dict$region_dirs)
  res_result_compare <- compare_result_to_dict_main(res_result_long, res_long, res_wide)
  
  if (isTRUE(export_csv)) {
    fwrite(res_wide, file.path(dict_dir, "alt_mapping_comparison.csv"))
    fwrite(res_long, file.path(dict_dir, "alt_mapping_long.csv"))
    fwrite(res_result_compare, file.path(dict_dir, "alt_mapping_result_compare.csv"))
  }
  
  list(
    dict_wide = res_wide,
    dict_long = res_long,
    result_long = res_result_long,
    result_compare = res_result_compare
  )
}

# -------------------------
# High-level cleaner: one country, list/delete candidates
# -------------------------
altClean <- function(country_code,
                     dict_dir,
                     result_dir,
                     delete = FALSE) {
  
  info <- report_country(country_code, dict_dir)
  print_country_report(info)
  
  dict_candidates <- build_dict_delete_list(info)
  
  # Result delete candidates: alts present in result but NOT in dict main-json
  # (i.e., "only_in_result" alts)
  # For one country, infer by scanning result and filtering against info$alts_main
  region_folder <- info$region_folder
  rdir_result <- file.path(result_dir, region_folder)
  
  result_candidates <- data.table()
  if (dir.exists(rdir_result)) {
    dt_res <- extract_result_alts_for_country(rdir_result, country_code)
    if (nrow(dt_res)) {
      only_in_result_alts <- setdiff(unique(dt_res$alt), info$alts_main)
      result_candidates <- build_result_delete_list(country_code, region_folder, only_in_result_alts, result_dir)
    }
  }
  
  all_candidates <- rbindlist(list(
    dict_candidates[, .(where, region_folder, country_code, alt, type = NA_character_, file_path, exists)],
    result_candidates
  ), fill = TRUE)
  
  cat("\n============================\n")
  cat("altClean summary\n")
  cat("Delete mode:", ifelse(isTRUE(delete), "YES (will delete)", "NO (dry run - list only)"), "\n")
  cat("DICT candidates:", nrow(dict_candidates), " | existing:", sum(dict_candidates$exists %in% TRUE), "\n")
  cat("RESULT candidates:", nrow(result_candidates), " | existing:", sum(result_candidates$exists %in% TRUE), "\n")
  cat("TOTAL candidates:", nrow(all_candidates), " | existing:", sum(all_candidates$exists %in% TRUE), "\n")
  cat("============================\n\n")
  
  existing_paths <- all_candidates[exists == TRUE, file_path]
  delete_files(existing_paths, dry_run = !isTRUE(delete))
  
  invisible(list(
    info = info,
    dict_to_delete = dict_candidates,
    result_to_delete = result_candidates,
    to_delete = all_candidates
  ))
}

# ============================================================
# USAGE
# ============================================================

# # 1) Run full mapping and export CSVs
# out <- run_alt_mapping(cfg$dict_dir, cfg$result_dir, export_csv = TRUE)
# View(out$dict_wide)
# View(out$dict_long)
# View(out$result_compare)
# 
# # 2) Single-country report + dry-run delete listing
# altClean(630, dict_dir = cfg$dict_dir, result_dir = cfg$result_dir, delete = FALSE)
# 
# # 3) Actually delete (be careful)
# altClean(630, dict_dir = cfg$dict_dir, result_dir = cfg$result_dir, delete = TRUE)

