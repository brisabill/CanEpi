library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(fs)
library(rmarkdown)
library(knitr)
library(tinytex)

# ----Setting directories ---------
ESTICAN_ROOT <- "C:/Users/mirandaa/Project/estican"  

DICT_DIR   <- fs::path(ESTICAN_ROOT, "dict")
RESULT_DIR <- fs::path(ESTICAN_ROOT, "result")

WORK_DIR <- fs::path(ESTICAN_ROOT, "other", "json_extraction")
OUT_DIR  <- fs::path(ESTICAN_ROOT, "other", "reports")

RMD_FILE <- fs::path(WORK_DIR, "country_report.Rmd")

DICT_CANCER  <- fs::path(WORK_DIR, "dict_cancer.csv")
DICT_COUNTRY <- fs::path(WORK_DIR, "dict_country.csv")
DICT_ICD_MAP <- fs::path(WORK_DIR, "dict_icd_map.csv")

# ---Loading the dictionaries
dict_cancer <- read.delim(DICT_CANCER, sep = ",", stringsAsFactors = TRUE) %>%
  select(-any_of("X"))

dict_country <- read.delim(DICT_COUNTRY, sep = ",", stringsAsFactors = FALSE) %>%
  select(-any_of("X"))

dict_icd_map <- read.delim(DICT_ICD_MAP, sep = ",", stringsAsFactors = FALSE)

# ---- HELPERS --------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

idx_from_letter <- function(letter) ifelse(letter == "i", "incidence", "mortality")

as_num <- function(x) suppressWarnings(as.numeric(x))

safe_fromJSON <- function(path) {
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
}

col_or_na <- function(df, nm, default = NA) {
  if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
}

# ============================================================
# 1) DICT EXTRACTION 
# ============================================================

dict_files <- fs::dir_ls(DICT_DIR, recurse = TRUE, type = "file", glob = "*.json") |>
  keep(~ str_detect(.x, "[/\\\\]\\d{2}[/\\\\]\\d+\\.json$"))

extract_one_dict_file <- function(path) {
  region  <- suppressWarnings(as.integer(fs::path_file(fs::path_dir(path))))
  country <- suppressWarnings(as.integer(str_remove(fs::path_file(path), "\\.json$")))

  x <- safe_fromJSON(path)
  if (is.null(x) || is.null(x$model_cancer)) return(tibble())

  get_chunk <- function(chunk_name) {
    chunk <- x$model_cancer[[chunk_name]]
    if (is.null(chunk) || length(chunk) == 0) return(tibble())

    as_tibble(chunk) %>%
      transmute(
        model        = .data$model,
        cancer_code  = .data$cancer_code,
        sex          = .data$sex,
        alt          = if ("alt"   %in% names(.)) .data$alt   else NA_character_,
        coeff        = if ("coeff" %in% names(.)) .data$coeff else NA_real_,
        index        = chunk_name,
        country_code = country,
        region       = region
      )
  }

  bind_rows(get_chunk("incidence"), get_chunk("mortality"))
}

df_models_dict <- map_dfr(dict_files, extract_one_dict_file) %>%
  left_join(dict_country, by = c("country_code" = "country_code")) %>%
  left_join(dict_cancer,  by = c("cancer_code"  = "cancer_code")) %>%
  left_join(dict_icd_map %>% distinct(icd_label, icd3), by = "icd_label",
            relationship = "many-to-many") %>%
  mutate(
    alt = if_else(model != "template" & is.na(alt), model, alt)
  ) %>%
  distinct()

# Countries available from dict
country_list <- df_models_dict %>%
  distinct(country_code) %>%
  arrange(country_code) %>%
  pull(country_code)

# ============================================================
# 2) RESULTS EXTRACTION 
# ============================================================

all_result_files <- fs::dir_ls(RESULT_DIR, recurse = TRUE, type = "file", glob = "*.json")

main_files <- all_result_files %>%
  keep(~ str_detect(.x, "[/\\\\]\\d{2}[/\\\\]\\d+[im]_source\\.json$"))

alt_files <- all_result_files %>%
  keep(~ str_detect(.x, "[/\\\\]\\d{2}[/\\\\]alt[/\\\\]\\d+_alt_.+_[im]_source\\.json$"))

extract_main_source <- function(path) {
  region <- suppressWarnings(as.integer(fs::path_file(fs::path_dir(path))))

  m <- str_match(fs::path_file(path), "^(\\d+)([im])_source\\.json$")
  if (any(is.na(m))) return(tibble())

  country_code <- as.integer(m[2])
  index <- idx_from_letter(m[3])

  x <- safe_fromJSON(path)
  if (is.null(x)) {
    return(tibble(
      region = region, country_code = country_code, index = index, alt = NA_character_,
      method = NA_integer_, pop_coverage = NA_real_, WHO_completeness = NA_real_,
      id_code = NA_real_, id_label = NA_character_, source = NA_character_, period = NA_character_,
      chunk = NA_character_, file = path
    ))
  }

  base <- tibble(
    region = region,
    country_code = country_code,
    index = index,
    alt = NA_character_,
    method = suppressWarnings(as.integer(x$method %||% NA_integer_)),
    pop_coverage = as_num(x$pop_coverage %||% NA_real_),
    WHO_completeness = as_num(x$WHO_completeness %||% NA_real_),
    file = path
  )

  ds <- x$data_source
  if (is.null(ds) || length(ds) == 0) {
    return(base %>% mutate(
      id_code = NA_real_, id_label = NA_character_, source = NA_character_, period = NA_character_,
      chunk = NA_character_
    ))
  }

  as_tibble(ds) %>%
    transmute(
      id_code  = suppressWarnings(as.numeric(coalesce(
        if ("id_code" %in% names(.)) .data$id_code else NA_real_,
        if ("id"      %in% names(.)) .data$id      else NA_real_,
        if ("code"    %in% names(.)) .data$code    else NA_real_
      ))),
      id_label = as.character(coalesce(
        if ("id_label" %in% names(.)) .data$id_label else NA_character_,
        if ("label"    %in% names(.)) .data$label    else NA_character_,
        if ("registry" %in% names(.)) .data$registry else NA_character_,
        if ("name"     %in% names(.)) .data$name     else NA_character_
      )),
      source = as.character(coalesce(
        if ("source" %in% names(.)) .data$source else NA_character_,
        if ("src"    %in% names(.)) .data$src    else NA_character_
      )),
      period = as.character(coalesce(
        if ("period"     %in% names(.)) .data$period     else NA_character_,
        if ("years"      %in% names(.)) .data$years      else NA_character_,
        if ("year_range" %in% names(.)) .data$year_range else NA_character_
      )),
      chunk = "MAIN"
    ) %>%
    bind_cols(., base[rep(1, nrow(.)), ])
}

extract_alt_source <- function(path) {
  region <- suppressWarnings(as.integer(fs::path_file(fs::path_dir(fs::path_dir(path)))))

  m <- str_match(fs::path_file(path), "^(\\d+)_alt_(.+)_([im])_source\\.json$")
  if (any(is.na(m))) return(tibble())

  country_code <- as.integer(m[2])
  altname <- m[3]
  index <- idx_from_letter(m[4])

  x <- safe_fromJSON(path)
  if (is.null(x)) {
    return(tibble(
      region = region, country_code = country_code, index = index, alt = altname,
      method = NA_integer_, pop_coverage = NA_real_, WHO_completeness = NA_real_,
      id_code = NA_real_, id_label = NA_character_, source = NA_character_, period = NA_character_,
      chunk = NA_character_, file = path
    ))
  }

  base <- tibble(
    region = region,
    country_code = country_code,
    index = index,
    alt = altname,
    method = suppressWarnings(as.integer(x$method %||% NA_integer_)),
    pop_coverage = as_num(x$pop_coverage %||% NA_real_),
    WHO_completeness = as_num(x$WHO_completeness %||% NA_real_),
    file = path
  )

  # collect data_source at root or inside named chunks; keep chunk name
  ds_list <- list()
  if (!is.null(x$data_source)) {
    ds_list[[".root"]] <- x$data_source
  } else {
    for (nm in names(x)) {
      obj <- x[[nm]]
      if (is.list(obj) && !is.null(obj$data_source)) ds_list[[nm]] <- obj$data_source
    }
  }

  if (length(ds_list) == 0) {
    return(base %>% mutate(
      id_code = NA_real_, id_label = NA_character_, source = NA_character_, period = NA_character_,
      chunk = NA_character_
    ))
  }

  ds <- purrr::imap_dfr(ds_list, ~ tibble::as_tibble(.x) %>% mutate(chunk = .y))

  src_col <- if (index == "incidence") "inc_source" else "mort_source"

  as_tibble(ds) %>%
    transmute(
      id_code  = suppressWarnings(as.numeric(coalesce(
        if ("id_code" %in% names(.)) .data$id_code else NA_real_,
        if ("id"      %in% names(.)) .data$id      else NA_real_,
        if ("code"    %in% names(.)) .data$code    else NA_real_
      ))),
      id_label = as.character(coalesce(
        if ("id_label" %in% names(.)) .data$id_label else NA_character_,
        if ("label"    %in% names(.)) .data$label    else NA_character_,
        if ("registry" %in% names(.)) .data$registry else NA_character_,
        if ("name"     %in% names(.)) .data$name     else NA_character_
      )),
      source = as.character(coalesce(
        if (src_col   %in% names(.)) .data[[src_col]] else NA_character_,
        if ("source"  %in% names(.)) .data$source     else NA_character_,
        if ("src"     %in% names(.)) .data$src        else NA_character_
      )),
      period = as.character(coalesce(
        if ("period"     %in% names(.)) .data$period     else NA_character_,
        if ("years"      %in% names(.)) .data$years      else NA_character_,
        if ("year_range" %in% names(.)) .data$year_range else NA_character_
      )),
      chunk = .data$chunk
    ) %>%
    bind_cols(., base[rep(1, nrow(.)), ])
}

df_models_results <- bind_rows(
  map_dfr(main_files, extract_main_source),
  map_dfr(alt_files,  extract_alt_source)
) %>%
  left_join(dict_country, by = c("country_code" = "country_code")) %>%
  distinct()

# Sources table (your Rmd likely uses this)
df_sources_by_chunk <- df_models_results %>%
  filter(!is.na(id_label)) %>%
  mutate(
    alt_display = if_else(is.na(alt) | alt == "", "MAIN", alt),
    chunk = if_else(is.na(chunk) | chunk == "" | chunk == ".root", "MAIN", chunk),
    chunk = factor(chunk, levels = c("MI_country", "MI_area", "MI_continent", "MAIN"))
  ) %>%
  group_by(country_code, index, alt_display, chunk) %>%
  summarise(col1 = str_c(unique(id_label), collapse = ", "), .groups = "drop") %>%
  arrange(country_code, index, alt_display, chunk)

# ============================================================
# 3) CREATION OF TEXTS
# ============================================================

df_alt_text <- df_models_dict %>%
  filter(!is.na(alt)) %>%
  mutate(
    cancer_label = str_squish(cancer_label),
    sex_txt = case_when(sex == 1 ~ "male", sex == 2 ~ "female", TRUE ~ NA_character_)
  ) %>%
  group_by(country_label, alt, index, cancer_label) %>%
  mutate(has_both_sexes = (gender == 0) & n_distinct(sex[sex %in% c(1,2)]) == 2) %>%
  ungroup() %>%
  mutate(
    cancer_txt = case_when(
      gender != 0    ~ cancer_label,
      has_both_sexes ~ cancer_label,
      TRUE           ~ str_c(cancer_label, " (", sex_txt, ")")
    ),
    cancer_txt = str_squish(cancer_txt)
  ) %>%
  group_by(country_label, alt, index, cancer_txt) %>%
  summarise(icd3_min = suppressWarnings(min(icd3, na.rm = TRUE)), .groups = "drop") %>%
  arrange(country_label, alt, index, icd3_min, cancer_txt) %>%
  group_by(country_label, alt, index) %>%
  summarise(col1 = str_c(cancer_txt, collapse = ", "), .groups = "drop") %>%
  left_join(df_models_dict %>% distinct(country_label, country_code), by = "country_label") %>%
  distinct()

df_coeff_text <- df_models_dict %>%
  filter(!is.na(coeff), abs(coeff - 1) > 1e-9) %>%
  mutate(
    cancer_label = str_squish(cancer_label),
    sex_txt   = case_when(sex == 1 ~ "male", sex == 2 ~ "female", TRUE ~ NA_character_),
    coeff_txt = sub("\\.?0+$", "", formatC(round(coeff, 2), format = "f", digits = 2))
  ) %>%
  group_by(country_label, index, cancer_label, coeff_txt) %>%
  mutate(has_both_sexes = (gender == 0) & n_distinct(sex[sex %in% c(1,2)]) == 2) %>%
  ungroup() %>%
  mutate(
    cancer_txt = case_when(
      gender != 0    ~ str_c(cancer_label, " (", coeff_txt, ")"),
      has_both_sexes ~ str_c(cancer_label, " (", coeff_txt, ")"),
      TRUE           ~ str_c(cancer_label, " (", sex_txt, " ", coeff_txt, ")")
    ),
    cancer_txt = str_squish(cancer_txt)
  ) %>%
  group_by(country_label, index, cancer_txt) %>%
  summarise(icd3_min = suppressWarnings(min(icd3, na.rm = TRUE)), .groups = "drop") %>%
  arrange(country_label, index, icd3_min, cancer_txt) %>%
  group_by(country_label, index) %>%
  summarise(
    col1 = str_c("Coeff was applied to ", str_c(cancer_txt, collapse = ", ")),
    .groups = "drop"
  ) %>%
  left_join(df_models_dict %>% distinct(country_label, country_code), by = "country_label") %>%
  distinct()

df_model_text <- df_models_dict %>%
  filter(!is.na(model)) %>%
  mutate(
    cancer_label = str_squish(cancer_label),
    sex_txt = case_when(sex == 1 ~ "male", sex == 2 ~ "female", TRUE ~ NA_character_)
  ) %>%
  group_by(country_label, model, index, cancer_label) %>%
  mutate(has_both_sexes = (gender == 0) & n_distinct(sex[sex %in% c(1,2)]) == 2) %>%
  ungroup() %>%
  mutate(
    cancer_txt = case_when(
      gender != 0    ~ cancer_label,
      has_both_sexes ~ cancer_label,
      TRUE           ~ str_c(cancer_label, " (", sex_txt, ")")
    ),
    cancer_txt = str_squish(cancer_txt)
  ) %>%
  group_by(country_label, model, index, cancer_txt) %>%
  summarise(icd3_min = suppressWarnings(min(icd3, na.rm = TRUE)), .groups = "drop") %>%
  arrange(country_label, model, index, icd3_min, cancer_txt) %>%
  group_by(country_label, model, index) %>%
  summarise(col1 = str_c(cancer_txt, collapse = ", "), .groups = "drop") %>%
  left_join(df_models_dict %>% distinct(country_label, country_code), by = "country_label") %>%
  distinct()

# ============================================================
# 4) Creation of functions
# ============================================================
# 1. pdfReport
pdfReport <- function(cc,
                      out_dir = OUT_DIR,
                      input_rmd = RMD_FILE) {
  cc <- as.integer(cc)
  if (is.na(cc)) stop("cc must be numeric (e.g., 76).")

  fs::dir_create(out_dir, recurse = TRUE)
  pdf_path <- fs::path(out_dir, paste0("country_", cc, ".pdf"))

  # fresh env each render
  e <- new.env(parent = globalenv())

  rmarkdown::render(
    input = input_rmd,
    output_file = pdf_path,
    params = list(cc = cc),
    envir = e,
    quiet = TRUE,
    knit_root_dir = WORK_DIR
  )

  invisible(pdf_path)
}

#2. consoleReport
consoleReport <- function(cc, rmd = RMD_FILE) {
  cc <- as.integer(cc)
  if (is.na(cc)) stop("cc must be numeric (e.g., 76).")

  tmp <- tempfile(fileext = ".md")
  e <- new.env(parent = globalenv())

  rmarkdown::render(
    input = rmd,
    output_format = "md_document",
    output_file = tmp,
    params = list(cc = cc),
    envir = e,
    quiet = TRUE,
    knit_root_dir = WORK_DIR
  )

  cat(paste(readLines(tmp, warn = FALSE), collapse = "\n"))
  invisible(NULL)
}

# 3. countryReport (main function)
# cc = single country code OR "all"
# all = TRUE -> render PDFs for all countries in dict
countryReport <- function(cc = NULL,
                          all = FALSE,
                          print = FALSE,
                          out_dir = OUT_DIR,
                          rmd = RMD_FILE) {

  if (is.character(cc) && length(cc) == 1 && tolower(cc) == "all") {
    all <- TRUE
  }

  if (isTRUE(all)) {
    if (length(country_list) == 0) stop("No countries found in dict JSONs.")
    paths <- character(0)

    for (k in country_list) {
      p <- pdfReport(k, out_dir = out_dir, input_rmd = rmd)
      paths <- c(paths, p)
      if (isTRUE(print)) consoleReport(k, rmd = rmd)
    }
    return(invisible(paths))
  }

  # single country
  if (is.null(cc)) stop("Provide cc (e.g. 76) or use all = TRUE / cc = 'all'.")
  cc <- as.integer(cc)
  if (is.na(cc)) stop("cc must be numeric (e.g., 76).")

  p <- pdfReport(cc, out_dir = out_dir, input_rmd = rmd)
  if (isTRUE(print)) consoleReport(cc, rmd = rmd)

  invisible(p)
}


