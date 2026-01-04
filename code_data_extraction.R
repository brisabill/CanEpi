library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(fs)
library (rmarkdown)
library (knitr)
library (purrr)
library (tinytex)

setwd("C:/Users/brisa/Documents/code_data_extraction")

#Load dictionaries
dict_cancer <- read.delim("dictionaries/dict_cancer.csv", sep = ",", stringsAsFactors = TRUE) %>%
  select (-X)

dict_country <- read.delim("dictionaries/dict_country.csv", sep = ",", stringsAsFactors = FALSE)%>%
  select (-X)

dict_icd_map <- read.delim("dictionaries/dict_icd_map.csv", sep = ",", stringsAsFactors = FALSE)

# -------- helpers --------
idx_from_letter <- function(letter) ifelse(letter == "i", "incidence", "mortality")

as_num <- function(x) suppressWarnings(as.numeric(x))

safe_fromJSON <- function(path) {
  tryCatch(fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
}
root_dict <- "C:/Users/brisa/Documents/code_data_extraction/dict"

root_results <- "C:/Users/brisa/Documents/code_data_extraction/result"

####Extraction from dict folder
# ONLY JSONs directly under each region folder: dict/09/840.json
files <- dir_ls(root_dict, recurse = TRUE, type = "file", glob = "*.json") |>
  keep(~ str_detect(.x, "/\\d{2}/\\d+\\.json$"))

extract_one_file <- function(path) {
  
  region <- as.integer(basename(path_dir(path)))
  country <- as.integer(str_remove(path_file(path), "\\.json$"))
  
  x <- tryCatch(
    fromJSON(path, simplifyVector = TRUE),
    error = function(e) return(NULL)
  )
  if (is.null(x) || is.null(x$model_cancer)) return(tibble())
  
  get_chunk <- function(chunk_name) {
    chunk <- x$model_cancer[[chunk_name]]
    if (is.null(chunk) || length(chunk) == 0) return(tibble())
    
    as_tibble(chunk) %>%
      transmute(
        model   = .data$model,
        cancer_code  = .data$cancer_code,
        sex     = .data$sex,
        alt     = if ("alt"   %in% names(.)) .data$alt   else NA_character_,
        coeff   = if ("coeff" %in% names(.)) .data$coeff else NA_real_,
        index   = chunk_name,
        country_code = country,
        region  = region
      )
  }
  
  bind_rows(get_chunk("incidence"), get_chunk("mortality"))
}

####Assembling dict dataframe with extracted information

df_models_dict <- map_dfr(files, extract_one_file) %>%
  left_join(dict_country, by = c("country_code" = "country_code"))%>%
  left_join(dict_cancer, by = c("cancer_code" = "cancer_code"))%>%
  left_join(dict_icd_map %>% #Adds icd3 label to that ICD codes can be numerically ordered
              distinct(icd_label, icd3), by = "icd_label") %>%
  mutate(
    alt = if_else(model != "template" & is.na(alt),
                  model,
                  alt)
  )


####Extraction from results folder

all_files <- dir_ls(root_results, recurse = TRUE, type = "file", glob = "*.json")

main_files <- all_files %>%
  keep(~ str_detect(.x, "[/\\\\]\\d{2}[/\\\\]\\d+[im]_source\\.json$"))

alt_files <- all_files %>%
  keep(~ str_detect(.x, "[/\\\\]\\d{2}[/\\\\]alt[/\\\\]\\d+_alt_.+_[im]_source\\.json$"))

extract_main_source <- function(path) {
  region <- as.integer(basename(path_dir(path)))
  
  m <- str_match(path_file(path), "^(\\d+)([im])_source\\.?(json)?$")
  if (any(is.na(m))) return(tibble())
  
  country_code <- as.integer(m[2])
  index   <- idx_from_letter(m[3])
  
  x <- safe_fromJSON(path)
  if (is.null(x)) {
    return(tibble(region, country_code, index, alt = NA_character_,
                  method = NA_integer_, pop_coverage = NA_real_, WHO_completeness = NA_real_,
                  id_code = NA_real_, id_label = NA_character_, source = NA_character_, period = NA_character_,
                  file = path))
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
    # keep one row for the file
    return(base %>% mutate(
      id_code = NA_real_, id_label = NA_character_, source = NA_character_, period = NA_character_
    ))
  }
  
  as_tibble(ds) %>%
    transmute(
      id_code  = if ("id_code" %in% names(.)) .data$id_code else NA_real_,
      id_label = if ("id_label" %in% names(.)) .data$id_label else NA_character_,
      source   = if ("source" %in% names(.)) .data$source else NA_character_,
      period   = if ("period" %in% names(.)) .data$period else NA_character_
    ) %>%
    bind_cols(., base[rep(1, nrow(.)), ])
}

col_or_na <- function(df, nm, default = NA) {
  if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
}

extract_alt_source <- function(path) {
  region <- as.integer(basename(path_dir(path_dir(path))))
  m <- str_match(path_file(path), "^(\\d+)_alt_(.+)_([im])_source\\.json$")
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
      file = path
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
  
  # ---- collect data_source from ANY level + keep chunk name ----
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
  
  ds_tbl <- purrr::imap_dfr(ds_list, ~ tibble::as_tibble(.x) %>% mutate(chunk = .y))
  
  # pick inc_source vs mort_source depending on index
  src_col <- if (index == "incidence") "inc_source" else "mort_source"
  
  # safe getter (vector of correct length, or NA)
  colv <- function(nm) if (nm %in% names(ds_tbl)) ds_tbl[[nm]] else rep(NA, nrow(ds_tbl))
  
  out <- ds_tbl %>%
    transmute(
      id_code  = suppressWarnings(as.numeric(dplyr::coalesce(
        colv("id_code"), colv("id"), colv("code")
      ))),
      id_label = as.character(dplyr::coalesce(
        colv("id_label"), colv("label"), colv("registry"), colv("name")
      )),
      source   = as.character(dplyr::coalesce(
        colv(src_col), colv("source"), colv("src")
      )),
      period   = as.character(dplyr::coalesce(
        colv("period"), colv("years"), colv("year_range")
      )),
      chunk    = as.character(colv("chunk"))
    ) %>%
    distinct(id_code, id_label, source, period, chunk, .keep_all = FALSE)
  
  bind_cols(out, base[rep(1, nrow(out)), ])
  
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

#assembling sources by code chunk datafra
df_sources_by_chunk <- df_models_results %>%
  filter(index == "incidence", !is.na(alt), !is.na(id_label)) %>%
  mutate(
    chunk = if_else(is.na(chunk) | chunk == "", ".root", chunk),
    # nice display order (optional)
    chunk = factor(chunk, levels = c("MI_country", "MI_area", "MI_continent", ".root"))
  ) %>%
  group_by(country_code, alt, chunk) %>%
  summarise(
    col1 = str_c(unique(id_label), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(country_code, alt, chunk)

#Assembling results dataframe
df_models_results <- bind_rows(
  map_dfr(main_files, extract_main_source),
  map_dfr(alt_files,  extract_alt_source)
)%>%
  left_join(dict_country, by = c("country_code" = "country_code"))%>%
  distinct()


#Creation of text dataframe for except; listing sex only when gender = 0 (not listing sex for gender specific cancer) and listing cancer order according to numerical value in icd3.

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

#creation of text dataframe for coeff; listing sex only when gender = 0 (not listing sex for gender specific cancer) and listing cancer order according to numerical value in icd3.
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
  left_join(df_models_dict %>% distinct(country_label, country_code), by = "country_label")%>%
  distinct()

#Creation of dataframe to list model
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
  summarise(col1 = str_c(cancer_txt, collapse = ", "), .groups = "drop")%>%
  left_join(df_models_dict %>% distinct(country_label, country_code), by = "country_label")%>%
  distinct()


#exporting to .csv
write.csv(df_alt_text, "alt_text.csv", row.names = FALSE)

write.csv(df_coeff_text, "coeff_text.csv", row.names = FALSE)

write.csv(df_model_text, "model_text.csv", row.names = FALSE)

write.csv(df_models_results, "df_models_results.csv")

write.csv(df_models_dict, "df_models_dict.csv")

####PDF exports
country_list <- df_models_dict %>%
  distinct(country_code) %>%
  arrange(country_code) %>%
  pull(country_code)

out_dir <- "pdf_reports"
dir.create(out_dir, showWarnings = FALSE)

for (cc in country_list) {
  e <- new.env(parent = globalenv())   # IMPORTANT: fresh env each time
  rmarkdown::render(
    input = "country_report.Rmd",
    output_file = file.path(out_dir, paste0("country_", cc, ".pdf")),
    params = list(cc = cc),
    envir = e
  )
}

# ---- PDF helper: render ONE country on demand ----
pdfReport <- function(cc,
                      out_dir = "pdf_reports",
                      input_rmd = "country_report.Rmd") {
  cc <- as.integer(cc)
  if (is.na(cc)) stop("cc must be a numeric country_code (e.g., 300).")
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # fresh env so each render is isolated (same behavior as your loop)
  e <- new.env(parent = globalenv())
  
  rmarkdown::render(
    input = input_rmd,
    output_file = file.path(out_dir, paste0("country_", cc, ".pdf")),
    params = list(cc = cc),
    envir = e
  )
}




