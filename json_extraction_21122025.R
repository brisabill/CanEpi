install.packages("data.table")
install.packages("jsonlite")
install.packages("fs")
install.packages("stringr")
install.packages("rmarkdown")
install.packages("tinytex")

library(data.table)
library(jsonlite)
library(fs)
library(stringr)
library(rmarkdown)
library(tinytex)
library(dplyr)

setwd("C:/Users/mirandaa/Desktop/estican/estican/other/data source and methods/json_extraction")

# Load dictionaries
dict_cancer   <- fread("dict_cancer.csv")
dict_country  <- fread("dict_country.csv")
dict_icd_map  <- fread("dict_icd_map.csv")

# functions
idx_from_letter <- function(letter) ifelse(letter == "i", "incidence", "mortality")
as_num <- function(x) suppressWarnings(as.numeric(x))

safe_fromJSON <- function(path) {
  tryCatch(fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
}

root_dict <- "C:/Users/mirandaa/Desktop/estican/estican/dict"
root_results <- "C:/Users/mirandaa/Desktop/estican/estican/result"

# Extract model files
files <- dir_ls(root_dict, recurse = TRUE, type = "file", glob = "*.json")
files <- files[str_detect(files, "/\\d{2}/\\d+\\.json$")]

extract_one_file <- function(path) {
  region <- as.integer(basename(path_dir(path)))
  country <- as.integer(str_remove(path_file(path), "\\.json$"))
  
  x <- safe_fromJSON(path)
  if (is.null(x) || is.null(x$model_cancer)) return(data.table())
  
  get_chunk <- function(chunk_name) {
    chunk <- x$model_cancer[[chunk_name]]
    if (is.null(chunk) || length(chunk) == 0) return(data.table())
    dt <- as.data.table(chunk)
    dt[, `:=`(
      index = chunk_name,
      country_code = country,
      region = region
    )]
    if (!"alt" %in% names(dt)) dt[, alt := NA_character_]
    if (!"coeff" %in% names(dt)) dt[, coeff := NA_real_]
    return(dt[, .(model, cancer_code, sex, alt, coeff, index, country_code, region)])
  }
  
  rbindlist(list(get_chunk("incidence"), get_chunk("mortality")), use.names = TRUE, fill = TRUE)
}

df_models_dict <- rbindlist(lapply(files, extract_one_file), fill = TRUE)
df_models_dict <- merge(df_models_dict, dict_country, by = "country_code", all.x = FALSE)
df_models_dict <- merge(df_models_dict, dict_cancer, by = "cancer_code", all.x = FALSE)
#df_models_dict <- merge(df_models_dict, unique(dict_icd_map[, .(icd_label, icd3)]), by = "icd_label", all.x = FALSE)

df_models_dict <- merge(
  df_models_dict,
  unique(dict_icd_map[, .(icd_label, icd3)]),
  by = "icd_label",
  all.x = FALSE,
  allow.cartesian = TRUE
)


df_models_dict[, alt := fifelse(model != "template" & is.na(alt), model, alt)]

# Extract results
all_files <- dir_ls(root_results, recurse = TRUE, type = "file", glob = "*.json")
main_files <- all_files[str_detect(all_files, "[/\\\\]\\d{2}[/\\\\]\\d+[im]_source\\.json$")]
alt_files <- all_files[str_detect(all_files, "[/\\\\]\\d{2}[/\\\\]alt[/\\\\]\\d+_alt_.+_[im]_source\\.json$")]

extract_main_source <- function(path) {
  region <- as.integer(basename(path_dir(path)))
  m <- str_match(path_file(path), "^(\\d+)([im])_source\\.?(json)?$")
  if (any(is.na(m))) return(data.table())
  country_code <- as.integer(m[2])
  index <- idx_from_letter(m[3])
  
  x <- safe_fromJSON(path)
  base <- data.table(
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
    base[, `:=`(id_code = NA_real_, id_label = NA_character_, source = NA_character_, period = NA_character_)]
    return(base)
  }
  
  ds_dt <- as.data.table(ds)
  ds_dt[, `:=`(
    id_code = if ("id_code" %in% names(ds_dt)) id_code else NA_real_,
    id_label = if ("id_label" %in% names(ds_dt)) id_label else NA_character_,
    source = if ("source" %in% names(ds_dt)) source else NA_character_,
    period = if ("period" %in% names(ds_dt)) period else NA_character_
  )]
  
  cbind(ds_dt[, .(id_code, id_label, source, period)], base[rep(1, .N)])
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

extract_alt_source <- function(path) {
  region <- as.integer(basename(path_dir(path_dir(path))))
  m <- str_match(path_file(path), "^(\\d+)_alt_(.+)_([im])_source\\.json$")
  if (any(is.na(m))) return(data.table())
  country_code <- as.integer(m[2])
  altname <- m[3]
  index <- idx_from_letter(m[4])
  
  x <- safe_fromJSON(path)
  base <- data.table(
    region = region,
    country_code = country_code,
    index = index,
    alt = altname,
    method = suppressWarnings(as.integer(x$method %||% NA_integer_)),
    pop_coverage = as_num(x$pop_coverage %||% NA_real_),
    WHO_completeness = as_num(x$WHO_completeness %||% NA_real_),
    file = path
  )
  
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
    base[, `:=`(id_code = NA_real_, id_label = NA_character_, source = NA_character_, period = NA_character_, chunk = NA_character_)]
    return(base)
  }
  
  ds_dt <- rbindlist(lapply(names(ds_list), function(nm) {
    dt <- as.data.table(ds_list[[nm]])
    dt[, chunk := nm]
    dt
  }), fill = TRUE)
  
  src_col <- if (index == "incidence") "inc_source" else "mort_source"
  
  getv <- function(nm) if (nm %in% names(ds_dt)) ds_dt[[nm]] else rep(NA, nrow(ds_dt))
  
  out <- data.table(
    id_code = suppressWarnings(as.numeric(coalesce(getv("id_code"), getv("id"), getv("code")))),
    id_label = coalesce(getv("id_label"), getv("label"), getv("registry"), getv("name")),
    source = coalesce(getv(src_col), getv("source"), getv("src")),
    period = coalesce(getv("period"), getv("years"), getv("year_range")),
    chunk = getv("chunk")
  )
  
  cbind(out, base[rep(1, .N)])
}

df_models_results <- rbindlist(c(lapply(main_files, extract_main_source),lapply(alt_files, extract_alt_source)), fill = TRUE
)%>% left_join(dict_country, by = c("country_code" = "country_code"))%>% distinct()


#Creation of text dataframe for except

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

# Text dataframe for coeff.
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

## Data by model
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


# Write outputs
fwrite(df_alt_text, "alt_text.csv")
fwrite(df_coeff_text, "coeff_text.csv")
fwrite(df_model_text, "model_text.csv")
fwrite(df_models_results, "df_models_results.csv")
fwrite(df_models_dict, "df_models_dict.csv")

# Generate reports
country_list <- sort(unique(df_models_dict$country_code))
out_dir <- "pdf_reports"
dir.create(out_dir, showWarnings = FALSE)

for (cc in country_list) {
  e <- new.env(parent = globalenv())
  rmarkdown::render(
    input = "country_report.Rmd",
    output_file = file.path(out_dir, paste0("country_", cc, ".pdf")),
    params = list(cc = cc),
    envir = e
  )
}
