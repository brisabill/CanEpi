library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(fs)
library(rmarkdown)
library(knitr)
library(tinytex)
library(readr)
library(dplyr)
library(fs)


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


#-------Crating the function

ageCheck <- function(cc) {
  
  OUT_BASE <- fs::path(ESTICAN_ROOT, "other", "peadriatric_check")
  OUT_DIR  <- fs::path(OUT_BASE, cc)
  dir_create(OUT_DIR)
  
  # find region folder
  region_dir <- dir_ls(RESULT_DIR, type = "directory") |>
    keep(~ file_exists(fs::path(.x, paste0(cc, "i.csv"))))
  
  if (length(region_dir) == 0) {
    stop("Country files not found for cc = ", cc)
  }
  
  RESULT_CC <- region_dir[[1]]
  
  # read files
  inc  <- read_csv(fs::path(RESULT_CC, paste0(cc, "i.csv")), show_col_types = FALSE)
  mort <- read_csv(fs::path(RESULT_CC, paste0(cc, "m.csv")), show_col_types = FALSE)
  pop  <- read_csv(fs::path(RESULT_CC, paste0(cc, "p.csv")), show_col_types = FALSE)
  
  # keep sexes
  inc  <- inc  %>% filter(sex %in% c(1, 2))
  mort <- mort %>% filter(sex %in% c(1, 2))
  pop  <- pop %>% filter(sex %in% c(1, 2))
  
  # add country code
  inc  <- inc  %>% mutate(country_code = cc)
  mort <- mort %>% mutate(country_code = cc)
  pop  <- pop %>% mutate(country_code = cc)
  
  # merge
  merged <- inc %>%
    left_join(
      mort,
      by = c("sex", "age", "cancer_code", "country_code"),
      suffix = c("_inc", "_mort")
    ) %>%
    left_join(
      pop,
      by = c("sex", "age", "country_code")
    ) %>%
    left_join(
      dict_country,
      by = "country_code"
    )
  
  # rename columns and add rates
  merged <- merged %>%
    rename(
      cases = cases_inc,
      mortality = cases_mort,
      py = py
    ) %>%
    mutate(
      rate = cases / py * 1e6,
      mortality_rate = mortality / py * 1e6
    )
  
  # --- table1 ---
  write_csv(
    merged,
    fs::path(OUT_DIR, paste0(cc, "_table1.csv"))
  )
  
  # --- table2 ---
  table2 <- merged %>%
    filter(age %in% c(1, 2, 3)) %>%
    group_by(sex, cancer_code, country_code, country_label) %>%
    summarise(
      cases = sum(cases, na.rm = TRUE),
      deaths = sum(mortality, na.rm = TRUE),
      py = sum(py, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      rate = cases / py * 1e6,
      mortality_rate = deaths / py * 1e6
    )
  
  write_csv(
    table2,
    fs::path(OUT_DIR, paste0(cc, "_table2.csv"))
  )
  
  print(table2, n = 72)
  
  # --- table3 ---
  t1_subset <- merged %>% 
    filter(age %in% c(1, 2, 3)) %>%
    filter(cancer_code %in% c(39,40)) %>%
    mutate(age = as.character(age))  # convert age to character
  
  table3_list <- list()
  
  for(sex_val in unique(t1_subset$sex)) {
    for(cancer_val in unique(t1_subset$cancer_code)) {
      
      # rows from table1 ages 1-3
      rows_t1 <- t1_subset %>%
        filter(sex == sex_val, cancer_code == cancer_val) %>%
        arrange(age)
      
      # corresponding summary row from table2
      row_t2 <- table2 %>%
        filter(sex == sex_val, cancer_code == cancer_val) %>%
        mutate(age = "all")  # now age is character
      
      # bind the rows together
      combined <- bind_rows(rows_t1, row_t2)
      
      # add to list
      table3_list <- append(table3_list, list(combined))
    }
  }
  
  table3 <- bind_rows(table3_list)
  
  write_csv(
    table3,
    fs::path(OUT_DIR, paste0(cc, "_table3.csv"))
  )
  
  
  invisible(merged)
}

