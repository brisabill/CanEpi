# Load necessary packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Plot population pyramid by ONLY country code
# - Works with: plot_population_pyramid(76), plot_population_pyramid("76"), plot_population_pyramid("076")
# - Auto-detects: correct base dir (result/), region folder (06/07/08...), and exact file name (76p.csv or 076p.csv)
plot_population_pyramid <- function(ccode,
                                   results_dir = "C:/Users/mirandaa/Desktop/estican/estican/result",
                                    global_pop_file = "//inti/CIN/DataShare/Globocan2022/source/population.csv",
                                    year_model = 2022, year_pop = 2024) {
  
  # ---- 0) Normalize input to integer for matching ----
  ccode_chr <- as.character(ccode)
  if (!grepl("^\\d+$", ccode_chr)) stop("ccode must be numeric-like (e.g., 76 or '076'). Got: ", ccode_chr)
  ccode_int <- as.integer(ccode_chr)
  
  # ---- 1) Fix results_dir automatically (your structure uses pop_pyramid/result/<region>/...) ----
  # If user passes pop_pyramid, switch to pop_pyramid/result when it exists
  if (dir.exists(file.path(results_dir, "result"))) {
    results_dir <- file.path(results_dir, "result")
  } else if (dir.exists(file.path(results_dir, "results"))) {
    results_dir <- file.path(results_dir, "results")
  }
  
  if (!dir.exists(results_dir)) stop("results_dir does not exist: ", results_dir)
  
  # ---- 2) Find the model file <ccode>p.csv allowing leading zeros ----
  # matches both 76p.csv and 076p.csv etc.
  model_pattern <- paste0("^0*", ccode_int, "p\\.csv$")
  
  region_folders <- list.dirs(results_dir, recursive = FALSE, full.names = TRUE)
  
  if (length(region_folders) == 0) {
    stop("No region folders found inside results_dir: ", results_dir,
         "\nExpected something like: ", results_dir, "/08/76p.csv")
  }
  
  matches <- character(0)
  for (fd in region_folders) {
    f <- list.files(fd, pattern = model_pattern, full.names = TRUE, ignore.case = TRUE)
    if (length(f) > 0) matches <- c(matches, f)
  }
  
  if (length(matches) == 0) {
    stop(
      "No model file found for country ", ccode_int, " inside: ", results_dir,
      "\nSearched for filename regex: ", model_pattern,
      "\nExpected something like: <results_dir>/<region>/", ccode_int, "p.csv (or 0", ccode_int, "p.csv)"
    )
  }
  
  if (length(matches) > 1) {
    stop(
      "Multiple model files found for country ", ccode_int, ":\n",
      paste(matches, collapse = "\n"),
      "\nPlease keep only one."
    )
  }
  
  pop_path <- matches[1]
  population <- read.csv(pop_path, stringsAsFactors = FALSE)
  
  # ---- 3) Read & subset the global population (handle 76 vs 076) ----
  df <- read.csv(global_pop_file, stringsAsFactors = FALSE)
  df$country_code <- as.character(df$country_code)
  
  if (!all(grepl("^\\d+$", df$country_code))) {
    stop("global_pop_file has non-numeric country_code values. Fix the file or adjust parsing.")
  }
  
  df$country_code_int <- as.integer(df$country_code)
  df_cc <- df %>% filter(country_code_int == ccode_int)
  
  if (nrow(df_cc) == 0) {
    stop("Country ", ccode_int, " not found in global_pop_file: ", global_pop_file)
  }
  
  # ---- 4) Long format ----
  df_cc_long <- df_cc %>%
    pivot_longer(
      cols = starts_with("age"),
      names_to = "age",
      names_prefix = "age",
      values_to = "py"
    ) %>%
    mutate(age = as.integer(age)) %>%
    filter(sex != 0)
  
  # ---- 5) Age groups ----
  age_labels <- c(
    "0-4","5-9","10-14","15-19","20-24","25-29",
    "30-34","35-39","40-44","45-49","50-54","55-59",
    "60-64","65-69","70-74","75-79","80-84","85+"
  )
  
  df_cc_long <- df_cc_long %>%
    mutate(age_group = factor(age, levels = 1:18, labels = age_labels))
  
  population <- population %>%
    mutate(age_group = factor(age, levels = 1:18, labels = age_labels))
  
  # ---- 6) Combine sources ----
  df_cc_long$year <- year_model
  population$year <- year_pop
  
  combined_data <- bind_rows(
    df_cc_long %>% mutate(source = as.character(year_model)),
    population %>% mutate(source = as.character(year_pop))
  )
  
  # ---- 7) Pyramid values ----
  pyramid_data <- combined_data %>%
    mutate(
      sex_label = ifelse(sex == 1, "Male", "Female"),
      pop_value = ifelse(sex == 2, -py, py)
    )
  
  # ---- 8) Plot ----
  year_colors <- setNames(
    c("#1f78b4", "#e31a1c"),
    c(as.character(year_pop), as.character(year_model))
  )
  
  p <- ggplot(pyramid_data, aes(x = age_group, y = pop_value, fill = source)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.7, width = 0.7) +
    coord_flip() +
    scale_y_continuous(
      labels = abs,
      expand = expansion(mult = c(0.02, 0.05)),
      breaks = pretty(c(-max(abs(pyramid_data$pop_value)), max(abs(pyramid_data$pop_value))))
    ) +
    scale_fill_manual(values = year_colors) +
    labs(
      title = paste0("Population Pyramid – Country ", ccode_int),
      x = "Age Group",
      y = "Person-Years",
      fill = "Year"
    ) +
    facet_grid(~sex_label, switch = "x", scales = "free_x", space = "free_x") +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      axis.title = element_text(face = "bold", size = 13),
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 13),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      panel.spacing = unit(0, "lines")
    )
  
  print(p)
  invisible(p)
}
