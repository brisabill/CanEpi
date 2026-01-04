library(data.table)
library(haven)
setwd("C:/Users/brisa/Documents/Working Directory")
#Checking directories
list.files()
list.files("data")

#Loading the data
#Variable definition
variable_def <- read.delim("data/variable_def.csv", sep = ",", stringsAsFactors = FALSE)
#Id dictionary
id_dict <- read.delim("data/id_dict.csv", sep = ",", stringsAsFactors = FALSE)
#ICD Dictionary
dict_cancer <- read.delim("data/dict_cancer.csv", sep = ",", stringsAsFactors = FALSE)
#Main dataset
survcan <- readRDS("data/survcan.rds") 

#Checking data structure
str(variable_def)
str(id_dict)
str(survcan)

head(variable_def)
head(id_dict)
head(survcan)

#Converts the objects from data.frame to data.table
setDT(variable_def)
setDT(id_dict)
setDT(survcan)

#Attacht the registry info from id_dict to the main dataset
survcan <- id_dict[survcan, on = "regcode"]

###labeling ICD codes into the main dataset###

#check for entries with NA values for icd10
survcan[is.na(icd10)]
sum(is.na(survcan$icd10))

#Convert ICD codes in the main dataset (surcan) to numeric so that R can compare them to ICD codes in dict_cancer dataset

survcan[, icd3 := substr(icd10, 2, 3)]   # e.g., "163" → "16"
survcan[, icd3 := as.numeric(icd3)]

#Convert dict_cancer to data.table
setDT(dict_cancer)

#Expand the entries in dict_cancer$icd_labels into numeric values

#1. Start with single ICD3 values, skip aggregates and different rows
dict_for_map <- dict_cancer[
  !(icd_label %in% c("OTH", "C00-97", "C00-97/C44"))
]

#2. creates a function to turn text labels into numeric vectors
expand_icd_label <- function(lbl) {
  parts <- strsplit(lbl, "\\+")[[1]]  # split on +
  out <- integer()
  
  for (p in parts) {
    p <- trimws(p)
    if (p == "") next
    
    if (grepl("-", p)) {
      # range, e.g. "C00-06" or "C82-86"
      bounds <- strsplit(p, "-", fixed = TRUE)[[1]]
      
      start <- as.integer(substr(bounds[1], 2, 3))       # "C00" -> 0
      end_str <- bounds[2]
      
      # second part may be "06" or "C06"
      if (startsWith(end_str, "C")) {
        end <- as.integer(substr(end_str, 2, 3))
      } else {
        end <- as.integer(end_str)
      }
      
      out <- c(out, seq(start, end))
    } else {
      # single code like "C11" or "C88"
      out <- c(out, as.integer(substr(p, 2, 3)))
    }
  }
  
  unique(out)
}

#3. Build mapping table with one row per ICD3
dict_icd_map <- dict_for_map[
  , .(icd3 = expand_icd_label(icd_label)),
  by = .(cancer_code, cancer_label, short_label, gender, icd_label)
]

#Merge values from dict_icd_map to survcan based on icd3 code into the new df1 dataset
df1 <- survcan
df1 <- dict_icd_map[df1, on = "icd3"]

#Check for unmapped ICD codes
df1[is.na(cancer_code), unique(icd10)]
df1[is.na(cancer_code), .N]

#Adds a label to all the unmmapped ICD3
df1[is.na(cancer_label), cancer_label := "Other specified cancers"]
df1[is.na(short_label),  short_label  := "Oth. specified"]
df1[is.na(cancer_code),  cancer_code  := 37]
df1[is.na(gender),  gender  := 0]
df1[is.na(icd_label),  icd_label  := "OTH"]

#Check for NA values in column cancer_code; ensure every entry has been labeled
df1[is.na(cancer_code), .N]

#remove entries 3 and 4 for sex
df1 <- df1[sex %in% c(1, 2)]

#Export to .csv
write.csv(df1, "survcan_cleaned.csv", row.names = FALSE)


##Table to check icd_label by sex
#turn sex into a factor variabloe so that it labels as male and female instead of 1 and 2
df1$sex <- as_factor(df1$sex, levels = "label")
#Creates table with cancer type by sex
wide_table <- dcast(df1, icd_label ~ sex, fun.aggregate = length, value.var = "sex")

#do the same using crosstable
crosstable (df1,
            c(cancer_label),
            by = "sex"
          ) %>%
  as_flextable()

### Table to check entries for cancer behaviors
#Check object class
class (df1$beh)
#convert to factor
df1$beh <- as.factor(df1$beh)
#check if there are NA values in variable beh
sum(is.na(df1$beh))
#simple table
table (df1$beh)

#Saving table output with percentages
#creates dataframe with counts
df_beh_table <- df1[, .N, by = beh][order(beh)]
#adds percentage column to dataframe
df_beh_table[, percent := round((N / sum(N)) * 100, 2)]

#check the cancer behavior by sex
unique (df1$sex)
#Create crosstable
crosstable (df1,
            c(beh),
            by ="sex") %>%
  as_flextable()

#Table with absolute counts of each cancer type
crosstable (df1,
            cancer_label) %>%
  as_flextable()

#table with percentages ordered
#most common cancer Trachea, bronchus and lung
tab1 <- df1 %>%
  count(cancer_label) %>%
  mutate(perc = n / sum(n)) %>%
  arrange(desc(perc))

#Histogram for age distribution
ggplot(df1 %>% filter(!is.na(age) & age >= 0 & age <= 120),
       aes(x = age)) +
  geom_histogram(binwidth = 5, color = "white", fill = "#4A90E2") +
  scale_x_continuous(
    limits = c(min(df1$age, na.rm = TRUE), max(105)),
    breaks = seq(
      floor(min(df1$age, na.rm = TRUE) / 5) * 5,
      ceiling(max(df1$age, na.rm = TRUE) / 5) * 5,
      by = 5
    )
  ) +
  labs(x = "Age at diagnosis", y = "Count")
#check for the amount of entries in which age is more than 105
sum (df1$age > 105)

#survival time histogram general
ggplot(df1 %>% filter(!is.na(surv_dd) & surv_dd >= 0),
       aes(x = surv_dd)) +
  geom_histogram(binwidth = 50, color = "white", fill = "#7DCE82") +
  scale_x_continuous(
    limits = c(0, max(5110)),
    breaks = seq(0,
                 ceiling(max(df1$surv_dd, na.rm = TRUE) / 365) * 365,
                 by = 365)
  ) +
  labs(x = "Survival time (days)", y = "Count")

#Survival timne histogram for Trachea, bronchus and lung
ggplot(df1 %>% filter(!is.na(surv_dd) & surv_dd >= 0 & cancer_label %in% c("Trachea, bronchus and lung")),
       aes(x = surv_dd)) +
  geom_histogram(binwidth = 50, color = "white", fill = "#7DCE82") +
  scale_x_continuous(
    limits = c(0, max(5110)),
    breaks = seq(0,
                 ceiling(max(df1$surv_dd, na.rm = TRUE) / 365) * 365,
                 by = 365)
  ) +
  labs(x = "Survival time (days)", y = "Count") 

