setwd("C:/Users/brisa/Documents/projector_embeddings")

library (Rcan)
library(dplyr)
install.packages("catboost")
library (haven)
library(data.table)


globocan <- read.delim("Globocan.csv", sep = ",", stringsAsFactors = FALSE)

data ("csu_CI5XII_data")

df1 <- csu_CI5XII_data

#creates coutry mapping label to code
country_map <- globocan %>%
  select(country_code, country_label) %>%
  distinct() %>%
  arrange(country_code)

#adds country_label to CI5 dataset 
df1 <- df1 %>%
  left_join(
    country_map,
    by = "country_code"
  )

length (unique(csu_CI5XII_data$country_code))
length (unique(globocan$country_code))

table(unique(csu_CI5XII_data$country_code))
table (unique(globocan$country_code))

unique(csu_CI5XII_data$country_code)
unique(globocan$country_code)

setdiff(
  unique(csu_CI5XII_data$country_code),
  unique(globocan$country_code)
)

setdiff(
  unique(globocan$country_code),
  unique(csu_CI5XII_data$country_code)
)

intersect(
  unique(csu_CI5XII_data$country_code),
  unique(globocan$country_code)
)

setdiff(
  unique(df1$country_code),
  unique(globocan$country_code)
)

sum (!df1$country_code %in% c(690, 156, 438), na.rm = FALSE)
sum (is.na(df1$country_code))

#Export NEW_DATA.csv for model prediction
df <- df1 %>%
    filter (!country_label %in% c(690, 156,438))%>%
    filter (!is.na(country_label))%>%
    select (-id_label, -country_code, country_label)

write.csv(df, "NEW_DATA.csv", row.names = FALSE)

####Models trained based on country_code
#just US
df2 <- read.delim("NEW_DATA_with_country_predictions.csv", sep = ",", stringsAsFactors = FALSE)

#Just Brazil
df3 <- read.delim("NEW_DATA_with_country_predictions.csv", sep = ",", stringsAsFactors = FALSE)

#entire CI5 dataset
df4 <- read.delim("NEW_DATA_with_country_predictions.csv", sep = ",", stringsAsFactors = FALSE)

#CI5 dataset with NA's in country_label removed 
df5 <- read.delim("NEW_DATA_with_country_predictions.csv", sep = ",", stringsAsFactors = FALSE)

####Models trained based on country_label
##CI5 dataset with NA's in country_label removed 
df6 <- read.delim("NEW_DATA_with_country_predictions.csv", sep = ",", stringsAsFactors = FALSE)


#create a dataset that will compare the true countries with the predicted country
cmp <- df %>%
  select(country_label,id_code, ethnic_code, cancer_code, sex, age, py, period) %>%
  inner_join(
    df5 %>% select(id_code, ethnic_code, cancer_code, sex, age, py, period, starts_with("pred_top")),
    by = c("id_code", "ethnic_code", "cancer_code", "sex", "age", "py", "period")
  ) %>%
  rename(true_country = country_label)

#Adds country label
cmp1 <- cmp %>%
  left_join(country_map, by = c("pred_top1" = "country_code")) %>%
  rename(pred_top1_label = country_label) %>%
  
  left_join(country_map, by = c("pred_top2" = "country_code")) %>%
  rename(pred_top2_label = country_label) %>%
  
  left_join(country_map, by = c("pred_top3" = "country_code")) %>%
  rename(pred_top3_label = country_label) %>%
  
  left_join(country_map, by = c("pred_top4" = "country_code")) %>%
  rename(pred_top4_label = country_label) %>%
  
  left_join(country_map, by = c("pred_top5" = "country_code")) %>%
  rename(pred_top5_label = country_label)






