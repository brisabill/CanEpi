# Mapping of alts and deletion of unused files

This repository maps used alts and delets unused alt both in dict and results folder and .json and .csv files.
---
```r
source("altCode.R")

# If delete = FALSE, then it will only list what files (with directory path) will be deleted
# Usage:
altClean(188, delete = FALSE)
# altClean(188, delete = TRUE)

```

# Creation of country reports and pdf export

This script extracts data and exports into a pdf file or prints info directly in the R console. It is dependent on the .Rmd file. It will create a folder named pdf_reports and place the created files within it

---
```r
source("code_data_estraction.R")

#Usage:
pdfReport(76)

#Usage:
consoleReport (76)

#Usage:
# One country (PDF only):
countryReport(76)

# One country + print to console:
countryReport(76, print = TRUE)

# All countries (PDFs only):
countryReport(all = TRUE)

# All countries (PDFs) + console:
countryReport(all = TRUE, print = TRUE)

# Also:
countryReport("all")

---
```
# Plot pf population pyramid
---
```r
source("plot_population_pyramid.R")
#Usage
pop_pyramid(250)
---
```
# To age specifiC incidence rates for Chilhood cancer

---
```r
source("pop_inc.R")
#Usage:

ageCheck(76)               # just tables
ageCheck(724, TRUE, 2022)  # tables + comparison
---
```

