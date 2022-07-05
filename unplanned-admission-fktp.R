library(tidyverse)
library(haven)
library(lubridate)
library(gtsummary)
library(skimr)
library(comorbidity)

## Load data
load("~/Documents/Projects/Personal - Readmission Study/data.RData")
fkrtl_sec$id <- as.numeric(factor(fkrtl_sec$FKL02, levels = unique(fkrtl_sec$FKL02)))
fkrtl$PSTV01 <- as.character(fkrtl$PSTV01)
fkrtl$FKL02 <- as.character(fkrtl$FKL02)
peserta$PSTV01 <- as.character(peserta$PSTV01)

# Calculate Charlson Comorbidity Index for each visit
cci <- comorbidity(x = fkrtl_sec, id = "id", code = "FKL24", map = "charlson_icd10_quan", assign0 = FALSE)
cci_score <- score(cci, assign0 = FALSE, weights = NULL)
cci$score <- score(cci, assign0 = FALSE, weights = NULL)
cci <- full_join(fkrtl_sec, cci, by = "id")