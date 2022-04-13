## Load library
library(tidyverse)
library(haven)
library(lubridate)
library(comorbidity)
library(gtsummary)
library(skimr)

## Load data
load("~/Documents/Projects/Personal - Readmission Study/data.RData")
fkrtl_sec <- read_dta("~/Documents/Dataset/BPJSKv3/05_diagnosissekunder.dta") %>% select(FKL02, FKL24) %>% 
  rename(visitId = FKL02, icd2 = FKL24)
fkrtl1$id <- as.character(fkrtl1$id)
fkrtl1$visitId <- as.character(fkrtl1$visitId)
peserta1$id <- as.character(peserta1$id)

# Calculate Charlson Comorbidity Index for each visit
cci <- comorbidity(x = fkrtl_sec, id = "visitId", code = "icd2", score = "charlson", icd = "icd10", assign0 = FALSE)

## Populate Ranap and exclude referral
df1 <- fkrtl1 %>% 
  filter(layanan == "RITL" & 
           status != "Rujuk" & 
           kelasTindakan != "Rawat Inap Kebidanan" & 
           kelasTindakan != "Rawat Inap Neonatal" & 
           outDate > inDate) %>% 
  mutate(year = lubridate::year(inDate))

## Calculate LOS and remove inhospital stay less than 1 days
df1$los <- as.integer(ymd(df1$outDate)-ymd(df1$inDate))
df2 <- df1 %>% arrange(id, inDate) %>% 
  distinct(visitId, .keep_all = TRUE) %>% 
  group_by(id) %>% 
  mutate(interval = as.integer(inDate - lag(outDate))) %>% 
  ungroup() %>% 
  mutate(readmission = NA)

## Labelize
df2$readmission <- if_else(is.na(df2$interval), "I", 
                                 if_else(df2$interval < 0, "", 
                                         if_else(df2$interval > 30, "I", 
                                                 if_else(df2$interval <= 30 & df2$interval >= 0 & df2$icd1 == lag(df2$icd1), "UR", "OR"))))

df3 <- df2 %>% group_by(id) %>% filter(readmission != "") %>% 
  mutate(readmission2 = lead(readmission),
         label = if_else(readmission == "I" & is.na(readmission2), "I",
                         if_else(readmission2 %in% c("OR", "I"), "I",
                                 if_else(readmission2 == "UR", "R", "")))) %>% 
  slice(1:(n()-1)) %>% ungroup()

df3$label <- as.factor(df3$label)


## Join with Peserta, calculate age and discretize into ageGroups
df <- df3 %>% 
  left_join(peserta1, by = "id") %>% 
  mutate(age = as.integer(year(ymd(inDate))-year(ymd(dob))),
         ageGroup = cut(age, breaks=c(-1,5,11,25,45,65,120), 
                        labels = c("Balita (0-5)", "Anak-anak (6-11)", "Remaja (12-25)", "Dewasa (26-45)", "Lansia (56-65)", "Manula (>65)")
                        )
         )

## Divide into Not readmitted and readmitted
#df$readmit <- as.factor(if_else(df$readmission == "UR", "Readmission", "Not readmission"))

## Discretize age into groups
#df <- df3 %>% mutate(losGroup = cut(los, breaks = c(1,6,11,21,500), labels = c("1-5", "6-10", "11-20", ">20"), include.lowest = TRUE, right = FALSE)) %>% arrange(PSTV01, FKL03)

## Make NA explicit
# df$rstype <- fct_explicit_na(df$rstype, na_level = "Missing")
# df$severity <- fct_explicit_na(df$severity, na_level = "Missing")
# df$kelasrawat <- fct_explicit_na(df$kelasrawat, na_level = "Missing")
# df$rsowner <- fct_explicit_na(df$rsowner, na_level = "Missing")

# Summary statistics
df_summ <- df %>% select(age, gender, status, label)
tbl_summary(df_summ, by = label) %>% add_n()

# Propensity Score Matching
library(MatchIt)
df_match <- matchit(label ~ age + gender, data = df, ratio = 3)
data_match <- match.data(df_match)

## Try predict something
# Split matched data into training and testing dataset
library(tidymodels)
df_split <- initial_split(data_match, prop = 0.80)
df_train <- training(df_split)
df_test <- testing(df_split)

