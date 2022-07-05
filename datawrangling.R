## Load library
library(tidyverse)
library(haven)
library(lubridate)
library(comorbidity)
library(gtsummary)
library(skimr)

## Load data
load("~/Documents/Projects/Personal - Readmission Study/data.RData")
fkrtl_sec$id <- as.numeric(1:nrow(fkrtl_sec))
fkrtl$PSTV01 <- as.character(fkrtl$PSTV01)
fkrtl$FKL02 <- as.character(fkrtl$FKL02)
peserta$PSTV01 <- as.character(peserta$PSTV01)

# Calculate Charlson Comorbidity Index for each visit
cci <- comorbidity(x = fkrtl_sec, id = "id", code = "FKL24", map = "charlson_icd10_quan", assign0 = FALSE)
cci <- full_join(fkrtl_sec, cci, by = "id")

## Populate Ranap and exclude referral
df1 <- fkrtl %>% 
  filter(FKL10 == "RITL" & 
           FKL14 != "Rujuk" & 
           FKL21 != "Rawat Inap Kebidanan" & 
           FKL21 != "Rawat Inap Neonatal" & 
           FKL04 > FKL03) %>% 
  mutate(year = lubridate::year(FKL03))

## Calculate LOS and remove inhospital stay less than 1 days
df1$los <- as.integer(ymd(df1$FKL04)-ymd(df1$FKL03))
df2 <- df1 %>% arrange(PSTV01, FKL03) %>% 
  distinct(FKL02, .keep_all = TRUE) %>% 
  group_by(PSTV01) %>% 
  mutate(interval = as.integer(FKL03 - lag(FKL04))) %>% 
  ungroup() %>% 
  mutate(readmission = NA)

## Labelize
df2$readmission <- if_else(is.na(df2$interval), "I", 
                                 if_else(df2$interval < 0, "", 
                                         if_else(df2$interval > 30, "I", 
                                                 if_else(df2$interval <= 30 & df2$interval >= 0 & df2$FKL18 == lag(df2$FKL18), "UR", "OR"))))

df3 <- df2 %>% group_by(PSTV01) %>% filter(readmission != "") %>% 
  mutate(readmission2 = lead(readmission),
         label = if_else(readmission == "I" & is.na(readmission2), "I",
                         if_else(readmission2 %in% c("OR", "I"), "I",
                                 if_else(readmission2 == "UR", "R", "")))) %>% 
  slice(1:(n()-1)) %>% 
  ungroup() %>% 
  left_join(cci, by = "FKL02") %>% 
  mutate_at(vars(mi:aids), ~replace_na(., 0))

df3$label <- as.factor(df3$label)

## Join with Peserta, calculate age and discretize into ageGroups
df <- df3 %>% 
  left_join(peserta, by = "PSTV01") %>% 
  mutate(age = as.integer(year(ymd(FKL03))-year(ymd(PSTV03))),
         ageGroup = cut(age, breaks=c(-1,5,11,25,45,65,120), 
                        labels = c("Balita (0-5)", "Anak-anak (6-11)", "Remaja (12-25)", "Dewasa (26-45)", "Lansia (56-65)", "Manula (>65)")
                        )
         ) %>% select(!c(FKL03, FKL04, FKL10, FKL14, interval, readmission, readmission2, FKL24, id, PSTV03)) %>% 
  rename(id = PSTV01,
         visitID = FKL02,
         provinsi = FKL05,
         distrik = FKL06,
         golPeserta = FKL12,
         kelasRawat = FKL13,
         icd0 = FKL16,
         icd1 = FKL18,
         kelPenyakit = FKL20,
         kelasProsedur = FKL21,
         severity = FKL23,
         cost = FKL48,
         gender = PSTV05,
         weight = PSTV15) %>% 
  filter(age >= 17, icd1 != "Z511") %>% 
  mutate(icd1 = str_sub(icd1, 1, 3))

data <- df %>% select(golPeserta, kelasRawat, severity, gender, kelasProsedur, los, age, ageGroup, label, mi:aids)

# Summary statistics
df_summ <- df %>% select(icd1)
tbl_summary(df_summ, by = label) %>% add_n()
count(df, icd1) %>% arrange(-n)

df_diab <- df %>% filter(icd1 == "E11")
df_typhoid <- df %>% filter(icd1 == "A01")
# Propensity Score Matching
library(MatchIt)
df_match <- matchit(label ~ age + gender, data = data, ratio = 1)
data_match <- match.data(df_match)

write_csv(data, "~/Documents/Projects/Personal - Readmission Study/full dataset.csv")
write_csv(data_match, "~/Documents/Projects/Personal - Readmission Study/undersampling dataset.csv")
