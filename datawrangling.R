## Load library
library(tidyverse)
#library(psych)
library(lubridate)
# library(survey)
# library(eeptools)
# library(sjmisc)
# library(forcats)
library(comorbidity)

## Load data
load("data1.RData")
fkrtl$PSTV01 <- as.character(fkrtl$PSTV01)
peserta$PSTV01 <- as.character(peserta$PSTV01)
fkrtl$FKL02 <- as.character(fkrtl$FKL02)
peserta <- peserta %>% select(PSTV01, PSTV03, PSTV05, PSTV07, PSTV08, PSTV13, PSTV14)

# Calculate Charlson Comorbidity Index for each visit
cci <- comorbidity(x = fkrtl_sec, id = "FKL02", code = "FKL24", score = "charlson", icd = "icd10", assign0 = FALSE)

## Populate Ranap and exclude referral
df1 <- fkrtl %>% filter(FKL10 == "RITL" & FKL14 != "Rujuk" & FKL21 != "Rawat Inap Kebidanan" & FKL21 != "Rawat Inap Neonatal" & FKL04 > FKL03) %>% mutate(year = lubridate::year(FKL03))

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

## Join with Peserta, calculate age and discretize into ageGroups
df3 <- df2 %>% 
  filter(readmission != "") %>% 
  left_join(peserta, by=c("PSTV01")) %>% 
  mutate(age = as.integer(year(ymd(FKL03))-year(ymd(PSTV03))),
         ageGroup = cut(age, breaks=c(-1,5,11,25,45,65,120), 
                        labels = c("Balita (0-5)", "Anak-anak (6-11)", "Remaja (12-25)", "Dewasa (26-45)", "Lansia (56-65)", "Manula (>65)")
                        )
         )

## Discretize age into groups
df <- df3 %>% mutate(losGroup = cut(los, breaks = c(1,6,11,21,500), labels = c("1-5", "6-10", "11-20", ">20"), include.lowest = TRUE, right = FALSE)) %>% arrange(PSTV01, FKL03)


## Renames
#setnames(df, old = c("PSTV01", "PSTV03", "PSTV06", "PSTV05", "PSTV07", "FKL02", "FKL03", "FKL04", "FKL05", "FKL06", "FKL07", "FKL09", "FKL13", "FKL14", "FKL16A", "FKL18", "FKL20", "FKL21", "FKL23", "FKL30", "FKL32", "PSTV15", "FKL24"), new = c("ID", "DOB", "marital", "gender", "kelaspeserta", "visitID", "inDate", "outDate", "provinsi", "distrik", "rsowner", "rstype", "kelasrawat", "statusPulang", "dxmasuk", "icd_1", "diseasegroup", "classprocedure", "severity", "procedure", "cost", "weight", "icd_2"))

df <- df %>% rename(ID = PSTV01,
                    DOB = PSTV03,
                    gender = PSTV05,
                    marital = PSTV06,
                    kelaspeserta = PSTV07,
                    visitID = FKL02,
                    inDate = FKL03,
                    outDate = FKL04,
                    provinsi = FKL05,
                    distrik = FKL06,
                    rsowner = FKL07,
                    rstype = FKL09,
                    kelasrawat = FKL13,
                    statusPulang = FKL14,
                    dxmasuk = FKL16A,
                    icd_1 = FKL18,
                    icd_2 = FKL24,
                    diseasegroup = FKL20,
                    classprocedure = FKL21,
                    severity = FKL23,
                    procedure = FKL30,
                    cost = FKL32,
                    weight = PSTV15)
## Make NA explicit
# df$rstype <- fct_explicit_na(df$rstype, na_level = "Missing")
# df$severity <- fct_explicit_na(df$severity, na_level = "Missing")
# df$kelasrawat <- fct_explicit_na(df$kelasrawat, na_level = "Missing")
# df$rsowner <- fct_explicit_na(df$rsowner, na_level = "Missing")

## Get Index admissions
df_predict <- df %>% select(ID, visitID, inDate, provinsi, distrik, rsowner, rstype, kelasrawat, statusPulang, icd_1, icd_2, severity, classprocedure, procedure, cost, weight, los, losGroup, year, gender, age, ageGroup, marital, kelaspeserta, membership, readmission, icd_2_class) %>% arrange(ID, inDate) %>% group_by(ID) %>% mutate(target = lead(readmission), re_cost = lead(cost), re_statusPulang = lead(statusPulang)) %>% ungroup()
df_predict$icd_1_class <- str_sub(df_predict$icd_1, 1, 3)
df_predict$dx <- paste(df_predict$icd_1, df_predict$icd_2, sep = ",")
df_predict$dxclass <- paste(df_predict$icd_1_class, df_predict$icd_2_class, sep = ",")
df_predict$target <- fct_explicit_na(df_predict$target, na_level = "None")
df_predict$target <- if_else((df_predict$target == "None" & df_predict$readmission == "I"), "I", as.character(df_predict$target))
df_predict$outcome <- if_else((df_predict$statusPulang == "MENINGGAL"), "Dead", "Live")
df_predict$re_outcome <- if_else((df_predict$re_statusPulang == "MENINGGAL"), "Dead", "Live")
df_predict$classprocedure <- if_else((df_predict$classprocedure == "Prosedur Rawat Inap"), "prosedur", "nonprosedur")
df_predict$severity <- sub("\\s.*", "", df_predict$severity)
df_index <- df_predict %>% filter(readmission == "I") %>% select(-visitID)
df_index$label <- if_else((df_index$target == "UR"), "R", "I")
df_index$re_outcome2 <- if_else((df_index$label == "R" & df_index$re_outcome == "Dead") | df_index$outcome == "Dead", "Dead", "Live")
df_index$region <- if_else(grepl(paste(c("SUMATERA", "JAWA", "BANTEN", "KALIMANTAN TENGAH", "KALIMANTAN BARAT", "JAKARTA", "YOGYAKARTA", "RIAU", "JAMBI", "LAMPUNG", "BENGKULU", "BANGKA BELITUNG", "ACEH"), collapse = "|"), df_index$provinsi), "barat", if_else(grepl(paste(c("KALIMANTAN SELATAN", "KALIMANTAN TIMUR", "KALIMANTAN UTARA", "NUSA TENGGARA", "BALI", "SULAWESI", "GORONTALO"), collapse = "|"), df_index$provinsi), "tengah", "timur"))
df_index <- df_index %>% select(-re_outcome, -outcome, -statusPulang, -re_statusPulang, -readmission, -icd_2_class, -icd_2) %>% rename(outcome = re_outcome2)

#mutate(comorbid = if_else(is.na(icd_2), "0", if_else(str_count(df_index$icd_2, '[A-Z.]+') == 1, "1", if_else(str_count(df_index$icd_2, '[A-Z.]+') == 2, "2", if_else(str_count(df_index$icd_2, '[A-Z.]+') >= 3, ">=3", "0")))))

# Export
write_csv(df_index, "data_v6.csv")
