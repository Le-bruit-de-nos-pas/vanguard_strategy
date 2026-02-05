library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
options(scipen = 999)

# Vector with all +18 Cancer patients + Metastatis and Cachexia Diagnosis Status

PONS_Demographics <- fread("PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics %>% select(patid, weight, age, died, cancer_metastasis, cachexia_onset)

PONS_Demographics <- PONS_Demographics %>% filter(age >= 18)

PONS_Demographics <- PONS_Demographics %>% mutate(cancer_metastasis = ifelse(is.na(cancer_metastasis),0,1))


# Vector with cancers of interest (i.e. excluding benign tumors and malignant but minor skin cancers)

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep = "\t")

PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)

names(PONS_Demographics)[4] <- "diagnosis"


# Final vector of patients to track

Pats_to_track_BMI <- PONS_Demographics  %>% select(patid, weight, diagnosis, died,  cancer_metastasis, cachexia_onset)

Pats_to_track_BMI <- Pats_to_track_BMI %>% filter(diagnosis != "-") 

Pats_to_track_BMI <- Pats_to_track_BMI %>% select(patid, weight, diagnosis, cancer_metastasis, cachexia_onset)

Pats_to_track_BMI$cachexia_onset <- as.Date(Pats_to_track_BMI$cachexia_onset)


# BMI and weight records (using BMI)

PONS_Measures <- fread("PONS_Measures_short.txt", sep = "\t")

PONS_Measures <- PONS_Measures %>% filter(test == "BMI")


# Filter for patients of interest

PONS_Measures <- PONS_Measures %>% select(-weight) %>% inner_join(Pats_to_track_BMI %>% select(patid, weight))


# Calculate mean and median for each patient

Summary_vals_pats <- PONS_Measures %>% group_by(patid) %>% summarise(mean = mean(value), median = median(value))

PONS_Measures <- PONS_Measures %>% left_join(Summary_vals_pats)

PONS_Measures <- PONS_Measures %>% arrange(patid, claimed)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)



# For each patient, remove any values that deviate more than 50% above/below that patient's median

PONS_Measures <- PONS_Measures %>% ungroup() %>% filter(value < 1.5*median & value > 0.5*median) 


# Calculate the mean and median again (also fetch the min and max BMI records for each patient)

Summary_vals_pats <- PONS_Measures %>% ungroup() %>% group_by(patid) %>% 
  summarise(mean = mean(value), median = median(value), min = min(value), max = max(value))

PONS_Measures <- PONS_Measures %>% select(-c(mean, median)) %>% left_join(Summary_vals_pats)


# For each patient, see when the min and max values have been reached for the first time

Min_Max_Dates <- PONS_Measures %>% ungroup() %>% filter(value == min) %>% mutate(mindate = claimed) %>% select(patid, claimed, mindate) %>% 
  full_join(PONS_Measures %>% ungroup() %>% filter(value == max) %>% mutate(maxdate = claimed) %>% select(patid, claimed, maxdate),
            by = "patid") %>% select(patid, mindate, maxdate)

Min_Max_Dates <- Min_Max_Dates %>% distinct()

PONS_Measures <- PONS_Measures %>% left_join(Min_Max_Dates) %>% select(-c(test, mean, median))

PONS_Measures$mindate <- as.Date(PONS_Measures$mindate)

PONS_Measures$maxdate <- as.Date(PONS_Measures$maxdate)



# Filter for patients that have >10 distinct BMI records

PONS_Measures <- PONS_Measures %>% select(patid, weight) %>% group_by(patid) %>% count() %>% 
  filter(n >= 10) %>% select(patid) %>% inner_join(PONS_Measures)


# Exclude patients currently in remission 

PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep = "\t")

PONS_Measures <- PONS_Demographics %>% 
  filter(Exact_Month == 60 & (Status == "Earliest" | Status == "Metastasis" | Status == "Death")) %>% 
  select(patid) %>% inner_join(PONS_Measures)
PONS_Measures <- PONS_Measures %>% select(-weight)



# Convert BMI records to wide format

temp <- PONS_Measures

temp <- temp %>% select(patid, claimed, value)


# Convert month (1 through 60) to their exact corresponding date (e.g., "2019-08")

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- paste0(Months_lookup$Month,"-1")

Months_lookup$Month <- as.Date(Months_lookup$Month)

Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

Months_lookup$Month <- as.character(Months_lookup$Month)

temp <- temp %>% mutate(claimed = as.character(claimed))

temp <- temp %>% mutate(claimed = str_sub(claimed, 1L, 7L))

temp <- temp %>% left_join(Months_lookup, by = c("claimed" = "Month")) %>% select(patid, value, Exact_Month) %>% distinct()



# For each patient, calculate the MAX BMI and MIN BMI within each Month (in case that patient happens to have +1 distinct records)

temp_max <- temp %>% group_by(patid, Exact_Month) %>% summarise(n = max(value))

temp_min <- temp %>% group_by(patid, Exact_Month) %>% summarise(n = min(value))

# Create 2 "wide" tables (60 Months) with the MAX/MIN BMI in each month per patient

temp_max <- temp_max %>% ungroup() %>% spread(key = Exact_Month, value = n)

temp_min <- temp_min %>% ungroup() %>% spread(key = Exact_Month, value = n)


fwrite(temp_max, "MAX_Cachexia_BMI_Wide.txt", sep = "\t")
fwrite(temp_min, "MIN_Cachexia_BMI_Wide.txt", sep = "\t")

temp_max <- fread("MAX_Cachexia_BMI_Wide.txt", sep = "\t", header = T)
temp_min <- fread("MIN_Cachexia_BMI_Wide.txt", sep = "\t", header = T)


# Pool the 2 tables together (i.e., For each patient, for each Month, compare that Month's MAX BMI with all following MIN BMIs/Months)

temp_max <- melt(temp_max) %>% drop_na() %>% arrange(patid)

names(temp_max)[2] <- "Month_Max"

names(temp_max)[3] <- "Max"

temp_max$Month_Max <- as.numeric(temp_max$Month_Max)

temp_min <- melt(temp_min) %>% drop_na() %>% arrange(patid)

names(temp_min)[2] <- "Month_Min"

names(temp_min)[3] <- "Min"

temp_min$Month_Min <- as.numeric(temp_min$Month_Min)

temp <- temp_max %>% left_join(temp_min)

temp <- temp %>% ungroup() %>% filter(Month_Min > Month_Max)


# BMI drops respecting the current cachexia criteria are:
#    -> Drops +5% within 6 months
#    -> Drops +10% within 12 months
#    -> Drops +2% ever resulting in a BMI<20
# Only drops still observed in the last 12 months are accepted

temp <- temp %>% mutate(Drop95 = ifelse( (Min < (Max*0.95)) & (Month_Min > Month_Max) & (Month_Min - Month_Max <= 6) & (Month_Min >= 49),1,0 ))
temp <- temp %>% mutate(Drop90 = ifelse( (Min < (Max*0.90)) & (Month_Min > Month_Max) & (Month_Min - Month_Max <= 12) & (Month_Min >= 49),1,0 ))
temp <- temp %>% mutate(Drop2_20 = ifelse( (Min < (Max*0.98)) & (Month_Min > Month_Max) & (Min < 20) & (Month_Min >= 49),1,0 ))

# A potentially cachexic patient is anyone meeting any of the above 3 criteria (plus a formal Cachexia Diagnosis, see below)

New_Cachexia_Pred <- temp %>% filter(Drop95 == 1 | Drop90 == 1 | Drop2_20 == 1) %>% select(patid) %>% distinct()



# If making a version where the +5% BMI drop criterion is now considered
# New_Cachexia_Pred <- temp %>% filter( Drop90==1 | Drop2_20==1) %>% select(patid) %>% distinct() 


# Project the patients meeting the above criteria, stratified by Cancer Diagnosis and Metastatic state
New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)  #  %>% ungroup() %>%  summarise(n=sum(POP_Mets)) # 1524333


# Include also those with a formal Cacheixa Diagnosis in the past year

data.frame(New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
             full_join(Pats_to_track_BMI %>% filter(cachexia_onset >= "2020-08-01")) %>%
             distinct() %>% 
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)  # %>% ungroup()) %>% summarise(n=sum(POP_Mets))  # 1603718


# Store a vector with all cachexia patients (predicted or formally diagnosed)

# CachexiaPats_ALL_NEW <- New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
#              full_join(Pats_to_track_BMI %>% filter(cachexia_onset>="2020-08-01")) %>%
#              distinct() %>% select(patid) %>% distinct()
# 
# fwrite(CachexiaPats_ALL_NEW, "CachexiaPats_ALL_NEW.txt")
# 
# CachexiaPats_ALL_NEW <- fread("CachexiaPats_ALL_NEW.txt")







# With the code above one is meant to highlight the proportion of those patient sof interest with BMI records that can be cachexic
# This proportion can be further reducted:
#                              -> (e.g., removing those with BMI>30 )
#                              -> (e.g., not considering the +5% criterion)


# Filter for patients with BMI < 30

PONS_Measures <- fread("PONS_Measures_short.txt", sep="\t")

PONS_Measures <- PONS_Measures %>% filter(test=="BMI")

PONS_Measures <- New_Cachexia_Pred %>% left_join(PONS_Measures) %>% select(patid, claimed, value)

PONS_Measures$claimed <- as.Date(PONS_Measures$claimed)

PONS_Measures <- PONS_Measures %>% group_by(patid) %>%  filter(value==min(value))

Pats_BMI_30 <- PONS_Measures %>% ungroup() %>% filter(value>=30) %>% select(patid) %>% distinct()

data.frame(New_Cachexia_Pred %>% left_join(Pats_to_track_BMI) %>%
             full_join(Pats_to_track_BMI %>% filter(cachexia_onset>="2020-08-01")) %>%
             distinct() %>%  anti_join(Pats_BMI_30) %>%
  group_by(diagnosis,cancer_metastasis) %>% summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis)  ) #  %>% ungroup() %>% summarise(n=sum(POP_Mets)) # 1289971




# We have then defined where to applied these proportion 
# (i.e., the target total cancer population of interest, including those without BMI records)
# Our target population were patients diagnosed only in the past 3 years


# Determine who has has been newly diagnosed in the past 3 years

PONS_Demographics <- fread("PONS_Time_Series_Groups.txt", sep="\t")

Active_Last3y <- PONS_Demographics %>% filter(Exact_Month>=25) %>% 
  filter(Status=="Earliest"|Status=="Metastasis") %>% select(patid) %>% distinct()

NaiveFirst3y <- PONS_Demographics %>% filter(Exact_Month==24) %>% 
  filter(Status=="Naive") %>% select(patid) %>% distinct()

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

Pats_to_track_BMI <- Active_Last3y %>% inner_join(NaiveFirst3y) %>% inner_join(Pats_to_track_BMI)  

sum(Pats_to_track_BMI$weight) # 6591459

data.frame(Pats_to_track_BMI %>%  ungroup() %>% group_by(diagnosis,cancer_metastasis) %>% anti_join(Pats_BMI_30) %>%
             summarise(POP_Mets=sum(weight)) %>%
             arrange(diagnosis, -cancer_metastasis))







# If wanting to filter for treated patients (not done!)

# Determine who has been treated with anticancer drugs in the past 2 years

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer!="-")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer)

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")

PONS_Ingredients <- fread("PONS Ingredients.txt", integer64 = "character", stringsAsFactors = F)

PONS_Ingredients <- PONS_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

PONS_Ingredients <- PONS_Ingredients %>% select(molecule, drug_class)

PONS_Ingredients$molecule <- as.numeric(PONS_Ingredients$molecule)

CAN_Drug_Histories <- New_Primary_Cancer_Box %>% select(patid, Primary_Cancer) %>% left_join(CAN_Drug_Histories, by=c("patid"="patient"))
sum(CAN_Drug_Histories$weight)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, Primary_Cancer, weight, month1:month60)

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

CAN_Drug_Histories$Month <- as.character(CAN_Drug_Histories$Month)

CAN_Drug_Histories$Month <- parse_number(CAN_Drug_Histories$Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=37)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Drugs!="-")

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Drugs, sep = ",", convert=T)

names(CAN_Drug_Histories)[5] <- "molecule"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(drug_class=="Chemotherapy"|
                                drug_class=="Biologic Therapy"|
                                drug_class=="Radiotherapy"|
                                drug_class=="GDF15"|
                                drug_class=="Surgery Inpatient") %>% select(patid) %>% distinct()

Treat_Last_2Years <- CAN_Drug_Histories
