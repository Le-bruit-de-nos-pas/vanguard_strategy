
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)


#  "Pats_to_track_BMI" corresponds to a vector of cancer patients of interest to track to be defined above,
#   including the patient ID, the projection weight, age, cancer type, metastasis status and clinical cachexia diagnosis status
#   A truncated version of the "PONS_Measures_short.txt" file will be attached
#   The "Months_lookup.txt" will also be attached





# BMI and weight records (using BMI)  [weight: projection weight,  value: BMI value]

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


# If also wanting to remove small daily/weekly/month fluctuations

# PONS_Measures <- PONS_Measures %>% group_by(patid) %>% mutate(TimeElapsed = 1 + as.numeric(claimed) - as.numeric(lag(claimed)))
# 
# PONS_Measures <- PONS_Measures %>% group_by(patid) %>% mutate(Difference = (100 * (value - lag(value)) / lag(value ))) %>%
#   mutate(DifferencePerDay = (100 * (value-lag(value)) / lag(value )) / TimeElapsed )


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

