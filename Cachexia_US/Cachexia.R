#              ************************  CACHEXIA   *****************************   

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)

# How many have cancer with BMIs at least 2x 6m apart ?  Diabetes --------------

DIA_Disorder_Histories <- fread("DIA Disorder Histories.txt")
DIA_Disorder_Histories <- DIA_Disorder_Histories %>% select(patient, weight, month60)

sum(DIA_Disorder_Histories$weight) # 48244424

DIA_Disorder_Histories %>% filter(grepl("c", month60)|grepl("C", month60)) %>% summarise(n=sum(weight)) # 7815173 (16%)

Diabetes_Cancer_History <- DIA_Disorder_Histories %>% mutate(CancerStatus = ifelse(grepl("c", month60)|grepl("C", month60), "Cancer", "no"))

Diabetes_Cancer_History <- Diabetes_Cancer_History %>% select(-month60)

DANU_Measures <- fread("DANU Measures.txt") # From NASH dataset

DANU_Measures <- DANU_Measures %>% filter(test == "BMI")
DANU_Measures <- DANU_Measures %>% select(patid, test, claimed, value)

Diabetes_Cancer_History <- Diabetes_Cancer_History %>% left_join(DANU_Measures, by=c("patient"="patid"))

Diabetes_Cancer_History %>% select(patient, weight, CancerStatus, test) %>% distinct() %>% group_by(CancerStatus, test) %>% summarise(n=sum(weight))

Diabetes_Cancer_History <- Diabetes_Cancer_History %>% drop_na()

Diabetes_Cancer_History <- Diabetes_Cancer_History %>% group_by(patient) %>% filter(claimed == min(claimed) | claimed == max(claimed)) %>%
  filter(row_number() == 1 | row_number() == n())

to_keep_2plusBMI <- Diabetes_Cancer_History %>% group_by(patient) %>% count() %>% filter(n!=1) %>% select(patient)

Diabetes_Cancer_History <- to_keep_2plusBMI %>% left_join(Diabetes_Cancer_History)

Diabetes_Cancer_History %>% filter(CancerStatus=="Cancer" ) %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight))

# Cancer with 2plus BMIS = 4281209 (87% of cancer with BMI, 55% of cancer, 9% of all diabetic)

Diabetes_Cancer_History <- Diabetes_Cancer_History %>% filter(CancerStatus=="Cancer" )

Diabetes_Cancer_History$claimed <- as.Date(Diabetes_Cancer_History$claimed)

Diabetes_Cancer_History %>% group_by(patient) %>% mutate(ElapsedTime = as.numeric(claimed - lag(claimed))/30.5) %>% 
  filter(!is.na(ElapsedTime)) %>% filter(ElapsedTime > 6) %>% ungroup() %>% summarise(n=sum(weight))


Diabetes_Cancer_History %>% group_by(patient) %>% mutate(BMIdiff = as.numeric(value - lag(value))) %>% filter(!is.na(BMIdiff)) %>%
  ungroup() %>% summarise(n=range(BMIdiff))



# How many have cancer with BMIs at least 2x 6m apart ?  Obesity --------------


OBE_Disorder_Histories <- fread("OBE Disorder Histories.txt")
OBE_Disorder_Histories <- OBE_Disorder_Histories %>% select(patient, weight, month60)

sum(OBE_Disorder_Histories$weight) # 106469049

OBE_Disorder_Histories %>% filter(grepl("c", month60)|grepl("C", month60)) %>% summarise(n=sum(weight)) # 8951507 (%)

OBE_Cancer_History <- OBE_Disorder_Histories %>% mutate(CancerStatus = ifelse(grepl("c", month60)|grepl("C", month60), "Cancer", "no"))

OBE_Cancer_History <- OBE_Cancer_History %>% select(-month60)

DANU_Measures <- fread("DANU Measures.txt") # From NASH dataset

DANU_Measures <- DANU_Measures %>% filter(test == "BMI")
DANU_Measures <- DANU_Measures %>% select(patid, test, claimed, value)

Obesity_Cancer_History <- OBE_Cancer_History %>% left_join(DANU_Measures, by=c("patient"="patid"))

Obesity_Cancer_History %>% select(patient, weight, CancerStatus, test) %>% distinct() %>% group_by(CancerStatus, test) %>% summarise(n=sum(weight))


Obesity_Cancer_History <- Obesity_Cancer_History %>% drop_na()

Obesity_Cancer_History <- Obesity_Cancer_History %>% group_by(patient) %>% filter(claimed == min(claimed) | claimed == max(claimed)) %>%
  filter(row_number() == 1 | row_number() == n())

to_keep_2plusBMI <- Obesity_Cancer_History %>% group_by(patient) %>% count() %>% filter(n!=1) %>% select(patient)

Obesity_Cancer_History <- to_keep_2plusBMI %>% left_join(Obesity_Cancer_History)

Obesity_Cancer_History %>% filter(CancerStatus=="Cancer" ) %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight))


Obesity_Cancer_History <- Obesity_Cancer_History %>% filter(CancerStatus=="Cancer" )

Obesity_Cancer_History$claimed <- as.Date(Obesity_Cancer_History$claimed)

Obesity_Cancer_History %>% group_by(patient) %>% mutate(ElapsedTime = as.numeric(claimed - lag(claimed))/30.5) %>% 
  filter(!is.na(ElapsedTime)) %>% filter(ElapsedTime > 6) %>% ungroup() %>% summarise(n=sum(weight))


Obesity_Cancer_History <- Obesity_Cancer_History %>% group_by(patient) %>% mutate(ElapsedTime = as.numeric(claimed - lag(claimed))/30.5)

Obesity_Cancer_History %>% group_by(patient) %>% mutate(BMIdiff = as.numeric(value - lag(value))) %>% filter(!is.na(BMIdiff)) %>%
  ungroup() %>% summarise(n=median(BMIdiff))

# ---------
# ---------
# Check BMI distribution across types of cancer ----------------

DIA_OBE_pts_CanDx <- fread("DIA_OBE_pts_CanDx.txt")

unique(DIA_OBE_pts_CanDx$condition)

#

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(condition != "Benign Tumor")
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(condition != "Cancer History")

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(-c(encoding, icd_flag))

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% mutate(fst_dt = as.Date(fst_dt))

min(DIA_OBE_pts_CanDx$fst_dt) # "2015-10-15"
max(DIA_OBE_pts_CanDx$fst_dt) # "2021-09-30"

DIA_OBE_pts_CanDx %>% group_by(ptid) %>% filter(fst_dt == min(fst_dt)) %>% slice(1)

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(ptid, fst_dt, condition)

names(DIA_OBE_pts_CanDx)[1] <- "patient"
names(DIA_OBE_pts_CanDx)[2] <- "Date"

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% group_by(patient) %>% filter(Date == min(Date)) %>% slice(1)

# Get BMIs
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       

DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

names(DANU_Events)[1] <- "patient"
names(DANU_Events)[3] <- "BMI"
names(DANU_Events)[4] <- "Date"

DANU_Events$Date <- as.Date(DANU_Events$Date)

names(DANU_Events)[4] <- "Date_BMI"

length(unique((DIA_OBE_pts_CanDx$patient))) # 118180 with Cancer, exc. those 2 types

temp <- DIA_OBE_pts_CanDx  %>% inner_join(DANU_Events, by=c("patient"="patient"))

length(unique((temp$patient))) # 80266 with BMIs


data.frame(temp %>% group_by(condition) %>% summarise(n=weighted.mean(BMI, weight, na.rm=T)) %>% arrange(n))

BMIs_After_FirstDx <- temp %>% group_by(patient) %>% filter(Date_BMI>=Date)

data.frame(BMIs_After_FirstDx %>% group_by(condition) %>% summarise(n=weighted.mean(BMI, weight, na.rm=T)) %>% arrange(n))



BMIs_After_FirstDx <- BMIs_After_FirstDx %>%  ungroup() %>% mutate(ElapsedTime = as.numeric(round((Date_BMI-Date)/30.5)))

BMIs_After_FirstDx %>% 
  filter(condition== "Reproductive Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Reproductive Cancer diagnosis")


BMIs_After_FirstDx <- BMIs_After_FirstDx %>% group_by(patient) %>% mutate(BMI_diff = (BMI/max(BMI))*100)

MaxDate <- BMIs_After_FirstDx %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) %>% slice(1) 

names(MaxDate)[3] <- "Date_MAX_BMI"

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% left_join(MaxDate , by=c("patient"="patient")) %>% select(-BMI.y)

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% group_by(patient) %>% mutate(ElapsedTimeMAX =  as.numeric(round((Date_BMI-Date_MAX_BMI)/30.5)))

# (....)

temp3 <- BMIs_After_FirstDx %>% filter(BMI_diff < 95 & ElapsedTimeMAX < 12 & ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

Percent_Ever_below95 <- Percent_Ever_below95 %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMax = n/TOTAL)

names(Percent_Ever_below95)[5] <- "Below95_12mfromFIRSTmax"


temp3 <- BMIs_After_FirstDx %>% filter(BMI_diff < 95 & ElapsedTimeMAX < 6 & ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

Percent_Ever_below95 <- Percent_Ever_below95 %>% left_join(temp3) %>% mutate(Percentage_6m_FirstMax = n/TOTAL)

names(Percent_Ever_below95)[7] <- "Below95_6mfromFIRSTmax"


temp3 <- BMIs_After_FirstDx %>% filter(BMI_diff < 90) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

Percent_Ever_below95 <- Percent_Ever_below95 %>% left_join(temp3) %>% mutate(Percentage_Below90ever = n/TOTAL)

names(Percent_Ever_below95)[9] <- "Below90ever"

fwrite(Percent_Ever_below95, "Percent_Ever_below95.txt", sep="\t")


MaxDateALL <- BMIs_After_FirstDx %>% group_by(patient) %>% select(patient, BMI.x, Date_BMI) %>% filter(BMI.x == max(BMI.x)) 

names(MaxDateALL)[3] <- "Date_MAX_BMI_ALL"

MaxDateALL$NewDate <- MaxDateALL$Date_MAX_BMI_ALL

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% left_join(MaxDateALL, by=c("patient"="patient", "Date_BMI"="Date_MAX_BMI_ALL")) %>% select(-c(BMI.x.y))

names(BMIs_After_FirstDx)[5] <- "BMI"

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% group_by(patient) %>% fill(NewDate, .direction = "down")

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% group_by(patient) %>% mutate(New_ElapsedTimeMAX =  as.numeric(round((Date_BMI-NewDate)/30.5)))



temp3 <- BMIs_After_FirstDx %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

Percent_Ever_below95 <- Percent_Ever_below95 %>% left_join(temp3) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(Percent_Ever_below95)[11] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- BMIs_After_FirstDx %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

Percent_Ever_below95 <- Percent_Ever_below95 %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(Percent_Ever_below95)[13] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(Percent_Ever_below95, "Percent_Ever_below95.txt", sep="\t")

# -------
# Select patients that have had no Cancer Dx within the first 12 months -----------

fwrite(BMIs_After_FirstDx, "BMIs_After_FirstDx.txt", sep="\t")

Pats_Cancer_First12m <- BMIs_After_FirstDx %>% ungroup() %>% filter(Date<"2016-10-15") %>% select(patient) %>% distinct()

Pats_Cancer_First12m <- BMIs_After_FirstDx %>% anti_join(Pats_Cancer_First12m)

length(unique(Pats_Cancer_First12m$patient)) #30168

Percent_Ever_below95_NewCancer <- Pats_Cancer_First12m %>% select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()


temp3 <- Pats_Cancer_First12m %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

names(Percent_Ever_below95_NewCancer)[2] <- "TOTAL"

Percent_Ever_below95_NewCancer <- Percent_Ever_below95_NewCancer %>% left_join(temp3) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(Percent_Ever_below95_NewCancer)[3] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- Pats_Cancer_First12m %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

Percent_Ever_below95_NewCancer <- Percent_Ever_below95_NewCancer %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(Percent_Ever_below95_NewCancer)[5] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(Percent_Ever_below95_NewCancer, "Percent_Ever_below95_NewCancer.txt", sep="\t")

 
# ---------

# Pool all BMIs together (old version) --------

# Cancer pats
DIA_OBE_pts_CanDx <- fread("DIA_OBE_pts_CanDx.txt")

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(condition != "Benign Tumor")
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(condition != "Cancer History")

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(-c(encoding, icd_flag))

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% mutate(fst_dt = as.Date(fst_dt))

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(ptid, fst_dt, condition)

names(DIA_OBE_pts_CanDx)[1] <- "patient"
names(DIA_OBE_pts_CanDx)[2] <- "Date"

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% group_by(patient) %>% filter(Date == min(Date)) %>% slice(1)


CancerPats <- DIA_OBE_pts_CanDx %>% select(patient) %>% distinct()




# BMIs from DANU Measures
DANU_Measures <- fread("DANU Measures.txt")

DANU_Measures <- DANU_Measures %>% filter(test == "BMI") %>% select(patid, weight, claimed, value)

names(DANU_Measures)[1] <- "patient"
names(DANU_Measures)[3] <- "Date_BMI"
names(DANU_Measures)[4] <- "BMI"

DANU_Measures$Date_BMI <- as.Date(DANU_Measures$Date_BMI)


DANU_Measures <- CancerPats %>% inner_join(DANU_Measures)



# Get BMIs from Events table
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       

DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

names(DANU_Events)[1] <- "patient"
names(DANU_Events)[3] <- "BMI"
names(DANU_Events)[4] <- "Date"

length(unique(DANU_Events$patient)) # 756608

DANU_Events$Date <- as.Date(DANU_Events$Date)

names(DANU_Events)[4] <- "Date_BMI"

length(unique((DIA_OBE_pts_CanDx$patient))) # 118180 with Cancer, exc. those 2 types

DANU_Events <- CancerPats %>% inner_join(DANU_Events)

DANU_Events <- DANU_Events %>% select(1,2,4,3)


DANU_Events <- DANU_Events %>% bind_rows(DANU_Measures) %>% distinct()



temp <- DIA_OBE_pts_CanDx  %>% inner_join(DANU_Events, by=c("patient"="patient"))

temp$Date_BMI <- as.Date(temp$Date_BMI)


temp %>% ungroup() %>% group_by(condition) %>% count()

BMIs_After_FirstDx <- temp %>% group_by(patient) %>% filter(Date_BMI>=Date)

fwrite(BMIs_After_FirstDx, "BMIs_After_FirstDx_pooled.txt", sep="\t")

BMIs_After_FirstDx <- fread("BMIs_After_FirstDx_pooled.txt", sep="\t")

data.frame(BMIs_After_FirstDx %>% group_by(condition) %>% summarise(n=weighted.mean(BMI, weight, na.rm=T)) %>% arrange(n))




BMIs_After_FirstDx <- BMIs_After_FirstDx %>%  ungroup() %>% mutate(ElapsedTime = as.numeric(round((Date_BMI-Date)/30.5)))


BMIs_After_FirstDx %>% 
  filter(condition== "Reproductive Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Reproductive Cancer diagnosis")



BMIs_After_FirstDx <- BMIs_After_FirstDx %>% group_by(patient) %>% arrange(Date_BMI)

MaxDateALL <- BMIs_After_FirstDx %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 

names(MaxDateALL)[3] <- "Date_MAX_BMI_ALL"

MaxDateALL$NewDate <- MaxDateALL$Date_MAX_BMI_ALL

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% left_join(MaxDateALL, by=c("patient"="patient", "Date_BMI"="Date_MAX_BMI_ALL")) %>% select(-c(BMI.y))

names(BMIs_After_FirstDx)[6] <- "BMI"

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% group_by(patient) %>% fill(NewDate, .direction = "down") %>% filter(!is.na(NewDate))

BMIs_After_FirstDx$Date <- as.Date(BMIs_After_FirstDx$Date)
BMIs_After_FirstDx$Date_BMI <- as.Date(BMIs_After_FirstDx$Date_BMI)
BMIs_After_FirstDx$NewDate <- as.Date(BMIs_After_FirstDx$NewDate)

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% group_by(patient) %>% mutate(New_ElapsedTimeMAX =  as.numeric(round((Date_BMI-NewDate)/30.5)))

BMIs_After_FirstDx <- BMIs_After_FirstDx %>% group_by(patient) %>% mutate(BMI_diff = (BMI/max(BMI))*100)




temp3 <- BMIs_After_FirstDx %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

Percent_Ever_below95 <- Percent_Ever_below95 %>% left_join(temp3) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(Percent_Ever_below95)[11] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- BMIs_After_FirstDx %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

Percent_Ever_below95 <- Percent_Ever_below95 %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(Percent_Ever_below95)[13] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(Percent_Ever_below95, "Percent_Ever_below95.txt", sep="\t")

# -----------
# Pool all BMIs together Diabetes & Obesity with Cancer --------

DIA_Disorder_Histories <- fread("DIA Disorder Histories.txt")
DIA_Pats <- DIA_Disorder_Histories %>% select(patient, weight) %>% distinct()
OBE_Disorder_Histories <- fread("OBE Disorder Histories.txt")
OBE_Pats <- OBE_Disorder_Histories %>% select(patient, weight) %>% distinct()

OBE_Pats <- OBE_Pats %>% anti_join(DIA_Pats)


# Cancer pats
DIA_OBE_pts_CanDx <- fread("DIA_OBE_pts_CanDx.txt")

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(condition != "Benign Tumor")
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(condition != "Cancer History")

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(-c(encoding, icd_flag))

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% mutate(fst_dt = as.Date(fst_dt))

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(ptid, fst_dt, condition)

names(DIA_OBE_pts_CanDx)[1] <- "patient"
names(DIA_OBE_pts_CanDx)[2] <- "Date"

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% group_by(patient) %>% filter(Date == min(Date)) %>% slice(1)

CancerPats <- DIA_OBE_pts_CanDx %>% select(patient) %>% distinct()




# BMIs from DANU Measures
DANU_Measures <- fread("DANU Measures.txt")

DANU_Measures <- DANU_Measures %>% filter(test == "BMI") %>% select(patid, weight, claimed, value)

names(DANU_Measures)[1] <- "patient"
names(DANU_Measures)[3] <- "Date_BMI"
names(DANU_Measures)[4] <- "BMI"

DANU_Measures$Date_BMI <- as.Date(DANU_Measures$Date_BMI)


DANU_Measures <- CancerPats %>% inner_join(DANU_Measures)



# Get BMIs from Events table
DANU_Events <- fread("DANU Events.txt")

DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       

DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)

names(DANU_Events)[1] <- "patient"
names(DANU_Events)[3] <- "BMI"
names(DANU_Events)[4] <- "Date"

DANU_Events$Date <- as.Date(DANU_Events$Date)

names(DANU_Events)[4] <- "Date_BMI"

length(unique((DIA_OBE_pts_CanDx$patient))) # 118180 with Cancer, exc. those 2 types

DANU_Events <- CancerPats %>% inner_join(DANU_Events)

DANU_Events <- DANU_Events %>% select(1,2,4,3)



# BMIs from DANU Measures 3.1 (All DANU)
DANU_Measures_3 <- fread("DANU Measures 3.1.txt")

DANU_Measures_3 <- DANU_Measures_3 %>% filter(test == "BMI") %>% select(patid, weight, claimed, value)

names(DANU_Measures_3)[1] <- "patient"
names(DANU_Measures_3)[3] <- "Date_BMI"
names(DANU_Measures_3)[4] <- "BMI"

DANU_Measures_3$Date_BMI <- as.Date(DANU_Measures_3$Date_BMI)


DANU_Measures_3 <- CancerPats %>% inner_join(DANU_Measures_3)


# Cancer Pats = 118180

CancerPats %>% inner_join(DIA_Pats) # 54,129
CancerPats %>% inner_join(DIA_Pats) %>%  ungroup() %>% summarise(n=sum(weight)) # 7712128

CancerPats %>% inner_join(OBE_Pats) # 64,051
CancerPats %>% inner_join(OBE_Pats) %>%  ungroup() %>% summarise(n=sum(weight)) # 8855712

# Total cancer = 16567840

All_BMIs <- DANU_Events %>% bind_rows(DANU_Measures) %>% bind_rows(DANU_Measures_3) %>% distinct()

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx  %>% inner_join(All_BMIs, by=c("patient"="patient"))

DIA_OBE_pts_CanDx$Date_BMI <- as.Date(DIA_OBE_pts_CanDx$Date_BMI)

length(unique(DIA_OBE_pts_CanDx$patient)) # 80677 with BMIs

DIA_OBE_pts_CanDx %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #11318063

DIA_Pats %>% inner_join(DIA_OBE_pts_CanDx) %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #34042 (4854924)
OBE_Pats %>% inner_join(DIA_OBE_pts_CanDx) %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #46635 (6463138)

DIA_Pats <- DIA_Pats %>% inner_join(DIA_OBE_pts_CanDx)

OBE_Pats <- OBE_Pats %>% inner_join(DIA_OBE_pts_CanDx)


DIA_Pats %>% group_by(patient) %>% filter(Date_BMI == min(Date_BMI) | Date_BMI == max(Date_BMI)) %>%
  filter(row_number() == 1 | row_number() == n()) %>% select(patient, weight) %>% group_by(patient, weight) %>% count() %>% filter(n>1) %>% ungroup() %>% summarise(n=sum(weight)) # 32941 (4700127)

OBE_Pats %>% group_by(patient) %>% filter(Date_BMI == min(Date_BMI) | Date_BMI == max(Date_BMI)) %>%
  filter(row_number() == 1 | row_number() == n()) %>% select(patient, weight) %>% group_by(patient, weight) %>% count() %>% filter(n>1) %>% ungroup() %>% summarise(n=sum(weight)) # 45451 (6299509)



DIA_Pats %>% group_by(patient) %>% filter(Date_BMI>Date) %>% filter(Date_BMI == min(Date_BMI) | Date_BMI == max(Date_BMI)) %>%
  filter(row_number() == 1 | row_number() == n()) %>% select(patient, weight) %>% group_by(patient, weight) %>% count() %>% filter(n>1) %>% ungroup() %>% summarise(n=sum(weight)) # 28849 (4119903)

OBE_Pats %>% group_by(patient) %>% filter(Date_BMI>Date) %>% filter(Date_BMI == min(Date_BMI) | Date_BMI == max(Date_BMI)) %>%
  filter(row_number() == 1 | row_number() == n()) %>% select(patient, weight) %>% group_by(patient, weight) %>% count() %>% filter(n>1) %>% ungroup() %>% summarise(n=sum(weight)) # 45451 (5492012)


DIA_Pats <- DIA_Pats %>% group_by(patient) %>% arrange(Date_BMI)
OBE_Pats <- OBE_Pats %>% group_by(patient) %>% arrange(Date_BMI)


DIA_Pats <- DIA_Pats %>% group_by(patient) %>% filter(Date_BMI>Date)
OBE_Pats <- OBE_Pats %>% group_by(patient) %>% filter(Date_BMI>Date)



DIA_Pats %>% group_by(patient) %>% filter(Date_BMI>Date) %>% filter(Date_BMI == min(Date_BMI) | Date_BMI == max(Date_BMI)) %>%
  filter(row_number() == 1 | row_number() == n()) %>% distinct() %>% select(patient, weight) %>% group_by(patient, weight) %>% count() %>% filter(n>1) %>% ungroup() %>% summarise(n=sum(weight)) # 28849 (4119903)

OBE_Pats %>% group_by(patient) %>% filter(Date_BMI>Date) %>% filter(Date_BMI == min(Date_BMI) | Date_BMI == max(Date_BMI)) %>%
  filter(row_number() == 1 | row_number() == n())  %>% distinct() %>% select(patient, weight) %>% group_by(patient, weight) %>% count() %>% filter(n>1) %>% ungroup() %>% summarise(n=sum(weight)) # 45451 (5492012)




DIA_Pats %>% group_by(patient) %>% mutate(ElapsTimeBMIs = as.numeric(Date_BMI-lag(Date_BMI))) %>% filter(ElapsTimeBMIs>6) %>%
  select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #23528  (3361411)



DIA_Pats %>% group_by(patient) %>% mutate(ElapsTimeBMIs = as.numeric(Date_BMI-lag(Date_BMI))) %>% filter(ElapsTimeBMIs>12) %>%
  select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #23118  (3302862)





OBE_Pats %>% group_by(patient) %>% mutate(ElapsTimeBMIs = as.numeric(Date_BMI-lag(Date_BMI))) %>% filter(ElapsTimeBMIs>6) %>%
  select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #32653  (4522358)



OBE_Pats %>% group_by(patient) %>% mutate(ElapsTimeBMIs = as.numeric(Date_BMI-lag(Date_BMI))) %>% filter(ElapsTimeBMIs>12) %>%
  select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #32097  (4444211)




DIA_MaxDateALL <- DIA_Pats %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 
OBE_MaxDateALL <- OBE_Pats %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 



# -----
# Pool all BMIs together NASH with Cancer -----------

NASH_Disorder_Histories <- fread("NASH Disorder Histories.txt")

NASH_Disorder_Histories <- NASH_Disorder_Histories %>% select(patient, weight, month60)

sum(NASH_Disorder_Histories$weight) # 1388973

NASH_Disorder_Histories %>% filter(grepl("c", month60)|grepl("C", month60)) %>% summarise(n=sum(weight)) # 245861.3 (18%)


NASH_Cancer_History <- NASH_Disorder_Histories %>% mutate(CancerStatus = ifelse(grepl("c", month60)|grepl("C", month60), "Cancer", "no"))

NASH_Cancer_History <- NASH_Cancer_History %>% select(-month60)

NASH_Cancer <- NASH_Cancer_History %>% filter(CancerStatus == "Cancer") %>% select(patient, weight)

sum(NASH_Cancer$weight) # 1816 # 245861.3




# BMIs from DANU Measures 3.1 (All DANU)
DANU_Measures_3 <- fread("DANU Measures 3.1.txt")

DANU_Measures_3 <- DANU_Measures_3 %>% filter(test == "BMI") %>% select(patid, weight, claimed, value)

names(DANU_Measures_3)[1] <- "patient"
names(DANU_Measures_3)[3] <- "Date_BMI"
names(DANU_Measures_3)[4] <- "BMI"

DANU_Measures_3$Date_BMI <- as.Date(DANU_Measures_3$Date_BMI)


DANU_Measures_3 <- NASH_Cancer %>% inner_join(DANU_Measures_3)

length(unique(DANU_Measures_3$patient)) # 1174 
DANU_Measures_3 %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 159575.2



# Using separate tables 

NASH_Extract_Observations <- fread("NASH Extract Observations.txt")
NASH_Extract_Observations <- NASH_Extract_Observations %>% filter(obs_type=="BMI") %>% select(patid, weight, obs_date, obs_result)

names(NASH_Extract_Observations)[1] <- "patient"
names(NASH_Extract_Observations)[3] <- "Date_BMI"
names(NASH_Extract_Observations)[4] <- "BMI"

NASH_Extract_Observations <- NASH_Extract_Observations %>% distinct()
NASH_Extract_Observations <- NASH_Cancer %>% inner_join(NASH_Extract_Observations)

NASH_Extract_Observations$Date_BMI <- as.Date(NASH_Extract_Observations$Date_BMI)







NASH_Extract_NLP_Measurements <- fread("NASH Extract NLP Measurements.txt")
NASH_Extract_NLP_Measurements <- NASH_Extract_NLP_Measurements %>% filter(measurement_type=="BMI") %>% select(patid, weight, note_date, measurement_value)

names(NASH_Extract_NLP_Measurements)[1] <- "patient"
names(NASH_Extract_NLP_Measurements)[3] <- "Date_BMI"
names(NASH_Extract_NLP_Measurements)[4] <- "BMI"


NASH_Extract_NLP_Measurements <- NASH_Extract_NLP_Measurements %>% distinct()
NASH_Extract_NLP_Measurements <- NASH_Cancer %>% inner_join(NASH_Extract_NLP_Measurements)

NASH_Extract_NLP_Measurements$Date_BMI <- as.Date(NASH_Extract_NLP_Measurements$Date_BMI)




NASH_Extract_Symptoms <- fread("NASH Extract Symptoms.txt")

NASH_Extract_Symptoms <- NASH_Extract_Symptoms %>% filter(grepl("BMI", code)) %>% select(patid, code, claimed)


NASH_Extract_Symptoms$claimed <- str_sub(NASH_Extract_Symptoms$claimed,2,-2)


names(NASH_Extract_Symptoms)[1] <- "patient"
names(NASH_Extract_Symptoms)[3] <- "Date_BMI"
names(NASH_Extract_Symptoms)[2] <- "BMI"

NASH_Extract_Symptoms <- NASH_Cancer %>% inner_join(NASH_Extract_Symptoms)


names(NASH_Extract_Symptoms)[1] <- "patient"
names(NASH_Extract_Symptoms)[4] <- "Date_BMI"
names(NASH_Extract_Symptoms)[3] <- "BMI"


NASH_Extract_Symptoms$BMI <- as.character(NASH_Extract_Symptoms$BMI)
NASH_Extract_Symptoms$BMI <- parse_number(NASH_Extract_Symptoms$BMI)
NASH_Extract_Symptoms$BMI <- as.character(NASH_Extract_Symptoms$BMI)

NASH_Extract_Symptoms <- NASH_Extract_Symptoms %>% select(1,2,4,3)

NASH_Extract_Symptoms <- separate_rows(NASH_Extract_Symptoms, Date_BMI, sep = ",", convert=T )

NASH_Extract_Symptoms$Date_BMI <- as.Date(NASH_Extract_Symptoms$Date_BMI)


NASH_Extract_NLP_Measurements %>% bind_rows(NASH_Extract_Observations) %>% distinct() %>% bind_rows(NASH_Extract_Symptoms) %>% distinct() %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) #1138 # 154600.2

NASH_BMIs <- NASH_Extract_NLP_Measurements %>% bind_rows(NASH_Extract_Observations) %>% distinct() %>% bind_rows(NASH_Extract_Symptoms) %>% distinct() 

length(unique(NASH_BMIs$patient))

NASH_BMIs %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) 


NASH_Disorders <- fread("NASH Disorders.txt")
NASH_Disorders <- NASH_Disorders %>% select(patient, weight, cancer) %>% filter(!is.na(cancer))
names(NASH_Disorders)[3] <- "Date"

NASH_BMIs <- NASH_Disorders %>% inner_join(NASH_BMIs)

fwrite(NASH_BMIs, "NASH_BMIs_All.txt")

NASH_BMIs <- fread("NASH_BMIs_All.txt")

# More than 2 BMIs

NASH_BMIs %>% group_by(patient) %>% filter(Date_BMI == min(Date_BMI) | Date_BMI == max(Date_BMI)) %>%
  filter(row_number() == 1 | row_number() == n()) %>% select(patient, weight) %>% group_by(patient, weight) %>%
  count() %>% filter(n>1) %>% ungroup() %>% summarise(n=sum(weight)) # 124587

# More than 2 BMIs after first cancer dx

NASH_BMIs %>% group_by(patient) %>% filter(Date_BMI>Date) %>% filter(Date_BMI == min(Date_BMI) | Date_BMI == max(Date_BMI)) %>%
  filter(row_number() == 1 | row_number() == n()) %>% select(patient, weight) %>% group_by(patient, weight) %>% count() %>% filter(n>1) %>% ungroup() %>% summarise(n=sum(weight)) #  (124587)




NASH_BMIs <- NASH_BMIs %>% group_by(patient) %>% arrange(Date_BMI)
NASH_BMIs <- NASH_BMIs %>% group_by(patient) %>% filter(Date_BMI>Date)


NASH_BMIs %>% group_by(patient) %>% mutate(ElapsTimeBMIs = as.numeric(Date_BMI-lag(Date_BMI))) %>% filter(ElapsTimeBMIs>6) %>%
  select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #  (99164)


NASH_BMIs %>% group_by(patient) %>% mutate(ElapsTimeBMIs = as.numeric(Date_BMI-lag(Date_BMI))) %>% filter(ElapsTimeBMIs>12) %>%
  select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(weight)) #  (97066)



# -----------

# Compare BMIs of ALL NASH patients with NASH Cancer patients -------------
# BMIs of NASH Cancer

NASH_BMIs_All_Cancer <- fread("NASH_BMIs_All.txt")


# BMIs of NASH Entire cohort 

# Using separate tables 

NASH_Extract_Observations <- fread("NASH Extract Observations.txt")
NASH_Extract_Observations <- NASH_Extract_Observations %>% filter(obs_type=="BMI") %>% select(patid, weight, obs_date, obs_result)

names(NASH_Extract_Observations)[1] <- "patient"
names(NASH_Extract_Observations)[3] <- "Date_BMI"
names(NASH_Extract_Observations)[4] <- "BMI"

NASH_Extract_Observations <- NASH_Extract_Observations %>% distinct()
NASH_Extract_Observations$Date_BMI <- as.Date(NASH_Extract_Observations$Date_BMI)
NASH_Extract_Observations <- NASH_Extract_Observations %>% select(1,3,4)





NASH_Extract_NLP_Measurements <- fread("NASH Extract NLP Measurements.txt")
NASH_Extract_NLP_Measurements <- NASH_Extract_NLP_Measurements %>% filter(measurement_type=="BMI") %>% select(patid, weight, note_date, measurement_value)

names(NASH_Extract_NLP_Measurements)[1] <- "patient"
names(NASH_Extract_NLP_Measurements)[3] <- "Date_BMI"
names(NASH_Extract_NLP_Measurements)[4] <- "BMI"

NASH_Extract_NLP_Measurements <- NASH_Extract_NLP_Measurements %>% distinct()
NASH_Extract_NLP_Measurements$Date_BMI <- as.Date(NASH_Extract_NLP_Measurements$Date_BMI)
NASH_Extract_NLP_Measurements <- NASH_Extract_NLP_Measurements %>% select(1,3,4)



NASH_Extract_Symptoms <- fread("NASH Extract Symptoms.txt")

NASH_Extract_Symptoms <- NASH_Extract_Symptoms %>% filter(grepl("BMI", code)) %>% select(patid, code, claimed)

NASH_Extract_Symptoms$claimed <- str_sub(NASH_Extract_Symptoms$claimed,2,-2)

names(NASH_Extract_Symptoms)[1] <- "patient"
names(NASH_Extract_Symptoms)[3] <- "Date_BMI"
names(NASH_Extract_Symptoms)[2] <- "BMI"


NASH_Extract_Symptoms$BMI <- as.character(NASH_Extract_Symptoms$BMI)
NASH_Extract_Symptoms$BMI <- parse_number(NASH_Extract_Symptoms$BMI)
NASH_Extract_Symptoms$BMI <- as.character(NASH_Extract_Symptoms$BMI)

NASH_Extract_Symptoms <- NASH_Extract_Symptoms %>% select(1,3,2)

NASH_Extract_Symptoms <- separate_rows(NASH_Extract_Symptoms, Date_BMI, sep = ",", convert=T )

NASH_Extract_Symptoms$Date_BMI <- as.Date(NASH_Extract_Symptoms$Date_BMI)


NASH_Extract_NLP_Measurements %>% bind_rows(NASH_Extract_Observations) %>% distinct() %>% bind_rows(NASH_Extract_Symptoms) %>% distinct() %>%
  select(patient) %>% distinct() # 6108

NASH_BMIs <- NASH_Extract_NLP_Measurements %>% bind_rows(NASH_Extract_Observations) %>% distinct() %>% bind_rows(NASH_Extract_Symptoms) %>% distinct() 

length(unique(NASH_BMIs$patient))

NASH_BMIs_EntireCohort <- NASH_BMIs


NASH_BMIs_EntireCohort$group <- "All NASH Patients"
NASH_BMIs_All_Cancer$group <- "NASH Cancer Patients"


NASH_BMIs_EntireCohort <- NASH_BMIs_EntireCohort %>% group_by(patient) %>% arrange(Date_BMI) %>% filter(Date_BMI==max(Date_BMI)) %>% slice(1)
NASH_BMIs_All_Cancer <- NASH_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI) %>% filter(Date_BMI==max(Date_BMI)) %>% slice(1)

NASH_BMIs_All_Cancer <-  NASH_BMIs_All_Cancer %>% select(-c(weight, Date))

Pooled <- NASH_BMIs_All_Cancer %>% bind_rows(NASH_BMIs_EntireCohort)

Pooled %>% ungroup() %>% group_by(group) %>% summarise(n=mean(as.numeric(BMI), na.rm=T))

Pooled$BMI <- as.numeric(Pooled$BMI)

Pooled <- Pooled %>% filter(!is.na(BMI))

Pooled %>% filter(BMI>10 & BMI<80) %>% ggplot(aes(x = BMI, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n BMI") + ylab("NASH Group \n")


# -------
# BMIs over time for different types of cancer: Diabetes and Obesity -------------

NASH_BMIs_All <- fread("NASH_BMIs_All.txt")
OBE_BMIs_All_Cancer <- fread("OBE_BMIs_All_Cancer.txt")
DIA_BMIs_All_Cancer <- fread("DIA_BMIs_All_Cancer.txt")


OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% filter(Date_BMI>=Date)
DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% filter(Date_BMI>=Date)

OBE_BMIs_All_Cancer$Date <- as.Date(OBE_BMIs_All_Cancer$Date)
DIA_BMIs_All_Cancer$Date <- as.Date(DIA_BMIs_All_Cancer$Date)


OBE_BMIs_All_Cancer$Date_BMI <- as.Date(OBE_BMIs_All_Cancer$Date_BMI)
DIA_BMIs_All_Cancer$Date_BMI <- as.Date(DIA_BMIs_All_Cancer$Date_BMI)

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>%  ungroup() %>% mutate(ElapsedTime = as.numeric(round((Date_BMI-Date)/30.5)))
DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>%  ungroup() %>% mutate(ElapsedTime = as.numeric(round((Date_BMI-Date)/30.5)))

data.frame(OBE_BMIs_All_Cancer %>% group_by(condition) %>% summarise(n=weighted.mean(BMI, weight, na.rm=T)) %>% arrange(n))


data.frame(DIA_BMIs_All_Cancer %>% group_by(condition) %>% summarise(n=weighted.mean(BMI, weight, na.rm=T)) %>% arrange(n))



OBE_BMIs_All_Cancer %>% 
  filter(condition== "Gastroesophageal Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Gastroesophageal Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Head Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Head Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Leukemia Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Leukemia Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Other Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Other Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Prostate Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Prostate Cancer diagnosis in Obesity")


OBE_BMIs_All_Cancer %>% 
  filter(condition== "Lung Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Lung Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Urinary Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Urinary Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Respiratory Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Respiratory Cancer diagnosis in Obesity")


OBE_BMIs_All_Cancer %>% 
  filter(condition== "Skin Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Skin Cancer diagnosis in Obesity")



OBE_BMIs_All_Cancer %>% 
  filter(condition== "Lymphoma Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Lymphoma Cancer diagnosis in Obesity")



OBE_BMIs_All_Cancer %>% 
  filter(condition== "Myeloma Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Myeloma Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Intestinal Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Intestinal Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Unspecified Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Unspecified Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Breast Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Breast Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Salivary Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Salivary Cancer diagnosis in Obesity")


OBE_BMIs_All_Cancer %>% 
  filter(condition== "Kidney Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Kidney Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Bone Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Bone Cancer diagnosis in Obesity")

OBE_BMIs_All_Cancer %>% 
  filter(condition== "Thyroid Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Thyroid Cancer diagnosis in Obesity")


OBE_BMIs_All_Cancer %>% 
  filter(condition== "Reproductive Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Reproductive Cancer diagnosis in Obesity")








DIA_BMIs_All_Cancer %>% 
  filter(condition== "Gastroesophageal Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Gastroesophageal Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Head Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Head Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Leukemia Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Leukemia Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Other Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Other Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Prostate Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Prostate Cancer diagnosis in Diabetes")


DIA_BMIs_All_Cancer %>% 
  filter(condition== "Lung Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Lung Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Urinary Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Urinary Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Respiratory Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Respiratory Cancer diagnosis in Diabetes")


DIA_BMIs_All_Cancer %>% 
  filter(condition== "Skin Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Skin Cancer diagnosis in Diabetes")



DIA_BMIs_All_Cancer %>% 
  filter(condition== "Lymphoma Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Lymphoma Cancer diagnosis in Diabetes")



DIA_BMIs_All_Cancer %>% 
  filter(condition== "Myeloma Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Myeloma Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Intestinal Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Intestinal Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Unspecified Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Unspecified Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Breast Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Breast Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Salivary Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Salivary Cancer diagnosis in Diabetes")


DIA_BMIs_All_Cancer %>% 
  filter(condition== "Kidney Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Kidney Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Bone Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Bone Cancer diagnosis in Diabetes")

DIA_BMIs_All_Cancer %>% 
  filter(condition== "Thyroid Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Thyroid Cancer diagnosis in Diabetes")


DIA_BMIs_All_Cancer %>% 
  filter(condition== "Reproductive Cancer") %>%
  ggplot(aes(ElapsedTime, BMI)) +
  geom_smooth(method="loess", fill="deepskyblue4", colour="firebrick", se=F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("BMI  \n")+xlab("\n Number of Months after Reproductive Cancer diagnosis in Diabetes")





length(unique(OBE_BMIs_All_Cancer$patient))
length(unique(DIA_BMIs_All_Cancer$patient))

# --------
# How many had +5% reducions within 6 months? 12 months? Diabetes and Obesity -----------
OBE_BMIs_All_Cancer <- fread("OBE_BMIs_All_Cancer.txt")
DIA_BMIs_All_Cancer <- fread("DIA_BMIs_All_Cancer.txt")


OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% filter(Date_BMI>=Date)
DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% filter(Date_BMI>=Date)

OBE_BMIs_All_Cancer$Date <- as.Date(OBE_BMIs_All_Cancer$Date)
DIA_BMIs_All_Cancer$Date <- as.Date(DIA_BMIs_All_Cancer$Date)


OBE_BMIs_All_Cancer$Date_BMI <- as.Date(OBE_BMIs_All_Cancer$Date_BMI)
DIA_BMIs_All_Cancer$Date_BMI <- as.Date(DIA_BMIs_All_Cancer$Date_BMI)








OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI)

MaxDateALL <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 

names(MaxDateALL)[3] <- "Date_MAX_BMI_ALL"

MaxDateALL$NewDate <- MaxDateALL$Date_MAX_BMI_ALL

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% left_join(MaxDateALL, by=c("patient"="patient", "Date_BMI"="Date_MAX_BMI_ALL")) %>% select(-c(BMI.y))

names(OBE_BMIs_All_Cancer)[6] <- "BMI"

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% fill(NewDate, .direction = "down") %>% filter(!is.na(NewDate))

OBE_BMIs_All_Cancer$Date <- as.Date(OBE_BMIs_All_Cancer$Date)
OBE_BMIs_All_Cancer$Date_BMI <- as.Date(OBE_BMIs_All_Cancer$Date_BMI)
OBE_BMIs_All_Cancer$NewDate <- as.Date(OBE_BMIs_All_Cancer$NewDate)

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% mutate(New_ElapsedTimeMAX =  as.numeric(round((Date_BMI-NewDate)/30.5)))

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% mutate(BMI_diff = (BMI/max(BMI))*100)


temp <- OBE_BMIs_All_Cancer %>% select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()
names(temp)[2] <- "TOTAL"

temp2 <- OBE_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

temp <- temp %>% left_join(temp2) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(temp)[3] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- OBE_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()


temp <- temp %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(temp)[5] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(temp, "Percent_Ever_below95_Obesity.txt", sep="\t")






DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI)

MaxDateALL <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 

names(MaxDateALL)[3] <- "Date_MAX_BMI_ALL"

MaxDateALL$NewDate <- MaxDateALL$Date_MAX_BMI_ALL

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% left_join(MaxDateALL, by=c("patient"="patient", "Date_BMI"="Date_MAX_BMI_ALL")) %>% select(-c(BMI.y))

names(DIA_BMIs_All_Cancer)[6] <- "BMI"

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% fill(NewDate, .direction = "down") %>% filter(!is.na(NewDate))

DIA_BMIs_All_Cancer$Date <- as.Date(DIA_BMIs_All_Cancer$Date)
DIA_BMIs_All_Cancer$Date_BMI <- as.Date(DIA_BMIs_All_Cancer$Date_BMI)
DIA_BMIs_All_Cancer$NewDate <- as.Date(DIA_BMIs_All_Cancer$NewDate)

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% mutate(New_ElapsedTimeMAX =  as.numeric(round((Date_BMI-NewDate)/30.5)))

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% mutate(BMI_diff = (BMI/max(BMI))*100)


temp <- DIA_BMIs_All_Cancer %>% select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()
names(temp)[2] <- "TOTAL"

temp2 <- DIA_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

temp <- temp %>% left_join(temp2) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(temp)[3] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- DIA_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()


temp <- temp %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(temp)[5] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(temp, "Percent_Ever_below95_Diabetes.txt", sep="\t")





BMI_reductions_DiaObe_Size <- fread("BMI_reductions_DiaObe_Size.txt")

names(BMI_reductions_DiaObe_Size)[3] <- "DIA_6m"
names(BMI_reductions_DiaObe_Size)[4] <- "DIA_12m"

names(BMI_reductions_DiaObe_Size)[6] <- "OBE_6m"
names(BMI_reductions_DiaObe_Size)[7] <- "OBE_12m"

BMI_reductions_DiaObe_Size$DIA_6m <- BMI_reductions_DiaObe_Size$DIA_6m * 100 
BMI_reductions_DiaObe_Size$DIA_12m <- BMI_reductions_DiaObe_Size$DIA_12m * 100 

BMI_reductions_DiaObe_Size$OBE_6m <- BMI_reductions_DiaObe_Size$OBE_6m * 100 
BMI_reductions_DiaObe_Size$OBE_12m <- BMI_reductions_DiaObe_Size$OBE_12m * 100 


library(ggrepel)
library(hrbrthemes)
library(viridis)



ggplot(BMI_reductions_DiaObe_Size, aes(x=`TOTAL DIA`, y=DIA_6m, size = `TOTAL DIA`, fill=DIA_6m, colour=DIA_6m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "A")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <6 months\n")

ggplot(BMI_reductions_DiaObe_Size, aes(x=`TOTAL DIA`, y=DIA_12m, size = `TOTAL DIA`, fill=DIA_12m, colour=DIA_12m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "A")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <12 months\n")







ggplot(BMI_reductions_DiaObe_Size, aes(x=`TOAL OBE`, y=OBE_6m, size = `TOAL OBE`, fill=OBE_6m, colour=OBE_6m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "D")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <6 months\n")

ggplot(BMI_reductions_DiaObe_Size, aes(x=`TOAL OBE`, y=OBE_12m, size = `TOAL OBE`, fill=OBE_12m, colour=OBE_12m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "D")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <12 months\n")
# -------
# How many had +5% reducions within 6 months? 12 months? After 1st Metastasis Diabetes and Obesity -----------

DIA_OBE_pts_CanDx <- fread("DIA_OBE_pts_CanDx.txt")
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(grepl("Metastasis", diagnosis))
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(ptid, fst_dt) %>% distinct() %>% group_by(ptid) %>% arrange(fst_dt)
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(ptid, fst_dt) %>% distinct() %>% group_by(ptid) %>%slice(1)
OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% ungroup()

names(DIA_OBE_pts_CanDx)[1] <- "patient"
names(DIA_OBE_pts_CanDx)[2] <- "Date_Metastasis"

OBE_BMIs_All_Cancer <- fread("OBE_BMIs_All_Cancer.txt")
DIA_BMIs_All_Cancer <- fread("DIA_BMIs_All_Cancer.txt")

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% inner_join(DIA_OBE_pts_CanDx, by=c("patient"="patient"))
OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>%  filter(Date_BMI > Date_Metastasis)


DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% inner_join(DIA_OBE_pts_CanDx, by=c("patient"="patient"))
DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% filter(Date_BMI > Date_Metastasis)





OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% select(-Date_Metastasis)
DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% select(-Date_Metastasis)



OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI)

MaxDateALL <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 

names(MaxDateALL)[3] <- "Date_MAX_BMI_ALL"

MaxDateALL$NewDate <- MaxDateALL$Date_MAX_BMI_ALL

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% left_join(MaxDateALL, by=c("patient"="patient", "Date_BMI"="Date_MAX_BMI_ALL")) %>% select(-c(BMI.y))

names(OBE_BMIs_All_Cancer)[6] <- "BMI"

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% fill(NewDate, .direction = "down") %>% filter(!is.na(NewDate))

OBE_BMIs_All_Cancer$Date <- as.Date(OBE_BMIs_All_Cancer$Date)
OBE_BMIs_All_Cancer$Date_BMI <- as.Date(OBE_BMIs_All_Cancer$Date_BMI)
OBE_BMIs_All_Cancer$NewDate <- as.Date(OBE_BMIs_All_Cancer$NewDate)

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% mutate(New_ElapsedTimeMAX =  as.numeric(round((Date_BMI-NewDate)/30.5)))

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% mutate(BMI_diff = (BMI/max(BMI))*100)


temp <- OBE_BMIs_All_Cancer %>% select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()
names(temp)[2] <- "TOTAL"

temp2 <- OBE_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

temp <- temp %>% left_join(temp2) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(temp)[3] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- OBE_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()


temp <- temp %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(temp)[5] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(temp, "Percent_Ever_below95_Obesity_metastasis.txt", sep="\t")




DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI)

MaxDateALL <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 

names(MaxDateALL)[3] <- "Date_MAX_BMI_ALL"

MaxDateALL$NewDate <- MaxDateALL$Date_MAX_BMI_ALL

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% left_join(MaxDateALL, by=c("patient"="patient", "Date_BMI"="Date_MAX_BMI_ALL")) %>% select(-c(BMI.y))

names(DIA_BMIs_All_Cancer)[6] <- "BMI"

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% fill(NewDate, .direction = "down") %>% filter(!is.na(NewDate))

DIA_BMIs_All_Cancer$Date <- as.Date(DIA_BMIs_All_Cancer$Date)
DIA_BMIs_All_Cancer$Date_BMI <- as.Date(DIA_BMIs_All_Cancer$Date_BMI)
DIA_BMIs_All_Cancer$NewDate <- as.Date(DIA_BMIs_All_Cancer$NewDate)

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% mutate(New_ElapsedTimeMAX =  as.numeric(round((Date_BMI-NewDate)/30.5)))

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% mutate(BMI_diff = (BMI/max(BMI))*100)


temp <- DIA_BMIs_All_Cancer %>% select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()
names(temp)[2] <- "TOTAL"

temp2 <- DIA_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

temp <- temp %>% left_join(temp2) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(temp)[3] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- DIA_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()


temp <- temp %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(temp)[5] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(temp, "Percent_Ever_below95_Diabetes_metastasis.txt", sep="\t")
 




BMI_reductions_DiaObe_Size <- fread("BMI_reductions_DiaObe_Size_metastasis.txt")

names(BMI_reductions_DiaObe_Size)[3] <- "DIA_6m"
names(BMI_reductions_DiaObe_Size)[4] <- "DIA_12m"

names(BMI_reductions_DiaObe_Size)[6] <- "OBE_6m"
names(BMI_reductions_DiaObe_Size)[7] <- "OBE_12m"

BMI_reductions_DiaObe_Size$DIA_6m <- BMI_reductions_DiaObe_Size$DIA_6m * 100 
BMI_reductions_DiaObe_Size$DIA_12m <- BMI_reductions_DiaObe_Size$DIA_12m * 100 

BMI_reductions_DiaObe_Size$OBE_6m <- BMI_reductions_DiaObe_Size$OBE_6m * 100 
BMI_reductions_DiaObe_Size$OBE_12m <- BMI_reductions_DiaObe_Size$OBE_12m * 100 


library(ggrepel)
library(hrbrthemes)
library(viridis)



ggplot(BMI_reductions_DiaObe_Size, aes(x=TOTAL_DIA, y=DIA_6m, size = TOTAL_DIA, fill=DIA_6m, colour=DIA_6m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "A")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <6 months\n")

ggplot(BMI_reductions_DiaObe_Size, aes(x=TOTAL_DIA, y=DIA_12m, size = TOTAL_DIA, fill=DIA_12m, colour=DIA_12m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "A")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <12 months\n")







ggplot(BMI_reductions_DiaObe_Size, aes(x=TOTAL_OBE, y=OBE_6m, size = TOTAL_OBE, fill=OBE_6m, colour=OBE_6m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "D")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <6 months\n")

ggplot(BMI_reductions_DiaObe_Size, aes(x=TOTAL_OBE, y=OBE_12m, size = TOTAL_OBE, fill=OBE_12m, colour=OBE_12m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "D")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <12 months\n")
# -----------
# How many had +5% reducions within 6 months? 12 months? After 1st Metastasis Diabetes and Obesity ALL BMIs for MAX -----------

DIA_OBE_pts_CanDx <- fread("DIA_OBE_pts_CanDx.txt")
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(grepl("Metastasis", diagnosis))
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(ptid, fst_dt) %>% distinct() %>% group_by(ptid) %>% arrange(fst_dt)
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(ptid, fst_dt) %>% distinct() %>% group_by(ptid) %>%slice(1)
OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% ungroup()

names(DIA_OBE_pts_CanDx)[1] <- "patient"
names(DIA_OBE_pts_CanDx)[2] <- "Date_Metastasis"

OBE_BMIs_All_Cancer <- fread("OBE_BMIs_All_Cancer.txt")
DIA_BMIs_All_Cancer <- fread("DIA_BMIs_All_Cancer.txt")

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% inner_join(DIA_OBE_pts_CanDx, by=c("patient"="patient"))


DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% inner_join(DIA_OBE_pts_CanDx, by=c("patient"="patient"))




OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI)

MaxDateALL <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 

names(MaxDateALL)[3] <- "Date_MAX_BMI_ALL"

MaxDateALL$NewDate <- MaxDateALL$Date_MAX_BMI_ALL

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% left_join(MaxDateALL, by=c("patient"="patient", "Date_BMI"="Date_MAX_BMI_ALL")) %>% select(-c(BMI.y))

names(OBE_BMIs_All_Cancer)[6] <- "BMI"

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% fill(NewDate, .direction = "down") %>% filter(!is.na(NewDate))

OBE_BMIs_All_Cancer$Date <- as.Date(OBE_BMIs_All_Cancer$Date)
OBE_BMIs_All_Cancer$Date_BMI <- as.Date(OBE_BMIs_All_Cancer$Date_BMI)
OBE_BMIs_All_Cancer$NewDate <- as.Date(OBE_BMIs_All_Cancer$NewDate)


OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% mutate(New_ElapsedTimeMAX =  as.numeric(round((Date_BMI-NewDate)/30.5)))

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% mutate(BMI_diff = (BMI/max(BMI))*100)

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% filter(Date_BMI>Date_Metastasis) %>% select(-Date_Metastasis)

temp <- OBE_BMIs_All_Cancer %>% select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()
names(temp)[2] <- "TOTAL"

temp2 <- OBE_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

temp <- temp %>% left_join(temp2) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(temp)[3] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- OBE_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()


temp <- temp %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(temp)[5] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(temp, "Percent_Ever_below95_Obesity_metastasis_all.txt", sep="\t")




DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI)

MaxDateALL <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% select(patient, BMI, Date_BMI) %>% filter(BMI == max(BMI)) 

names(MaxDateALL)[3] <- "Date_MAX_BMI_ALL"

MaxDateALL$NewDate <- MaxDateALL$Date_MAX_BMI_ALL

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% left_join(MaxDateALL, by=c("patient"="patient", "Date_BMI"="Date_MAX_BMI_ALL")) %>% select(-c(BMI.y))

names(DIA_BMIs_All_Cancer)[6] <- "BMI"

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% fill(NewDate, .direction = "down") %>% filter(!is.na(NewDate))

DIA_BMIs_All_Cancer$Date <- as.Date(DIA_BMIs_All_Cancer$Date)
DIA_BMIs_All_Cancer$Date_BMI <- as.Date(DIA_BMIs_All_Cancer$Date_BMI)
DIA_BMIs_All_Cancer$NewDate <- as.Date(DIA_BMIs_All_Cancer$NewDate)

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% filter(Date_BMI>Date_Metastasis) %>% select(-Date_Metastasis)

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% mutate(New_ElapsedTimeMAX =  as.numeric(round((Date_BMI-NewDate)/30.5)))

DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% mutate(BMI_diff = (BMI/max(BMI))*100)


temp <- DIA_BMIs_All_Cancer %>% select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()
names(temp)[2] <- "TOTAL"

temp2 <- DIA_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 6 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()

temp <- temp %>% left_join(temp2) %>% mutate(Percentage_6m_FirstMaxNEW = n/TOTAL)

names(temp)[3] <- "Below95_6mfromNEW_FIRSTmax"

temp3 <- DIA_BMIs_All_Cancer %>% filter(BMI_diff < 95 & New_ElapsedTimeMAX < 12 & New_ElapsedTimeMAX > 0) %>% 
  select(patient, condition) %>% distinct() %>% group_by(condition) %>% count()


temp <- temp %>% left_join(temp3) %>% mutate(Percentage_12m_FirstMaxNEW = n/TOTAL)

names(temp)[5] <- "Below95_12mfromNEW_FIRSTmax"


fwrite(temp, "Percent_Ever_below95_Diabetes_metastasis_all.txt", sep="\t")





BMI_reductions_DiaObe_Size <- fread("BMI_reductions_DiaObe_Size_metastasis.txt")

names(BMI_reductions_DiaObe_Size)[3] <- "DIA_6m"
names(BMI_reductions_DiaObe_Size)[4] <- "DIA_12m"

names(BMI_reductions_DiaObe_Size)[6] <- "OBE_6m"
names(BMI_reductions_DiaObe_Size)[7] <- "OBE_12m"

BMI_reductions_DiaObe_Size$DIA_6m <- BMI_reductions_DiaObe_Size$DIA_6m * 100 
BMI_reductions_DiaObe_Size$DIA_12m <- BMI_reductions_DiaObe_Size$DIA_12m * 100 

BMI_reductions_DiaObe_Size$OBE_6m <- BMI_reductions_DiaObe_Size$OBE_6m * 100 
BMI_reductions_DiaObe_Size$OBE_12m <- BMI_reductions_DiaObe_Size$OBE_12m * 100 


library(ggrepel)
library(hrbrthemes)
library(viridis)



ggplot(BMI_reductions_DiaObe_Size, aes(x=TOTAL_DIA, y=DIA_6m, size = TOTAL_DIA, fill=DIA_6m, colour=DIA_6m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "A")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <6 months\n")

ggplot(BMI_reductions_DiaObe_Size, aes(x=TOTAL_DIA, y=DIA_12m, size = TOTAL_DIA, fill=DIA_12m, colour=DIA_12m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "A")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <12 months\n")







ggplot(BMI_reductions_DiaObe_Size, aes(x=TOTAL_OBE, y=OBE_6m, size = TOTAL_OBE, fill=OBE_6m, colour=OBE_6m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "D")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <6 months\n")

ggplot(BMI_reductions_DiaObe_Size, aes(x=TOTAL_OBE, y=OBE_12m, size = TOTAL_OBE, fill=OBE_12m, colour=OBE_12m)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = condition), 
                  colour = "black", 
                  size = 6,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20))+
  scale_colour_viridis_c(option = "D")+
  xlab("\nNumber of individual patient samples")+
  ylab("% share with +5% BMI reduction \n <12 months\n")
# -------


# BMI distribution across cancer types , Diabetes and Obesity ----------------

OBE_BMIs_All_Cancer <- fread("OBE_BMIs_All_Cancer.txt")
DIA_BMIs_All_Cancer <- fread("DIA_BMIs_All_Cancer.txt")

OBE_BMIs_All_Cancer$BMI <- as.numeric(OBE_BMIs_All_Cancer$BMI)
DIA_BMIs_All_Cancer$BMI <- as.numeric(DIA_BMIs_All_Cancer$BMI)



length(unique(DIA_BMIs_All_Cancer$patient)) # 34042
length(unique(OBE_BMIs_All_Cancer$patient)) # 46635

DIA_BMIs_All_Cancer %>% ggplot(aes(BMI))+
  geom_density(fill="deeppink4", colour="deeppink4", alpha=0.8, size=1.5)+
  facet_wrap(~condition)+
  xlim(10,80)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n BMI: All Records from Diabetes Patients")+ylab("Proportion of Patients \n")


DIA_BMIs_All_Cancer %>% group_by(condition) %>% summarise(n=weighted.mean(BMI, weight))


OBE_BMIs_All_Cancer %>% ggplot(aes(BMI))+
  geom_density(fill="deepskyblue4", colour="deepskyblue4", alpha=0.8, size=1.5)+
  facet_wrap(~condition)+
  xlim(10,80)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n BMI: All Records from Obesity Patients")+ylab("Proportion of Patients \n")

OBE_BMIs_All_Cancer %>% group_by(condition) %>% summarise(n=weighted.mean(BMI, weight))



# -----------
# Percentage of patients with metastasis --------------

DIA_OBE_pts_CanDx <- fread("DIA_OBE_pts_CanDx.txt")
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% filter(grepl("Metastasis", diagnosis))
DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% select(ptid) %>% distinct()

DIA_OBE_pts_CanDx <- DIA_OBE_pts_CanDx %>% ungroup()

length(unique(DIA_OBE_pts_CanDx$ptid)) # 18008

names(DIA_OBE_pts_CanDx)[1] <- "patient"


OBE_BMIs_All_Cancer <- fread("OBE_BMIs_All_Cancer.txt")
DIA_BMIs_All_Cancer <- fread("DIA_BMIs_All_Cancer.txt")

OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% select(patient, weight, condition) %>% distinct() 
DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% select(patient, weight, condition) %>% distinct() 

DIA_BMIs_All_Cancer %>% group_by(condition) %>% count()




OBE_BMIs_All_Cancer %>% group_by(condition) %>% count()




DIA_BMIs_All_Cancer %>% inner_join(DIA_OBE_pts_CanDx, by=c("patient"="patient")) %>% group_by(condition) %>% count()


OBE_BMIs_All_Cancer %>% inner_join(DIA_OBE_pts_CanDx, by=c("patient"="patient")) %>% group_by(condition) %>% count()


# -------------
# Pool all BMIs together Diabetes & Obesity with Cancer --------
DIA_Disorder_Histories <- fread("DIA Disorder Histories.txt")
DIA_Pats <- DIA_Disorder_Histories %>% select(patient, weight) %>% distinct()
OBE_Disorder_Histories <- fread("OBE Disorder Histories.txt")
OBE_Pats <- OBE_Disorder_Histories %>% select(patient, weight) %>% distinct()
OBE_Pats <- OBE_Pats %>% anti_join(DIA_Pats)


# BMIs from DANU Measures
DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test == "BMI") %>% select(patid, weight, claimed, value)
names(DANU_Measures)[1] <- "patient"
names(DANU_Measures)[3] <- "Date_BMI"
names(DANU_Measures)[4] <- "BMI"
DANU_Measures$Date_BMI <- as.Date(DANU_Measures$Date_BMI)


# Get BMIs from Events table
DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code))       
DANU_Events$code <- as.character(DANU_Events$code)
DANU_Events$code <- parse_number(DANU_Events$code)
names(DANU_Events)[1] <- "patient"
names(DANU_Events)[3] <- "BMI"
names(DANU_Events)[4] <- "Date"
DANU_Events$Date <- as.Date(DANU_Events$Date)
names(DANU_Events)[4] <- "Date_BMI"
DANU_Events <- DANU_Events %>% select(1,2,4,3)


# BMIs from DANU Measures 3.1 (All DANU)
DANU_Measures_3 <- fread("DANU Measures 3.1.txt")
DANU_Measures_3 <- DANU_Measures_3 %>% filter(test == "BMI") %>% select(patid, weight, claimed, value)
names(DANU_Measures_3)[1] <- "patient"
names(DANU_Measures_3)[3] <- "Date_BMI"
names(DANU_Measures_3)[4] <- "BMI"
DANU_Measures_3$Date_BMI <- as.Date(DANU_Measures_3$Date_BMI)


All_BMIs <- DANU_Events %>% bind_rows(DANU_Measures) %>% bind_rows(DANU_Measures_3) %>% distinct()

DIA_Pats <- DIA_Pats %>% inner_join(All_BMIs)
OBE_Pats <- OBE_Pats %>% inner_join(All_BMIs)

DIA_BMIs_All_Pats <- DIA_Pats
OBE_BMIs_All_Pats <- OBE_Pats

DIA_BMIs_All_Cancer <- fread("DIA_BMIs_All_Cancer.txt")
OBE_BMIs_All_Cancer <- fread("OBE_BMIs_All_Cancer.txt")


DIA_BMIs_All_Pats$group <- "All Diabetes Patients"
OBE_BMIs_All_Pats$group <- "All Obesity Patients"


DIA_BMIs_All_Cancer$group <- "Diabetes Cancer Patients"
OBE_BMIs_All_Cancer$group <- "Obesity Cancer Patients"



DIA_BMIs_All_Pats <- DIA_BMIs_All_Pats %>% group_by(patient) %>% arrange(Date_BMI) %>% filter(Date_BMI==max(Date_BMI)) %>% slice(1)
DIA_BMIs_All_Cancer <- DIA_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI) %>% filter(Date_BMI==max(Date_BMI)) %>% slice(1)

DIA_BMIs_All_Pats <-  DIA_BMIs_All_Pats %>% select(-c(weight, Date_BMI))
DIA_BMIs_All_Cancer <-  DIA_BMIs_All_Cancer %>% select(-c(weight, Date, condition, Date_BMI))



OBE_BMIs_All_Pats <- OBE_BMIs_All_Pats %>% group_by(patient) %>% arrange(Date_BMI) %>% filter(Date_BMI==max(Date_BMI)) %>% slice(1)
OBE_BMIs_All_Cancer <- OBE_BMIs_All_Cancer %>% group_by(patient) %>% arrange(Date_BMI) %>% filter(Date_BMI==max(Date_BMI)) %>% slice(1)

OBE_BMIs_All_Pats <-  OBE_BMIs_All_Pats %>% select(-c(weight, Date_BMI))
OBE_BMIs_All_Cancer <-  OBE_BMIs_All_Cancer %>% select(-c(weight, Date, condition, Date_BMI))






Pooled_DIA <- DIA_BMIs_All_Pats %>% bind_rows(DIA_BMIs_All_Cancer)

Pooled_DIA %>% ungroup() %>% group_by(group) %>% summarise(n=mean(as.numeric(BMI), na.rm=T))


Pooled_OBE <- OBE_BMIs_All_Pats %>% bind_rows(OBE_BMIs_All_Cancer)

Pooled_OBE %>% ungroup() %>% group_by(group) %>% summarise(n=mean(as.numeric(BMI), na.rm=T))



Pooled_DIA %>% filter(BMI>10 & BMI<80) %>% ggplot(aes(x = BMI, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n BMI") + ylab("Diabetes Group \n")



Pooled_OBE %>% filter(BMI>10 & BMI<80) %>% ggplot(aes(x = BMI, y = group, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  geom_density_ridges_gradient( scale = 2,  calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail Probability", option = "D", direction = -1)  +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("\n BMI") + ylab("Obesity Group \n")

# -------
# Try to use weights instead -------
NASH_Extract_NLP_Measurements <- fread("NASH Extract NLP Measurements.txt")

NASH_NLP_Weights <- NASH_Extract_NLP_Measurements %>% filter(measurement_type == "WEIGHT")

NASH_NLP_Weights$measurement_value <- as.numeric(NASH_NLP_Weights$measurement_value)
NASH_NLP_Weights <- NASH_NLP_Weights %>% filter(!is.na(measurement_value))
NASH_NLP_Weights <- NASH_NLP_Weights %>% filter(measurement_detail == "kg" | measurement_detail == "lb")
NASH_NLP_Weights <- NASH_NLP_Weights %>% filter(measurement_value > 30 & measurement_value < 500)
NASH_NLP_Weights <- NASH_NLP_Weights %>% arrange(measurement_detail)
NASH_NLP_Weights <- NASH_NLP_Weights %>% mutate(measurement_value = ifelse(measurement_detail=="lb", measurement_value*0.45359237, measurement_value))
NASH_NLP_Weights <- NASH_NLP_Weights %>% select(patid, weight, note_date, measurement_value)


NASH_NLP_Heights <- NASH_Extract_NLP_Measurements %>% filter(measurement_type == "HEIGHT")
NASH_NLP_Heights <- NASH_NLP_Heights %>% select(patid, weight, note_date, measurement_type, measurement_value, measurement_detail)
NASH_NLP_Heights <- NASH_NLP_Heights %>% filter(measurement_detail == "cm" | measurement_detail == "cm")
NASH_NLP_Heights$measurement_value <- as.numeric(NASH_NLP_Heights$measurement_value)
NASH_NLP_Heights <- NASH_NLP_Heights %>% filter(!is.na(measurement_value))

NASH_NLP_Heights <- NASH_NLP_Heights %>% mutate(measurement_value = ifelse(measurement_detail=="m", measurement_value*100, measurement_value))

range(NASH_NLP_Heights$measurement_value)

NASH_NLP_Heights <- NASH_NLP_Heights %>% filter(measurement_value  >100)

NASH_NLP_Heights <- NASH_NLP_Heights %>% select(patid, weight, note_date, measurement_value)


NASH_NLP_BMIs <- NASH_NLP_Heights %>% inner_join(NASH_NLP_Weights, by=c("patid"="patid", "weight"="weight", "note_date"="note_date")) 

NASH_NLP_BMIs$measurement_value.x <- NASH_NLP_BMIs$measurement_value.x/100

NASH_NLP_BMIs$BMI <- NASH_NLP_BMIs$measurement_value.y/(NASH_NLP_BMIs$measurement_value.x*NASH_NLP_BMIs$measurement_value.x)

names(NASH_NLP_BMIs)[1] <- "patient"
names(NASH_NLP_BMIs)[3] <- "Date_BMI"
names(NASH_NLP_BMIs)[6] <- "BMI"

NASH_NLP_BMIs <- NASH_NLP_BMIs %>% select(1,2,3,6)
