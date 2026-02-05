# Overlap between CKD HF and Diabetes -----------

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)

options(scipen = 999)



# Diabetes & Obesity

DANU_Demographics <- fread("DANU Demographics.txt", sep="\t")


Obesity <- DANU_Demographics %>% filter(grepl("Obesity", obesity_condition))  %>% select(patid, weight, age) 
sum(Obesity$weight)

Diabetes <- DANU_Demographics %>% filter(!grepl("-", diabetes_condition))  %>% select(patid, weight, age) 
sum(Diabetes$weight)
Diabetes$DIA <- 1


Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt")
Diabetes <- Diabetes %>% inner_join(Treatment_exp_Vector %>% select(patient), by=c("patid"="patient")) 
sum(Diabetes$weight)








# Heart Failure


HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"

sum(HF_Demographics$weight) # 17277771


HF_Demographics %>% filter(died=="N" | death_date>"2020-05-01" ) %>% summarise(n=sum(weight)) # 14363507

HF_Demographics <- HF_Demographics %>% filter(died=="N" | death_date>"2020-05-01" )

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs != "104")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient) %>% distinct()

HF_Demographics %>% inner_join(HF_Drug_Histories) %>% summarise(n=sum(weight)) # 12987606


HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset, heart_failure_condition)

HF_Demographics <- HF_Demographics %>% drop_na()

# ALL Heart Failure
sum(HF_Demographics$weight) # 14363507

# ALL Chronic Failure
HF_Demographics %>% filter(heart_failure_condition=="Chronic Heart Failure") %>% summarise(n=sum(weight)) # 14363507

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis == "Heart Failure")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, description)

DANU_Dossiers <- fread("DANU Dossiers Full.txt")
names(DANU_Dossiers)[1] <- "patient"

DANU_Dossiers <- HF_Demographics %>% select(patient) %>% inner_join(DANU_Dossiers)

DANU_Dossiers <- DANU_Dossiers %>% select(patient, weight, code, earliest, latest, frequency)

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 14363507

DANU_Dossiers <- DANU_Dossiers %>% left_join(DANU_Diagnosis_Codes)

DANU_Dossiers <- DANU_Dossiers %>% drop_na()

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 14363507

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-"&Drugs!="104")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient) %>% distinct()

HF_Drug_Histories %>% inner_join(DANU_Dossiers) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))  # 12987606

DANU_Dossiers <- HF_Drug_Histories %>% inner_join(DANU_Dossiers) 

# Must have specified systolic or dyastolic 
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 7723035
  
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>19) %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6730993

unique(DANU_Dossiers$description)

# Must have at least 2 different codes
DANU_Dossiers %>%  # filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 4476287

# Must have specified systolic or dyastolic and have at least 2 different codes
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1682791

DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(Group="Systolic") %>%
  full_join(DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(Group="Diastolic") ) %>%
  distinct() %>% ungroup() %>%
  mutate(check=1) %>% spread(key=Group, value=check) %>%
  group_by(Diastolic, Systolic) %>% summarise(n=sum(weight))

#   Diastolic Systolic       n
#       <dbl>    <dbl>   <dbl>
# 1         1        1 508127.
# 2         1       NA 660221.
# 3        NA        1 514443.

DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(Group="Systolic") %>%
  full_join(DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(Group="Diastolic") ) %>%
  distinct() %>% ungroup() %>%
  mutate(check=1) %>% spread(key=Group, value=check) %>%
  group_by(Diastolic, Systolic) %>% summarise(n=sum(weight))

#   Diastolic Systolic        n
#       <dbl>    <dbl>    <dbl>
# 1         1        1 1004342.
# 2         1       NA 3366024.
# 3        NA        1 2360626.

Diastolic_Pats <- DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(Group="Systolic") %>%
  full_join(DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(Group="Diastolic") ) %>%
  distinct() %>% ungroup() %>%
  mutate(check=1) %>% spread(key=Group, value=check) %>%
  filter(Diastolic==1 & is.na(Systolic)) %>% select(patient, weight) %>% distinct()


Systolic_Pats <- DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(Group="Systolic") %>%
  full_join(DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>% 
  select(patient, weight) %>% distinct() %>% mutate(Group="Diastolic") ) %>%
  distinct() %>% ungroup() %>%
  mutate(check=1) %>% spread(key=Group, value=check) %>%
  filter(Systolic==1 & is.na(Diastolic)) %>% select(patient, weight) %>% distinct()


Heart_Failure <- Systolic_Pats %>% full_join(Diastolic_Pats) 

HF_Demographics <- fread("HF Demographics.txt")

Heart_Failure <- Heart_Failure %>% rename("patid"="patient") %>% left_join(HF_Demographics %>% select(patid, weight, age)) %>% distinct() %>% mutate(HF=1)



# CKD
Stages_Complete <- fread("CKD_Stages_Complete_FilledIn.txt", sep="\t")
DANU_Demographics_Full <- fread("DANU Demographics Full.txt", sep="\t")
DANU_Demographics_Full <- DANU_Demographics_Full %>% select(patid, weight, age)
CKD <- Stages_Complete %>% rename("patid"="patient") %>% select(patid) %>% left_join(DANU_Demographics_Full) %>%
  mutate(CKD=1)


CKD %>% summarise(n=sum(weight)) # 13384925
Heart_Failure %>% summarise(n=sum(weight)) # 5726651
Diabetes %>% summarise(n=sum(weight)) # 30625690

CKD %>% inner_join(Obesity %>% select(patid)) %>% summarise(n=sum(weight)) # 1340891
Heart_Failure %>% inner_join(Obesity %>% select(patid)) %>% summarise(n=sum(weight)) # 587065
Diabetes %>% inner_join(Obesity %>% select(patid)) %>% summarise(n=sum(weight)) # 21798678


CKD %>%   filter(age<65) %>% summarise(n=sum(weight)) # 13384925
Heart_Failure %>%   filter(age<65) %>% summarise(n=sum(weight)) # 5726651
Diabetes %>%   filter(age<65) %>% summarise(n=sum(weight)) # 30625690


CKD %>% full_join(Heart_Failure) %>% full_join(Diabetes) %>%
  filter(age>=65) %>%
  group_by(DIA, CKD, HF) %>% summarise(n=sum(weight)) 



CKD %>% full_join(Heart_Failure) %>% full_join(Diabetes) %>%
    filter(age>=65) %>%
  inner_join(Obesity %>% select(patid)) %>%
  group_by(DIA, CKD, HF) %>% summarise(n=sum(weight)) 


# -------

