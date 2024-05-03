library(tidyverse)
library(data.table)


DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, heart_failure_onset, heart_failure_condition)
DANU_Demographics <- DANU_Demographics %>% drop_na()

# ALL Heart Failure
sum(DANU_Demographics$weight) # 

# ALL Chronic Failure
DANU_Demographics %>% filter(heart_failure_condition=="Chronic Heart Failure") %>% summarise(n=sum(weight)) # 


DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis == "Heart Failure")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, description)

DANU_Dossiers <- fread("DANU Dossiers.txt")
DANU_Dossiers <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Dossiers) %>% select(patid, weight, code, earliest, latest, frequency)

DANU_Dossiers %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 
DANU_Dossiers <- DANU_Dossiers %>% left_join(DANU_Diagnosis_Codes)
DANU_Dossiers <- DANU_Dossiers %>% drop_na()
DANU_Dossiers %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #

# Must have specified systolic or dyastolic 
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 
  
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patid) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 
  

unique(DANU_Dossiers$description)

# Must have at least 2 different codes
DANU_Dossiers %>%  # filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patid, code) %>% distinct() %>%
  group_by(patid) %>% count() %>% filter(n>1) %>%
  select(patid) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) #

# Must have specified systolic or dyastolic and have at least 2 different codes
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patid, code) %>% distinct() %>%
  group_by(patid) %>% count() %>% filter(n>1) %>%
  select(patid) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 


DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patid, code) %>% distinct() %>%
  group_by(patid) %>% count() %>% filter(n>1) %>%
  select(patid) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>% 
  select(patid, weight) %>% distinct() %>% mutate(Group="Systolic") %>%
  full_join(DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patid, code) %>% distinct() %>%
  group_by(patid) %>% count() %>% filter(n>1) %>%
  select(patid) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>% 
  select(patid, weight) %>% distinct() %>% mutate(Group="Diastolic") ) %>%
  distinct() %>% ungroup() %>%
  mutate(check=1) %>% spread(key=Group, value=check) %>%
  group_by(Diastolic, Systolic) %>% summarise(n=sum(weight))



DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patid) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  select(patid) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>% 
  select(patid, weight) %>% distinct() %>% mutate(Group="Systolic") %>%
  full_join(DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patid) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  select(patid) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>% 
  select(patid, weight) %>% distinct() %>% mutate(Group="Diastolic") ) %>%
  distinct() %>% ungroup() %>%
  mutate(check=1) %>% spread(key=Group, value=check) %>%
  group_by(Diastolic, Systolic) %>% summarise(n=sum(weight))

DANU_Dossiers %>% filter(grepl("ystolic", description)) %>%  select(code, description) %>% distinct()


DANU_Dossiers %>% filter(grepl("iastolic", description)) %>%  select(code, description) %>% distinct()

