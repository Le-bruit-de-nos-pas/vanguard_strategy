library(tidyverse)
library(data.table)


DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, heart_failure_onset, heart_failure_condition)
DANU_Demographics <- DANU_Demographics %>% drop_na()
# ALL Heart Failure
sum(DANU_Demographics$weight) # 13,728,269
# ALL Chronic Failure
DANU_Demographics %>% filter(heart_failure_condition=="Chronic Heart Failure") %>% summarise(n=sum(weight)) # 13359838

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis == "Heart Failure")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, description)

DANU_Dossiers <- fread("DANU Dossiers.txt")
DANU_Dossiers <- DANU_Demographics %>% select(patid) %>% left_join(DANU_Dossiers) %>% select(patid, weight, code, earliest, latest, frequency)

DANU_Dossiers %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 13,728,269
DANU_Dossiers <- DANU_Dossiers %>% left_join(DANU_Diagnosis_Codes)
DANU_Dossiers <- DANU_Dossiers %>% drop_na()
DANU_Dossiers %>% select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 13,728,269

# Must have specified systolic or dyastolic 
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 7953975
  
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patid) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6649599
  

unique(DANU_Dossiers$description)

# Must have at least 2 different codes
DANU_Dossiers %>%  # filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patid, code) %>% distinct() %>%
  group_by(patid) %>% count() %>% filter(n>1) %>%
  select(patid) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 4,093,047

# Must have specified systolic or dyastolic and have at least 2 different codes
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patid, code) %>% distinct() %>%
  group_by(patid) %>% count() %>% filter(n>1) %>%
  select(patid) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1542291


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

  Diastolic Systolic       n
      <dbl>    <dbl>   <dbl>
1         1        1 459498.
2         1       NA 603952.
3        NA        1 478841.

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

 1: D=$I5023                                     Acute on chronic systolic (congestive) heart failure
 2: D=$I5022                                              Chronic systolic (congestive) heart failure
 3: D=$I5020                                          Unspecified systolic (congestive) heart failure
 4: D=$I5021                                                Acute systolic (congestive) heart failure
 5: D=$I5041            Acute combined systolic (congestive) and diastolic (congestive) heart failure
 6: D=$I5043 Acute on chronic combined systolic (congestive) and diastolic (congestive) heart failure
 7: D=$I5042          Chronic combined systolic (congestive) and diastolic (congestive) heart failure
 8: D=$I5040      Unspecified combined systolic (congestive) and diastolic (congestive) heart failure
 9:  D=42843                           Acute on chronic combined systolic and diastolic heart failure
10:  D=42823                                                  Acute on chronic systolic heart failure
11:  D=42822                                                           Chronic systolic heart failure
12:  D=42821                                                             Acute systolic heart failure
13:  D=42820                                                      Systolic heart failure, unspecified
14:  D=42841                                      Acute combined systolic and diastolic heart failure
15:  D=42842                                    Chronic combined systolic and diastolic heart failure

DANU_Dossiers %>% filter(grepl("iastolic", description)) %>%  select(code, description) %>% distinct()

 1: D=$I5033                                    Acute on chronic diastolic (congestive) heart failure
 2: D=$I5032                                             Chronic diastolic (congestive) heart failure
 3: D=$I5031                                               Acute diastolic (congestive) heart failure
 4: D=$I5030                                         Unspecified diastolic (congestive) heart failure
 5: D=$I5041            Acute combined systolic (congestive) and diastolic (congestive) heart failure
 6: D=$I5043 Acute on chronic combined systolic (congestive) and diastolic (congestive) heart failure
 7: D=$I5042          Chronic combined systolic (congestive) and diastolic (congestive) heart failure
 8: D=$I5040      Unspecified combined systolic (congestive) and diastolic (congestive) heart failure
 9:  D=42843                           Acute on chronic combined systolic and diastolic heart failure
10:  D=$I503                                                     Diastolic (congestive) heart failure
11:  D=42833                                                 Acute on chronic diastolic heart failure
12:  D=42832                                                          Chronic diastolic heart failure
13:  D=42841                                      Acute combined systolic and diastolic heart failure
14:  D=42830                                                     Diastolic heart failure, unspecified
15:  D=42831                                                            Acute diastolic heart failure
16:  D=42842                                    Chronic combined systolic and diastolic heart failure