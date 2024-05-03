library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)
library(randomForest)
library(DALEX)
library(gbm)
library(readxl)


# Heart Failure Sizing ------------

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



# -----
# Estimate Sizing with All DANU patients ------------------------------------------------------
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

#   Diastolic Systolic       n
#       <dbl>    <dbl>   <dbl>
# 1         1        1 459498.
# 2         1       NA 603952.
# 3        NA        1 478841.

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

#  1: D=$I5023                                     Acute on chronic systolic (congestive) heart failure
#  2: D=$I5022                                              Chronic systolic (congestive) heart failure
#  3: D=$I5020                                          Unspecified systolic (congestive) heart failure
#  4: D=$I5021                                                Acute systolic (congestive) heart failure
#  5: D=$I5041            Acute combined systolic (congestive) and diastolic (congestive) heart failure
#  6: D=$I5043 Acute on chronic combined systolic (congestive) and diastolic (congestive) heart failure
#  7: D=$I5042          Chronic combined systolic (congestive) and diastolic (congestive) heart failure
#  8: D=$I5040      Unspecified combined systolic (congestive) and diastolic (congestive) heart failure
#  9:  D=42843                           Acute on chronic combined systolic and diastolic heart failure
# 10:  D=42823                                                  Acute on chronic systolic heart failure
# 11:  D=42822                                                           Chronic systolic heart failure
# 12:  D=42821                                                             Acute systolic heart failure
# 13:  D=42820                                                      Systolic heart failure, unspecified
# 14:  D=42841                                      Acute combined systolic and diastolic heart failure
# 15:  D=42842                                    Chronic combined systolic and diastolic heart failure



DANU_Dossiers %>% filter(grepl("iastolic", description)) %>%  select(code, description) %>% distinct()

#  1: D=$I5033                                    Acute on chronic diastolic (congestive) heart failure
#  2: D=$I5032                                             Chronic diastolic (congestive) heart failure
#  3: D=$I5031                                               Acute diastolic (congestive) heart failure
#  4: D=$I5030                                         Unspecified diastolic (congestive) heart failure
#  5: D=$I5041            Acute combined systolic (congestive) and diastolic (congestive) heart failure
#  6: D=$I5043 Acute on chronic combined systolic (congestive) and diastolic (congestive) heart failure
#  7: D=$I5042          Chronic combined systolic (congestive) and diastolic (congestive) heart failure
#  8: D=$I5040      Unspecified combined systolic (congestive) and diastolic (congestive) heart failure
#  9:  D=42843                           Acute on chronic combined systolic and diastolic heart failure
# 10:  D=$I503                                                     Diastolic (congestive) heart failure
# 11:  D=42833                                                 Acute on chronic diastolic heart failure
# 12:  D=42832                                                          Chronic diastolic heart failure
# 13:  D=42841                                      Acute combined systolic and diastolic heart failure
# 14:  D=42830                                                     Diastolic heart failure, unspecified
# 15:  D=42831                                                            Acute diastolic heart failure
# 16:  D=42842                                    Chronic combined systolic and diastolic heart failure


# -------------------------------------------------------------------------------------------------------------
# Core 6 - HF drug-treated patients (last 5 years) --------------------------------------------------------------------

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

sum(as.numeric(HF_Drug_Histories$weight)) # 13359838 (close to what we had with all DANU, 13 million)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 12041998 treat-experienced

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

unique(HF_Ingredients$drug_class)

HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="MRA"|
                            drug_class=="ACE"|
                            drug_class=="Beta Blocker"|
                            drug_class=="ARB"|
                            drug_class=="ARNI"|
                            drug_class=="SGLT2")

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
unique(HF_Ingredients$drug_class)


string_Short_Drugs        <- paste0("\\b(",paste0(HF_Ingredients$molecule, collapse = "|"),")\\b")

HF_Drug_Histories <- HF_Drug_Histories %>% filter(grepl(string_Short_Drugs, Drugs))

HF_Drug_Histories <- HF_Drug_Histories %>% arrange(patient, weight, Month, Drugs) %>%
  group_by(patient, weight, Month) %>% mutate(treat_new = paste(Drugs, collapse=",")) 

HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(patient, weight, Month, treat_new) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% spread(key=Month, value=treat_new)

HF_Drug_Histories[is.na(HF_Drug_Histories)] <- "-"

sum(as.numeric(HF_Drug_Histories$weight)) # 11146437 (with any of those 6 classes) 
# [11146437/12041998 = 0.9256302 of treat-exp]
# [11146437/13359838 = 0.8343243 of All HF]

fwrite(HF_Drug_Histories, "HF Drug Histories 6classes only.txt", sep="\t")

# ------------------------------------------


# Core 6 -  Drug Usage Core 6 -----------------------------------------------------

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 11146437

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="MRA"|
                            drug_class=="ACE"|
                            drug_class=="Beta Blocker"|
                            drug_class=="ARB"|
                            drug_class=="ARNI"|
                            drug_class=="SGLT2")

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 7547588

HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))



HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(patient, weight) %>% count() %>%
  ungroup() %>%
  group_by(n) %>% summarise(n=sum(as.numeric(weight)))



# No of lines of therapy

HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")

sum(as.numeric(HF_Drug_Histories$weight)) # 11146437

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>%
   summarise(total=sum(as.numeric(weight)))
  
HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(n=weighted.mean(n,as.numeric(weight))) # 2.85

HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>% 
  summarise(n=weighted.median(n,as.numeric(weight))) # 1.5

HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  mutate(n=ifelse(n>=10,10,n)) %>% distinct() %>%
  group_by(n) %>% summarise(total=sum(as.numeric(weight)))


# Class penetrance vs LoT

# No of lines of therapy
HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 11146437

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

LoT <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() 

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 11146437

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="MRA"|
                            drug_class=="ACE"|
                            drug_class=="Beta Blocker"|
                            drug_class=="ARB"|
                            drug_class=="ARNI"|
                            drug_class=="SGLT2")

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 7547588

LoT <- LoT %>% left_join(HF_Drug_Histories) %>% select(patient, weight, n, drug_class) %>% distinct() %>% 
  mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                           ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                  ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  distinct() %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"none", drug_class)) %>%
  mutate(Treat=1) %>%
  spread(key=drug_class, value=Treat)


LoT %>% mutate(n=ifelse(n>=8,8,n)) %>% select(patient, weight, n) %>% distinct() %>% group_by(n) %>% summarise(totalpats=sum(as.numeric(weight)))
 
data.frame(
  LoT %>% mutate(n=ifelse(n>=8,8,n)) %>% group_by(n, `ACE|ARB|ARNI`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`ACE|ARB|ARNI`="totalpats") %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=8,8,n)) %>% group_by(n, `Beta Blocker`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`Beta Blocker`="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=8,8,n)) %>% group_by(n, MRA) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("MRA"="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=8,8,n)) %>% group_by(n, SGLT2  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("SGLT2"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=8,8,n)) %>% group_by(n, none  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("none"  ="totalpats"))
  )


LoT %>% group_by(n, `Beta Blocker`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`Beta Blocker`="totalpats")

LoT %>% group_by(n, MRA  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("MRA"  ="totalpats")

LoT %>% group_by(n, SGLT2  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("SGLT2"  ="totalpats")

# ---------------------------------------------
# Core 6 -  Class Penetrance 12 months before and after 1st Dx --------------------------------------------------

HF_Demographics <- fread("HF Demographics.txt", colClasses = "character")

HF_Demographics <- HF_Demographics %>% select(patid, weight, heart_failure_onset)

HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))

HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- paste0(Months_lookup$Month,"-1")

Months_lookup$Month <- as.Date(Months_lookup$Month)

Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patid, Exact_Month) %>% distinct()
names(HF_Demographics)[1] <- "patient"

HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")

HF_Demographics <- HF_Drug_Histories %>% select(patient) %>% inner_join(HF_Demographics)

HF_Demographics %>%
  ggplot(aes(Exact_Month)) +
  geom_density(fill="darkslategray4", colour="darkslategray", size=2, alpha=0.5) +
  theme_minimal() +
  xlab("\n Month of 1st Heart Failure Dx") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

names(HF_Demographics)[2] <- "First_HF_Dx"


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

#HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="MRA"|
                            drug_class=="ACE"|
                            drug_class=="Beta Blocker"|
                            drug_class=="ARB"|
                            drug_class=="ARNI"|
                            drug_class=="SGLT2")

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)

unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.character(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 11146437

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class))))

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)

HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Demographics) %>% 
  mutate(Lapsed=Month-First_HF_Dx) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(HF_Drug_Histories)

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 5029500

HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  rename("Drug Class"="drug_class") %>%
  mutate(n=n/5029500) %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Class`)) +
  geom_line(size=2, alpha=0.8) +
  theme_minimal() +
  scale_colour_viridis_d() +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % \n")


HF_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))



HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
    mutate(n=ifelse(drug_class=="ACE|ARB|ARNI", n/3573710,
                    ifelse(drug_class=="BEta Blocker", n/3615376,
                           ifelse(drug_class=="MRA",n/666444,
                                  ifelse(drug_class=="SGLT2",n/191942,n/3185265))))) %>%
  rename("Drug Class"="drug_class") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Class`)) +
  geom_line(size=2, alpha=0.8) +
  theme_minimal() +
  scale_colour_viridis_d() +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % (of Class-experienced) \n")

# ------------------------
# Core 6 -  Check All possible combinations of drug classes ---------------------------------------------

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

sum(as.numeric(HF_Drug_Histories$weight)) # 13359838 (close to what we had with all DANU, 13 million)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 12041998 treat-experienced

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

unique(HF_Ingredients$drug_class)

HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="MRA"|
                            drug_class=="ACE"|
                            drug_class=="Beta Blocker"|
                            drug_class=="ARB"|
                            drug_class=="ARNI"|
                            drug_class=="SGLT2")

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
unique(HF_Ingredients$drug_class)


string_Short_Drugs        <- paste0("\\b(",paste0(HF_Ingredients$molecule, collapse = "|"),")\\b")

HF_Drug_Histories <- HF_Drug_Histories %>% filter(grepl(string_Short_Drugs, Drugs))

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients) %>% mutate(Drugs=ifelse(drug_class=="Beta Blocker","B",
                                                                        ifelse(drug_class=="MRA", "M",
                                                                               ifelse(drug_class=="SGLT2", "S", "A")))) %>%
  select(patient, weight, Month, Drugs) %>% distinct() %>% 
           arrange(patient, weight, Month, Drugs) %>%
           group_by(patient, weight, Month) %>% mutate(treat_new = paste(Drugs, collapse=",")) 


HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(patient, weight, Month, treat_new) %>% distinct()

data.frame(unique(HF_Drug_Histories$treat_new))

#    unique.HF_Drug_Histories.treat_new.
# 1                                    A
# 2                                    B
# 3                                  A,B
# 4                                A,B,M
# 5                                  B,M
# 6                                    M
# 7                                  A,M
# 8                                    S
# 9                                A,B,S
# 10                                 A,S
# 11                                 B,S
# 12                               B,M,S
# 13                               A,M,S
# 14                                 M,S
# 15                             A,B,M,S

length(unique(HF_Drug_Histories$patient))

HF_Drug_Histories %>% group_by(treat_new) %>% summarise(n=sum(as.numeric(weight))) %>% arrange(-n)

#    treat_new          n
#    <chr>          <dbl>
#  1 A,B       153165804.
#  2 A         116727619.
#  3 B         108150570.
#  4 A,B,M      21031722.
#  5 B,M         9437094.
#  6 M           5818283.
#  7 A,M         4649002.
#  8 A,B,S       4418624.
#  9 A,S         2639246.
# 10 S           1839795.
# 11 B,S         1417002.
# 12 A,B,M,S      843497.
# 13 B,M,S        203143.
# 14 A,M,S        157332.
# 15 M,S          104455.

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 11146437

HF_Drug_Histories <- HF_Drug_Histories %>% spread(key=Month, value=treat_new)

HF_Drug_Histories[is.na(HF_Drug_Histories)] <- "-"

fwrite(HF_Drug_Histories, "HF Drug Histories 4 Classes Combinations.txt", sep="\t")

# ------------------------------------------


# Core 6  - All Drugs-  Drug Usage -----------------------------------------------------

HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
Core6 <- HF_Drug_Histories %>% select(patient)

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 8104813

HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))

# 1 Advanced Therapy   1268603.
# 2 Cardiac Device       46504.
# 3 Hospitalization      73687.
# 4 Injectable Therapy   56505.
# 5 Oral Therapy       7976564.

HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))

#  1 ACE|ARB|ARNI       5034727.
#  2 Beta Blocker       5559347.
#  3 Cardiac Device       46504.
#  4 Diuretic           4194939.
#  5 Heart Transplant       251.
#  6 Hospital Inpatient   60128.
#  7 Inotropic           259057.
#  8 MRA                 931039.
#  9 Other               141345.
# 10 SGLT2               362556.
# 11 Surgery Inpatient    13307.
# 12 Vasodilator         924415.

HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(patient, weight) %>% count() %>%
  ungroup() %>%
  group_by(n) %>% summarise(n=sum(as.numeric(weight)))



# No of lines of therapy

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

sum(as.numeric(HF_Drug_Histories$weight)) # 11146437

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>%
   summarise(total=sum(as.numeric(weight))) # 11146437
  
HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(n=weighted.mean(n,as.numeric(weight))) # 5.19

HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>% 
  summarise(n=weighted.median(n,as.numeric(weight))) # 3.5

HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  mutate(n=ifelse(n>=15,15,n)) %>% distinct() %>%
  group_by(n) %>% summarise(total=sum(as.numeric(weight)))


HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  ggplot(aes(n)) +
  geom_density(fill="turquoise4", colour="turquoise4", size=1, alpha=0.7) +
  theme_minimal() +
  xlim(0,20) +
  xlab("\n No. Therapy Lines Experienced up to April 21") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

# Class penetrance vs LoT

# No of lines of therapy
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

sum(as.numeric(HF_Drug_Histories$weight)) # 11146437

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

LoT <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() 

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

sum(as.numeric(HF_Drug_Histories$weight)) # 11146437

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 8104813

LoT <- LoT %>% left_join(HF_Drug_Histories) %>% select(patient, weight, n, drug_class) %>% distinct() %>% 
  mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                           ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                  ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  distinct() %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"none", drug_class)) %>%
  mutate(Treat=1) %>%
  spread(key=drug_class, value=Treat)


LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% select(patient, weight, n) %>% distinct() %>% group_by(n) %>% summarise(totalpats=sum(as.numeric(weight)))
 
names(LoT)

data.frame(
  LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `ACE|ARB|ARNI`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`ACE|ARB|ARNI`="totalpats") %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Beta Blocker`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`Beta Blocker`="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Cardiac Device`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Cardiac Device"="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Diuretic  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Diuretic"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Heart Transplant`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Heart Transplant"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Hospital Inpatient`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Hospital Inpatient"  ="totalpats"))  %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Inotropic  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Inotropic"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, MRA ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("MRA"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, SGLT2  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("SGLT2"  ="totalpats"))  %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Surgery Inpatient`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Surgery Inpatient"  ="totalpats"))  %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Vasodilator  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Vasodilator"  ="totalpats"))  %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, none  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("none"  ="totalpats"))
  )



# ---------------------------------------------
# Core 6  - All Drugs-  Drug Usage - EVER TRIED -----------------------------------------------------

HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
Core6 <- HF_Drug_Histories %>% select(patient)

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 11146437

HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))
 
# 1 Advanced Therapy    3711068.
# 2 Cardiac Device      1729878.
# 3 Hospitalization     1288466.
# 4 Injectable Therapy  1386973.
# 5 Oral Therapy       11139745.

HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))

#  1 ACE|ARB|ARNI       9018950.
#  2 Beta Blocker       9278918.
#  3 Cardiac Device     1729878.
#  4 Diuretic           9469885.
#  5 Heart Transplant     23030.
#  6 Hospital Inpatient  996300.
#  7 Inotropic           909882.
#  8 MRA                2150205.
#  9 Other               740149.
# 10 SGLT2               766696.
# 11 Surgery Inpatient   477405.
# 12 Vasodilator        2671426.



# ---------------------------------------------
# Core 6 - All Drugs - Class Penetrance 12 months before and after 1st Dx --------------------------------------------------

HF_Demographics <- fread("HF Demographics.txt", colClasses = "character")

HF_Demographics <- HF_Demographics %>% select(patid, weight, heart_failure_onset)

HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))

HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- paste0(Months_lookup$Month,"-1")

Months_lookup$Month <- as.Date(Months_lookup$Month)

Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patid, Exact_Month) %>% distinct()
names(HF_Demographics)[1] <- "patient"

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

HF_Demographics <- HF_Drug_Histories %>% select(patient) %>% inner_join(HF_Demographics)

HF_Demographics %>%
  ggplot(aes(Exact_Month)) +
  geom_density(fill="darkslategray4", colour="darkslategray", size=1, alpha=0.5) +
  theme_minimal() +
  xlab("\n Month of 1st Heart Failure Dx") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

names(HF_Demographics)[2] <- "First_HF_Dx"


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

#HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))


HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)

unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.character(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 11146437

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class))))

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)

HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Demographics) %>% 
  mutate(Lapsed=Month-First_HF_Dx) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(HF_Drug_Histories)

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 5029500

HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  rename("Drug Class"="drug_class") %>%
  mutate(n=n/5029500) %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Class`, linetype=`Drug Class`)) +
  geom_line(size=1, alpha=.8) +
  theme_minimal() +
  scale_colour_viridis_d(option="D") +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % \n")


HF_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))



HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
    mutate(n=ifelse(drug_class=="ACE|ARB|ARNI", n/3573710,
                    ifelse(drug_class=="BEta Blocker", n/3615376,
                           ifelse(drug_class=="MRA",n/666444,
                                  ifelse(drug_class=="SGLT2",n/191942,
                                         ifelse(drug_class=="Cardiac Device", n/594108,
                                                ifelse(drug_class=="Diuretic", n/3270191,
                                                       ifelse(drug_class=="Heart Transplant", n/3745,
                                                              ifelse(drug_class=="Hospital Inpatient", n/359959,
                                                                     ifelse(drug_class=="Inotropic", n/255783,
                                                                            ifelse(drug_class=="Other", n/200007,
                                                                                   ifelse(drug_class=="Surgery Inpatient", n/175572,
                                                                                          ifelse(drug_class=="Vasodilator", n/862319,n/2910572))))))))))))) %>%
  rename("Drug Class"="drug_class") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Class`, linetype=`Drug Class`)) +
  geom_line(size=1, alpha=1) +
  theme_minimal() +
  scale_colour_viridis_d(option="D") +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % (of Class-experienced) \n")

# ------------------------


# All Exp - All Drugs-  Drug Usage -----------------------------------------------------

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 8334664

HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))

#   drug_group                n
#   <chr>                 <dbl>
# 1 Advanced Therapy   1311772.
# 2 Cardiac Device       51329.
# 3 Hospitalization      82303.
# 4 Injectable Therapy   62277.
# 5 Oral Therapy       8160748.

HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))

#    drug_class                n
#    <chr>                 <dbl>
#  1 ACE|ARB|ARNI       5034727.
#  2 Beta Blocker       5559347.
#  3 Cardiac Device       51329.
#  4 Diuretic           4384895.
#  5 Heart Transplant       497.
#  6 Hospital Inpatient   66454.
#  7 Inotropic           269015.
#  8 MRA                 931039.
#  9 Other               158207.
# 10 SGLT2               362556.
# 11 Surgery Inpatient    15352.
# 12 Vasodilator         941610.

HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(patient, weight) %>% count() %>%
  ungroup() %>%
  group_by(n) %>% summarise(n=sum(as.numeric(weight)))



# No of lines of therapy

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>%
   summarise(total=sum(as.numeric(weight))) # 12041998
  
HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(n=weighted.mean(n,as.numeric(weight))) # 4.92

HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>% 
  summarise(n=weighted.median(n,as.numeric(weight))) # 1.5

HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  mutate(n=ifelse(n>=15,15,n)) %>% distinct() %>%
  group_by(n) %>% summarise(total=sum(as.numeric(weight)))


HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  ggplot(aes(n)) +
  geom_density(fill="turquoise4", colour="turquoise4", size=1, alpha=0.7) +
  theme_minimal() +
  xlim(0,20) +
  xlab("\n No. Therapy Lines Experienced up to April 21") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

# Class penetrance vs LoT

# No of lines of therapy
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

LoT <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() 

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 8334664

LoT <- LoT %>% left_join(HF_Drug_Histories) %>% select(patient, weight, n, drug_class) %>% distinct() %>% 
  mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                           ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                  ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  distinct() %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"none", drug_class)) %>%
  mutate(Treat=1) %>%
  spread(key=drug_class, value=Treat)


LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% select(patient, weight, n) %>% distinct() %>% group_by(n) %>% summarise(totalpats=sum(as.numeric(weight)))
 
names(LoT)

data.frame(
  LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `ACE|ARB|ARNI`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`ACE|ARB|ARNI`="totalpats") %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Beta Blocker`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`Beta Blocker`="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Cardiac Device`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Cardiac Device"="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Diuretic  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Diuretic"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Heart Transplant`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Heart Transplant"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Hospital Inpatient`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Hospital Inpatient"  ="totalpats"))  %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Inotropic  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Inotropic"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, MRA ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("MRA"  ="totalpats")) %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, SGLT2  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("SGLT2"  ="totalpats"))  %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Surgery Inpatient`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Surgery Inpatient"  ="totalpats"))  %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Vasodilator  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Vasodilator"  ="totalpats"))  %>%
  full_join(
    LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, none  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("none"  ="totalpats"))
  )



# ---------------------------------------------
# All Exp - All Drugs-  Drug Usage - EVER TRIED -----------------------------------------------------


# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 11146437

HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))
 
# 1 Advanced Therapy    3711068.
# 2 Cardiac Device      1729878.
# 3 Hospitalization     1288466.
# 4 Injectable Therapy  1386973.
# 5 Oral Therapy       11139745.

HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))

#  1 ACE|ARB|ARNI       9018950.
#  2 Beta Blocker       9278918.
#  3 Cardiac Device     1729878.
#  4 Diuretic           9469885.
#  5 Heart Transplant     23030.
#  6 Hospital Inpatient  996300.
#  7 Inotropic           909882.
#  8 MRA                2150205.
#  9 Other               740149.
# 10 SGLT2               766696.
# 11 Surgery Inpatient   477405.
# 12 Vasodilator        2671426.



# -----------------------------------------------------------------------------
# All Exp - All Drugs - Class Penetrance 12 months before and after 1st Dx --------------------------------------------------

HF_Demographics <- fread("HF Demographics.txt", colClasses = "character")

HF_Demographics <- HF_Demographics %>% select(patid, weight, heart_failure_onset)

HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))

HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- paste0(Months_lookup$Month,"-1")

Months_lookup$Month <- as.Date(Months_lookup$Month)

Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patid, Exact_Month) %>% distinct()
names(HF_Demographics)[1] <- "patient"

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Demographics <- HF_Drug_Histories %>% select(patient) %>% inner_join(HF_Demographics)

HF_Demographics %>%
  ggplot(aes(Exact_Month)) +
  geom_density(fill="darkslategray4", colour="darkslategray", size=1, alpha=0.5) +
  theme_minimal() +
  xlab("\n Month of 1st Heart Failure Dx") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

names(HF_Demographics)[2] <- "First_HF_Dx"


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

#HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))


HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)

unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.character(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 13359838

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class))))

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)

HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Demographics) %>% 
  mutate(Lapsed=Month-First_HF_Dx) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(HF_Drug_Histories)

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 6039278

HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  rename("Drug Class"="drug_class") %>%
  mutate(n=n/6039278) %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Class`, linetype=`Drug Class`)) +
  geom_line(size=1, alpha=.8) +
  theme_minimal() +
  scale_colour_viridis_d(option="D") +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % \n")


HF_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))



HF_Drug_Histories %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
    mutate(n=ifelse(drug_class=="ACE|ARB|ARNI", n/3573710,
                    ifelse(drug_class=="BEta Blocker", n/3615376,
                           ifelse(drug_class=="MRA",n/666444,
                                  ifelse(drug_class=="SGLT2",n/191942,
                                         ifelse(drug_class=="Cardiac Device", n/649701,
                                                ifelse(drug_class=="Diuretic", n/3478836,
                                                       ifelse(drug_class=="Heart Transplant", n/4007,
                                                              ifelse(drug_class=="Hospital Inpatient", n/407038,
                                                                     ifelse(drug_class=="Inotropic", n/271544,
                                                                            ifelse(drug_class=="Other", n/229509,
                                                                                   ifelse(drug_class=="Surgery Inpatient", n/196300,
                                                                                          ifelse(drug_class=="Vasodilator", n/890341,n/3185265))))))))))))) %>%
  rename("Drug Class"="drug_class") %>%
  ggplot(aes(Lapsed,n*100, colour=`Drug Class`, linetype=`Drug Class`)) +
  geom_line(size=1, alpha=1) +
  theme_minimal() +
  scale_colour_viridis_d(option="D") +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % (of Class-experienced) \n")

# ------------------------


# Estimate Sizing Core 6 Systolic vs Diastolic ------------------------------------------------------
HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"

HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
Core6 <- HF_Drug_Histories %>% select(patient)

HF_Demographics <- Core6 %>% inner_join(HF_Demographics)

HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset, heart_failure_condition)

HF_Demographics <- HF_Demographics %>% drop_na()

# ALL Heart Failure
sum(HF_Demographics$weight) # 11146437

# ALL Chronic Failure
HF_Demographics %>% filter(heart_failure_condition=="Chronic Heart Failure") %>% summarise(n=sum(weight)) # 11146437

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis == "Heart Failure")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, description)

DANU_Dossiers <- fread("DANU Dossiers.txt")
names(DANU_Dossiers)[1] <- "patient"

DANU_Dossiers <- HF_Demographics %>% select(patient) %>% inner_join(DANU_Dossiers)

DANU_Dossiers <- DANU_Dossiers %>% select(patient, weight, code, earliest, latest, frequency)

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 11146437

DANU_Dossiers <- DANU_Dossiers %>% left_join(DANU_Diagnosis_Codes)

DANU_Dossiers <- DANU_Dossiers %>% drop_na()

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 11146437

# Must have specified systolic or dyastolic 
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6587056
  
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>20) %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 5707160

unique(DANU_Dossiers$description)

# Must have at least 2 different codes
DANU_Dossiers %>%  # filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 3698594

# Must have specified systolic or dyastolic and have at least 2 different codes
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1398557

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
# 1         1        1 420255.
# 2         1       NA 533466.
# 3        NA        1 444836.

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
# 1         1        1  838244.
# 2         1       NA 2808275.
# 3        NA        1 2060640.



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
  filter(Diastolic==1 & is.na(Systolic)) %>% select(patient) %>% distinct()


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
  filter(Systolic==1 & is.na(Diastolic)) %>% select(patient) %>% distinct()



DANU_Dossiers %>% filter(grepl("ystolic", description)) %>%  select(code, description) %>% distinct()

#  1: D=$I5020                                          Unspecified systolic (congestive) heart failure
#  2: D=$I5023                                     Acute on chronic systolic (congestive) heart failure
#  3: D=$I5022                                              Chronic systolic (congestive) heart failure
#  4: D=$I5021                                                Acute systolic (congestive) heart failure
#  5: D=$I5043 Acute on chronic combined systolic (congestive) and diastolic (congestive) heart failure
#  6: D=$I5042          Chronic combined systolic (congestive) and diastolic (congestive) heart failure
#  7: D=$I5041            Acute combined systolic (congestive) and diastolic (congestive) heart failure
#  8: D=$I5040      Unspecified combined systolic (congestive) and diastolic (congestive) heart failure
#  9:  D=42843                           Acute on chronic combined systolic and diastolic heart failure
# 10:  D=42823                                                  Acute on chronic systolic heart failure
# 11:  D=42820                                                      Systolic heart failure, unspecified
# 12:  D=42822                                                           Chronic systolic heart failure
# 13:  D=42821                                                             Acute systolic heart failure

DANU_Dossiers %>% filter(grepl("iastolic", description)) %>%  select(code, description) %>% distinct()

#  1: D=$I5033                                    Acute on chronic diastolic (congestive) heart failure
#  2: D=$I5032                                             Chronic diastolic (congestive) heart failure
#  3: D=$I5031                                               Acute diastolic (congestive) heart failure
#  4: D=$I5030                                         Unspecified diastolic (congestive) heart failure
#  5: D=$I5041            Acute combined systolic (congestive) and diastolic (congestive) heart failure
#  6: D=$I5043 Acute on chronic combined systolic (congestive) and diastolic (congestive) heart failure
#  7: D=$I5042          Chronic combined systolic (congestive) and diastolic (congestive) heart failure
#  8: D=$I5040      Unspecified combined systolic (congestive) and diastolic (congestive) heart failure
#  9:  D=42843                           Acute on chronic combined systolic and diastolic heart failure
# 10:  D=$I503                                                     Diastolic (congestive) heart failure
# 11:  D=42833                                                 Acute on chronic diastolic heart failure
# 12:  D=42832                                                          Chronic diastolic heart failure
# 13:  D=42841                                      Acute combined systolic and diastolic heart failure
# 14:  D=42830                                                     Diastolic heart failure, unspecified
# 15:  D=42831                                                            Acute diastolic heart failure
# 16:  D=42842                                    Chronic combined systolic and diastolic heart failure


# --------------------------------------------------
# Core 6  - All Drugs-  Drug Usage - Systolic vs Dyastolic -----------------------------------------------------
Diastolic_Pats$group <- "Diastolic"
Systolic_Pats$group <- "Systolic"

Groups <- Diastolic_Pats %>% bind_rows(Systolic_Pats)

HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
Core6 <- HF_Drug_Histories %>% select(patient)

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(Groups) %>%  group_by(group) %>% summarise(n=sum(as.numeric(weight))) # 11146437


# 1 Diastolic 2117370.
# 2 Systolic  1551070.
# 3 NA        4436373.


HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_group) %>% summarise(n=sum(as.numeric(weight)))
 
#  1 Diastolic Advanced Therapy    372628.
#  2 Diastolic Cardiac Device       11150.
#  3 Diastolic Hospitalization      36409.
#  4 Diastolic Injectable Therapy   20123.
#  5 Diastolic Oral Therapy       2078123.
#  6 Systolic  Advanced Therapy    273144.
#  7 Systolic  Cardiac Device       15637.
#  8 Systolic  Hospitalization      19705.
#  9 Systolic  Injectable Therapy   14458.
# 10 Systolic  Oral Therapy       1529287.
# 11 NA        Advanced Therapy    622832.
# 12 NA        Cardiac Device       19717.
# 13 NA        Hospitalization      17573.
# 14 NA        Injectable Therapy   21925.
# 15 NA        Oral Therapy       4369154.

data.frame(HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight))))

# 1  Diastolic       ACE|ARB|ARNI 1181042.39
# 2  Diastolic       Beta Blocker 1415225.15
# 3  Diastolic     Cardiac Device   11149.51
# 4  Diastolic           Diuretic 1275468.40
# 5  Diastolic   Heart Transplant     141.89
# 6  Diastolic Hospital Inpatient   30362.27
# 7  Diastolic          Inotropic   58105.68
# 8  Diastolic                MRA  218865.40
# 9  Diastolic              Other   45782.87
# 10 Diastolic              SGLT2   71945.77
# 11 Diastolic  Surgery Inpatient    5904.66
# 12 Diastolic        Vasodilator  285158.98
# 13  Systolic       ACE|ARB|ARNI 1032899.26
# 14  Systolic       Beta Blocker 1242768.56
# 15  Systolic     Cardiac Device   15637.19
# 16  Systolic           Diuretic  801596.42
# 17  Systolic   Heart Transplant     109.09
# 18  Systolic Hospital Inpatient   15971.69
# 19  Systolic          Inotropic   79989.94
# 20  Systolic                MRA  315440.96
# 21  Systolic              Other   30986.00
# 22  Systolic              SGLT2   89761.40
# 23  Systolic  Surgery Inpatient    3624.38
# 24  Systolic        Vasodilator  176959.84
# 25      <NA>       ACE|ARB|ARNI 2820785.53
# 26      <NA>       Beta Blocker 2901353.36
# 27      <NA>     Cardiac Device   19717.09
# 28      <NA>           Diuretic 2117874.63
# 29      <NA> Hospital Inpatient   13794.35
# 30      <NA>          Inotropic  120961.19
# 31      <NA>                MRA  396732.35
# 32      <NA>              Other   64576.30
# 33      <NA>              SGLT2  200848.73
# 34      <NA>  Surgery Inpatient    3778.34
# 35      <NA>        Vasodilator  462296.40
# ----------------------------------------------------------------------
# Core 6  - All Drugs-  Drug Usage EVER TRIED Systolic vs Dyastolic -----------------------------------------------------
Diastolic_Pats$group <- "Diastolic"
Systolic_Pats$group <- "Systolic"

Groups <- Diastolic_Pats %>% bind_rows(Systolic_Pats)

HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
Core6 <- HF_Drug_Histories %>% select(patient)

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(Groups) %>%  group_by(group) %>% summarise(n=sum(as.numeric(weight))) # 11146437

#   group            n
#   <chr>        <dbl>
# 1 Diastolic 2808275.
# 2 Systolic  2060640.
# 3 NA        6277522.


HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_group) %>% summarise(n=sum(as.numeric(weight)))
 
#  1 Diastolic Advanced Therapy   1060408.
#  2 Diastolic Cardiac Device      388506.
#  3 Diastolic Hospitalization     529918.
#  4 Diastolic Injectable Therapy  471939.
#  5 Diastolic Oral Therapy       2807321.
#  6 Systolic  Advanced Therapy    776178.
#  7 Systolic  Cardiac Device      591933.
#  8 Systolic  Hospitalization     370717.
#  9 Systolic  Injectable Therapy  333383.
# 10 Systolic  Oral Therapy       2060487.
# 11 NA        Advanced Therapy   1874482.
# 12 NA        Cardiac Device      749440.
# 13 NA        Hospitalization     387831.
# 14 NA        Injectable Therapy  581652.
# 15 NA        Oral Therapy       6271937.

data.frame(HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight))))

# 1  Diastolic       ACE|ARB|ARNI 2213694.74
# 2  Diastolic       Beta Blocker 2350731.76
# 3  Diastolic     Cardiac Device  388505.99
# 4  Diastolic           Diuretic 2763438.51
# 5  Diastolic   Heart Transplant    2097.32
# 6  Diastolic Hospital Inpatient  432618.61
# 7  Diastolic          Inotropic  218838.70
# 8  Diastolic                MRA  519937.90
# 9  Diastolic              Other  239377.72
# 10 Diastolic              SGLT2  163693.56
# 11 Diastolic  Surgery Inpatient  173938.57
# 12 Diastolic        Vasodilator  795658.86
# 13  Systolic       ACE|ARB|ARNI 1798820.42
# 14  Systolic       Beta Blocker 1938916.61
# 15  Systolic     Cardiac Device  591932.55
# 16  Systolic           Diuretic 1857588.87
# 17  Systolic   Heart Transplant   12335.08
# 18  Systolic Hospital Inpatient  269179.86
# 19  Systolic          Inotropic  274055.85
# 20  Systolic                MRA  670477.31
# 21  Systolic              Other  143398.60
# 22  Systolic              SGLT2  171830.78
# 23  Systolic  Surgery Inpatient  164638.26
# 24  Systolic        Vasodilator  513600.17
# 25      <NA>       ACE|ARB|ARNI 5006435.18
# 26      <NA>       Beta Blocker 4989269.51
# 27      <NA>     Cardiac Device  749439.90
# 28      <NA>           Diuretic 4848857.96
# 29      <NA>   Heart Transplant    8597.37
# 30      <NA> Hospital Inpatient  294501.69
# 31      <NA>          Inotropic  416987.00
# 32      <NA>                MRA  959789.56
# 33      <NA>              Other  357372.87
# 34      <NA>              SGLT2  431171.50
# 35      <NA>  Surgery Inpatient  138827.70
# 36      <NA>        Vasodilator 1362166.91


# ---------------------------------------------

# Core 6  - All Drugs-  PATHS temp-----------------------------------------------------

HF_Drug_Histories <- fread("HF Drug Histories 6classes only.txt", colClasses = "character")
Core6 <- HF_Drug_Histories %>% select(patient)

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838
HF_Drug_Histories <- Core6 %>% inner_join(HF_Drug_Histories) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients) 


HF_Drug_Histories <- HF_Drug_Histories %>% select(-Drugs)

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) 

HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient, weight, drug_class) %>% filter(Month==min(Month))

HF_Drug_Histories <- HF_Drug_Histories %>% arrange(patient, weight, Month, drug_class)

HF_Drug_Histories <-  HF_Drug_Histories %>% group_by(patient, weight, Month) %>% mutate(paths = paste(drug_class  , collapse=" + ")) 

HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(-drug_class)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, paths) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient, weight) %>% mutate(paths2 = paste(paths  , collapse=" -> ")) 

temp <- HF_Drug_Histories %>% ungroup() %>% group_by(paths2) %>% summarise(n=sum(as.numeric(weight))) %>% arrange(-n)

# ---------------------------------
# All Pats  - All Drugs-  PATHS temp to MRA -----------------------------------------------------

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

Naive_Pats <- HF_Drug_Histories %>% filter(Month<=12) %>% filter(Drugs=="-") %>% group_by(patient) %>% count() %>% filter(n==12)

HF_Drug_Histories <- Naive_Pats %>% left_join(HF_Drug_Histories) # %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
unique(HF_Ingredients$drug_class)

#HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% select(-Drugs)

HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient, weight, drug_class) %>% filter(Month==min(Month))

HF_Drug_Histories <- HF_Drug_Histories %>% arrange(patient, weight, Month, drug_class)

HF_Drug_Histories <-  HF_Drug_Histories %>% group_by(patient, weight, Month) %>% mutate(paths = paste(drug_class  , collapse=" + ")) 

HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(-drug_class)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, paths) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient, weight) %>% mutate(paths2 = paste(paths  , collapse=" -> ")) 

temp <- HF_Drug_Histories %>% ungroup() %>% group_by(paths2) %>% summarise(n=sum(as.numeric(weight))) %>% arrange(-n)

fwrite(temp, "temp.csv")

# --------------------------------------------------------------
# Estimate Sizing All Pats Systolic vs Diastolic ------------------------------------------------------
HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"

HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset, heart_failure_condition)

HF_Demographics <- HF_Demographics %>% drop_na()

# ALL Heart Failure
sum(HF_Demographics$weight) # 13359838

# ALL Chronic Failure
HF_Demographics %>% filter(heart_failure_condition=="Chronic Heart Failure") %>% summarise(n=sum(weight)) # 13359838

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis == "Heart Failure")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, description)

DANU_Dossiers <- fread("DANU Dossiers.txt")
names(DANU_Dossiers)[1] <- "patient"

DANU_Dossiers <- HF_Demographics %>% select(patient) %>% inner_join(DANU_Dossiers)

DANU_Dossiers <- DANU_Dossiers %>% select(patient, weight, code, earliest, latest, frequency)

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 13359838

DANU_Dossiers <- DANU_Dossiers %>% left_join(DANU_Diagnosis_Codes)

DANU_Dossiers <- DANU_Dossiers %>% drop_na()

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 13359838

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient) %>% distinct()

HF_Drug_Histories %>% inner_join(DANU_Dossiers) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))  # 12041998
DANU_Dossiers <- HF_Drug_Histories %>% inner_join(DANU_Dossiers) 

# Must have specified systolic or dyastolic 
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6587056
  
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>20) %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6123871

unique(DANU_Dossiers$description)

# Must have at least 2 different codes
DANU_Dossiers %>%  # filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 3698594

# Must have specified systolic or dyastolic and have at least 2 different codes
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1398557

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

#  Diastolic Systolic       n
#       <dbl>    <dbl>   <dbl>
# 1         1        1 447523.
# 2         1       NA 583117.
# 3        NA        1 466900.

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
# 1         1        1  891382.
# 2         1       NA 3067220.
# 3        NA        1 2165269.


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
  filter(Diastolic==1 & is.na(Systolic)) %>% select(patient) %>% distinct()

First_Diastolic <- Groups %>% filter(group=="Diastolic") %>% left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>%
  mutate(earliest=as.Date(earliest)) %>%
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>% ungroup() %>% select(patient, earliest)

fwrite(First_Diastolic, "First_Diastolic.txt")

First_Systolic <- Groups %>% filter(group=="Systolic") %>% left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>%
  mutate(earliest=as.Date(earliest)) %>%
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>% ungroup() %>% select(patient, earliest)

fwrite(First_Systolic, "First_Systolic.txt")

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
  filter(Systolic==1 & is.na(Diastolic)) %>% select(patient) %>% distinct()



DANU_Dossiers %>% filter(grepl("ystolic", description)) %>%  select(code, description) %>% distinct()



DANU_Dossiers %>% filter(grepl("iastolic", description)) %>%  select(code, description) %>% distinct()



# ----------------------------------------
# All Patients  - All Drugs-  Drug Usage - Systolic vs Dyastolic -----------------------------------------------------
Diastolic_Pats$group <- "Diastolic"
Systolic_Pats$group <- "Systolic"

Groups <- Diastolic_Pats %>% bind_rows(Systolic_Pats)

fwrite(Groups, "Groups_Diastolic_vs_Systolic.txt", sep="\t")
# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(Groups) %>%  group_by(group) %>% summarise(n=sum(as.numeric(weight))) # 11146437


# 1 Diastolic 2187709.
# 2 Systolic  1569524.
# 3 NA        4577431.


HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_group) %>% summarise(n=sum(as.numeric(weight)))
 
#  1 Diastolic Advanced Therapy    383922.
#  2 Diastolic Cardiac Device       12290.
#  3 Diastolic Hospitalization      41125.
#  4 Diastolic Injectable Therapy   23113.
#  5 Diastolic Oral Therapy       2134680.
#  6 Systolic  Advanced Therapy    278927.
#  7 Systolic  Cardiac Device       16586.
#  8 Systolic  Hospitalization      20881.
#  9 Systolic  Injectable Therapy   14955.
# 10 Systolic  Oral Therapy       1541516.
# 11 NA        Advanced Therapy    648923.
# 12 NA        Cardiac Device       22452.
# 13 NA        Hospitalization      20298.
# 14 NA        Injectable Therapy   24208.
# 15 NA        Oral Therapy       4484552.

data.frame(HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight))))

# 1  Diastolic       ACE|ARB|ARNI 1181042.39
# 2  Diastolic       Beta Blocker 1415225.15
# 3  Diastolic     Cardiac Device   12290.01
# 4  Diastolic           Diuretic 1335015.92
# 5  Diastolic   Heart Transplant     141.89
# 6  Diastolic Hospital Inpatient   33901.72
# 7  Diastolic          Inotropic   60810.16
# 8  Diastolic                MRA  218865.40
# 9  Diastolic              Other   50411.73
# 10 Diastolic              SGLT2   71945.77
# 11 Diastolic  Surgery Inpatient    7081.27
# 12 Diastolic        Vasodilator  289239.63
# 13  Systolic       ACE|ARB|ARNI 1032899.26
# 14  Systolic       Beta Blocker 1242768.56
# 15  Systolic     Cardiac Device   16586.34
# 16  Systolic           Diuretic  814323.63
# 17  Systolic   Heart Transplant     109.09
# 18  Systolic Hospital Inpatient   16855.81
# 19  Systolic          Inotropic   81562.09
# 20  Systolic                MRA  315440.96
# 21  Systolic              Other   33202.51
# 22  Systolic              SGLT2   89761.40
# 23  Systolic  Surgery Inpatient    3915.90
# 24  Systolic        Vasodilator  179257.30
# 25      <NA>       ACE|ARB|ARNI 2820785.53
# 26      <NA>       Beta Blocker 2901353.36
# 27      <NA>     Cardiac Device   22452.26
# 28      <NA>           Diuretic 2235555.44
# 29      <NA>   Heart Transplant     245.91
# 30      <NA> Hospital Inpatient   15696.60
# 31      <NA>          Inotropic  126642.95
# 32      <NA>                MRA  396732.35
# 33      <NA>              Other   74592.58
# 34      <NA>              SGLT2  200848.73
# 35      <NA>  Surgery Inpatient    4355.04
# 36      <NA>        Vasodilator  473113.45

# ---------------------------------------------------------------
# All Patients - All Drugs-  Drug Usage EVER TRIED Systolic vs Dyastolic -----------------------------------------------------
Diastolic_Pats$group <- "Diastolic"
Systolic_Pats$group <- "Systolic"

Groups <- Diastolic_Pats %>% bind_rows(Systolic_Pats)

# Classes on month 60
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(Groups) %>%  group_by(group) %>% summarise(n=sum(as.numeric(weight))) # 11146437

# 1 Diastolic 3067220.
# 2 Systolic  2165269.
# 3 NA        6809510.


HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_group) %>% summarise(n=sum(as.numeric(weight)))
 
#  1 Diastolic Advanced Therapy   1128115.
#  2 Diastolic Cardiac Device      430534.
#  3 Diastolic Hospitalization     608539.
#  4 Diastolic Injectable Therapy  519010.
#  5 Diastolic Oral Therapy       2950179.
#  6 Systolic  Advanced Therapy    804294.
#  7 Systolic  Cardiac Device      633707.
#  8 Systolic  Hospitalization     399790.
#  9 Systolic  Injectable Therapy  353388.
# 10 Systolic  Oral Therapy       2094506.
# 11 NA        Advanced Therapy   2025099.
# 12 NA        Cardiac Device      834891.
# 13 NA        Hospitalization     432315.
# 14 NA        Injectable Therapy  644941.
# 15 NA        Oral Therapy       6602367.

data.frame(HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_class=="ACE", "ACE|ARB|ARNI" , 
                                               ifelse(drug_class=="ARB", "ACE|ARB|ARNI" ,
                                                      ifelse(drug_class=="ARNI", "ACE|ARB|ARNI" , drug_class)))) %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight))))

# 1  Diastolic       ACE|ARB|ARNI 2213694.74
# 2  Diastolic       Beta Blocker 2350731.76
# 3  Diastolic     Cardiac Device  430533.50
# 4  Diastolic           Diuretic 2953367.93
# 5  Diastolic   Heart Transplant    2540.30
# 6  Diastolic Hospital Inpatient  491526.94
# 7  Diastolic          Inotropic  233165.67
# 8  Diastolic                MRA  519937.90
# 9  Diastolic              Other  268542.79
# 10 Diastolic              SGLT2  163693.56
# 11 Diastolic  Surgery Inpatient  202308.56
# 12 Diastolic        Vasodilator  827896.81
# 13  Systolic       ACE|ARB|ARNI 1798820.42
# 14  Systolic       Beta Blocker 1938916.61
# 15  Systolic     Cardiac Device  633706.82
# 16  Systolic           Diuretic 1911613.93
# 17  Systolic   Heart Transplant   12880.86
# 18  Systolic Hospital Inpatient  291474.93
# 19  Systolic          Inotropic  280460.07
# 20  Systolic                MRA  670477.31
# 21  Systolic              Other  154155.36
# 22  Systolic              SGLT2  171830.78
# 23  Systolic  Surgery Inpatient  175831.27
# 24  Systolic        Vasodilator  526980.46
# 25      <NA>       ACE|ARB|ARNI 5006435.18
# 26      <NA>       Beta Blocker 4989269.51
# 27      <NA>     Cardiac Device  834890.79
# 28      <NA>           Diuretic 5242577.83
# 29      <NA>   Heart Transplant    9994.49
# 30      <NA> Hospital Inpatient  327989.73
# 31      <NA>          Inotropic  446706.46
# 32      <NA>                MRA  959789.56
# 33      <NA>              Other  427744.13
# 34      <NA>              SGLT2  431171.50
# 35      <NA>  Surgery Inpatient  154518.07
# 36      <NA>        Vasodilator 1426838.18




# -------------------------------------------
# All Patients - All Drugs-  Drug Usage Last 12 months Systolic vs Dyastolic -----------------------------------------------------
Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")



HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% left_join(Groups) %>%  group_by(group) %>% summarise(n=sum(as.numeric(weight))) 



data.frame(HF_Drug_Histories %>% 
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  left_join(Groups) %>%  
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_")) 


1  Diastolic                ACE  796005.00
2  Diastolic                ARB  791158.31
3  Diastolic               ARNI   18368.63
4  Diastolic       Beta_Blocker 1783759.22
5  Diastolic     Cardiac_Device  100290.30
6  Diastolic           Diuretic 1951234.37
7  Diastolic   Heart_Transplant     302.24
8  Diastolic Hospital_Inpatient  196233.44
9  Diastolic          Inotropic   99385.20
10 Diastolic                MRA  320859.98
11 Diastolic              Other  139146.23
12 Diastolic              SGLT2  105118.92
13 Diastolic  Surgery_Inpatient   65056.90
14 Diastolic        Vasodilator  455947.78
15  Systolic                ACE  688367.85
16  Systolic                ARB  487089.19
17  Systolic               ARNI  280146.23
18  Systolic       Beta_Blocker 1552715.20
19  Systolic     Cardiac_Device  147058.41
20  Systolic           Diuretic 1208808.43
21  Systolic   Heart_Transplant    2796.71
22  Systolic Hospital_Inpatient  105775.88
23  Systolic          Inotropic  132004.41
24  Systolic                MRA  432157.22
25  Systolic              Other   82478.12
26  Systolic              SGLT2  120370.24
27  Systolic  Surgery_Inpatient   46516.42
28  Systolic        Vasodilator  282726.02
29      <NA>                ACE 1904762.10
30      <NA>                ARB 1683060.99
31      <NA>               ARNI  170359.10
32      <NA>       Beta_Blocker 3682530.23
33      <NA>     Cardiac_Device  183114.61
34      <NA>           Diuretic 3279165.68
35      <NA>   Heart_Transplant    2357.67
36      <NA> Hospital_Inpatient  107133.10
37      <NA>          Inotropic  194590.47
38      <NA>                MRA  566830.75
39      <NA>              Other  205186.45
40      <NA>              SGLT2  279362.80
41      <NA>  Surgery_Inpatient   34987.99
42      <NA>        Vasodilator  742115.76



# -----------------------------------
# Create new MECE stocks -----------------

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)


string_AdvancedProcedure <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Cardiac Device"|
                                                                      HF_Ingredients$drug_group=="Hospitalization"], collapse = "|"),")\\b")

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")

string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")

string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

string_OtherAdvanced <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Advanced Therapy"], collapse = "|"),")\\b")

string_Injectables <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")

string_Oral <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Oral Therapy"], collapse = "|"),")\\b")

# Surgery/Transplant/Hospital/Devices 1
# Neprilysin inhibitor 2
# MRA 3
# Injectables 4
# SGLT2 5
# Other Advanced 6
# Orals* 7
# Orals 8
# lapsed 9


HF_Drug_Histories <- HF_Drug_Histories %>% mutate(Stock = ifelse(grepl(string_AdvancedProcedure, Drugs), 1,
                                            ifelse(grepl(string_MRA, Drugs), 2, 
                                                   ifelse(grepl(string_ARNI, Drugs), 3,
                                                      ifelse(grepl(string_Injectables, Drugs), 4, 
                                                             ifelse(grepl(string_SGLT2, Drugs), 5,
                                                                    ifelse(grepl(string_OtherAdvanced, Drugs), 6, 
                                                                           ifelse(grepl(string_Oral, Drugs), 8, 9))))))))


HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient) %>% mutate(AdvExp = cumsum(Stock==1))

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(Stock2=ifelse(Stock==8&AdvExp!=0,7,Stock))
HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(disease, patient, weight, Month, Stock2)
HF_Drug_Histories <- HF_Drug_Histories %>% spread(key=Month, value=Stock2)

fwrite(HF_Drug_Histories, "HF_Box_Histories_Paulo2.txt", sep="\t")



HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Box_Histories     <- fread("HF_Box_Histories_Paulo2.txt", integer64 = "character")

# Flows table in long format
flHF <- HF_Drug_Histories
flHF <- flHF[,disease := NULL]

flHF <- melt(flHF, id = c("patient","weight"))
names(flHF)[c(3,4)] <- c("p1","v1")
flHF <- flHF[, p1 := str_extract(p1,"[:digit:]+")]
flHF$p1 <- as.numeric(flHF$p1)
flHF <- data.frame(cbind(flHF[p1 < 60], flHF[p1 > 1,.(p2 = p1, v2 = v1)]), stringsAsFactors = F)
flHF <- flHF[,c(1:3,5,4,6)]

# Any flow flag and stops flag
flHF <- setDT(flHF)[, flow := (v1 != v2)*1]
flHF <- flHF[, stops := (flow == 1 & v2 == "-")*1]

# Treatment experience
RxExp <- data.frame(HF_Drug_Histories, stringsAsFactors = F)
RxExp$month1 <- (RxExp$month1 != "-")*1

for(i in 2:60){
  cat(i)
  RxExp[,i+2] <- (((RxExp[,i+2] != "-")*1 + RxExp[,i+2-1]) > 0)*1
}

RxExp <- setDT(RxExp)
RxExp <- melt(RxExp, id = c("patient","weight"))
RxExp <- RxExp[, month := str_extract(variable,"[:digit:]+")]
RxExp$month <- as.numeric(RxExp$month)
names(RxExp)[4] <- "HF_RxExp"

flHF <- RxExp[,.(patient,month,HF_RxExp)][flHF, on = .(patient, month = p1)]
flHF <- flHF[,.(patient, weight, p1 = month, p2, v1, v2, p1_RxExp = HF_RxExp, flow, stops)]

# Starts and re-starts flag
flHF <- flHF[, starts := (flow == 1 & v1 == "-" & p1_RxExp == 0)*1]
flHF <- flHF[, re_starts := (flow == 1 & v1 == "-" & p1_RxExp == 1)*1]
flHF <- flHF[, disease := "HF US"]
flHF <- flHF[,c(12,1:11)]




# Bring Therapy classes (Stocks) to the table
HF_Box_Histories <- HF_Box_Histories[,disease := NULL]
HF_Box_Histories <- data.frame(HF_Box_Histories, stringsAsFactors = F)
sto
for(i in 1:60){
  cat(i)
  HF_Box_Histories[,i+2] <- unlist(lapply(HF_Box_Histories[,i+2],function(x) str_sub(x, 1L, 1L)))
}




setDT(HF_Box_Histories) 
HF_Box_Histories <- melt(HF_Box_Histories, id = c("patient","weight"))
names(HF_Box_Histories)[c(3,4)] <- c("p","s")
HF_Box_Histories <- HF_Box_Histories[, p := str_extract(p,"[:digit:]+")]
HF_Box_Histories$p <- as.numeric(HF_Box_Histories$p)


flHF <- flHF %>% mutate(weight=as.numeric(weight)) %>% 
  left_join(HF_Box_Histories, by=c("patient"="patient", "weight"="weight", "p1"="p")) %>%
  rename("s1"="s")


flHF <- flHF %>% mutate(weight=as.numeric(weight)) %>% 
  left_join(HF_Box_Histories, by=c("patient"="patient", "weight"="weight", "p2"="p")) %>%
  rename("s2"="s")

names(flHF)[c(6,7)] <- c("d1","d2")

fwrite(flHF,"HF_Flows_Aux._Long2.txt")

# --------------------------------------------------------------------------------
# Flows --------------------------------------------------------------------------

HF_Flows_Aux._Long <- fread("HF_Flows_Aux._Long2.txt")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% inner_join(HF_Drug_Histories)

HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>% summarise(n2=sum(n)) #12041998

HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2) %>% summarise(n=sum(as.numeric(weight))) 

# 1     1  128557.
# 2     2  908719.
# 3     3  218181.
# 4     4   47706.
# 5     5  269880.
# 6     6 1040075.
# 7     7 1271602.
# 8     8 4449944.
# 9     9 3707335.


HF_Flows_Aux._Long %>% filter(p2>=49)  %>% filter(flow==1) %>% group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>% spread(key=s2, value=n) %>% 
  arrange(-s1) %>% select(1,10:2)


     s1      `9`      `8`      `7`      `6`     `5`     `4`     `3`      `2`     `1`
  <int>    <dbl>    <dbl>    <dbl>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl>
1     9      NA  1960800.  339422.  405181.  59002. 119849.  43868.  163363. 264206.
2     8 1753078. 4600126.      NA   515191. 119892. 143768.  54516.  241564. 262168.
3     7  359969.      NA  1224281.  146706.  26732.  42727.  29745.   74739. 181199.
4     6  361909.  475116.  146332. 1602995.  29878.  57003.  20435.   68870. 119413.
5     5   44634.   87668.   16295.   19043. 322257.   6242.   5465.   15607.  12734.
6     4   92148.  138015.   39975.   60499.   6321.  25735.   3460.   10538.   7155.
7     3   32692.   32538.   26503.   12088.   3460.    595. 289239.   48460.  28297.
8     2  133138.  202674.   75996.   59989.  13166.   4707.  45970. 1388514. 109579.
9     1  197767.      NA   426430.  117791.  13254.  10555.  31210.  122570. 127284


HF_Flows_Aux._Long <- fread("HF_Flows_Aux._Long2.txt")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% inner_join(HF_Drug_Histories)

data.frame(HF_Flows_Aux._Long %>% filter(p2==60) %>% left_join(Groups) %>% group_by(group, s2) %>% 
             summarise(n=sum(as.numeric(weight)))) 


HF_Flows_Aux._Long %>% left_join(Groups) %>% filter(group=="Diastolic") %>%
  filter(p2>=49)  %>% filter(flow==1) %>% group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>% spread(key=s2, value=n) %>% 
  arrange(-s1) %>% select(1,10:2)

     s1     `9`      `8`     `7`     `6`    `5`    `4`    `3`     `2`     `1`
  <int>   <dbl>    <dbl>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>
1     9     NA   454909. 108561. 112557. 12961. 40413.  1735.  34575   87858.
2     8 409236. 1313272.     NA  163191. 28011. 58046.  1892.  68474. 102821.
3     7 108526.      NA  440597.  53649.  6365. 17108.  1920.  22439.  88985.
4     6 105613.  147812.  55568. 530440.  8776. 23755.  1988.  24072.  50570.
5     5   8730.   21634.   6019.   4924. 73899.  1643.   268.   3924.   3395.
6     4  31131.   54966.  16145.  27385.  2126. 10202.   127.   2992.   1707.
7     3   1605.    1391.   1556.    648.    NA     NA  11410.   1017.   1663.
8     2  31119.   55766.  22585.  19623.  2904.  1832.  1172. 339176.  28340.
9     1  70019.      NA  171147.  50072.  3896.  5289.  2458.  31361.  47122.


HF_Flows_Aux._Long %>% left_join(Groups) %>% filter(group=="Systolic") %>%
  filter(p2>=49)  %>% filter(flow==1) %>% group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>% spread(key=s2, value=n)  %>% 
  arrange(-s1) %>% select(1,10:2)

     s1     `9`     `8`     `7`     `6`    `5`    `4`     `3`     `2`    `1`
  <int>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>   <dbl>   <dbl>  <dbl>
1     9     NA  265005.  86249.  67019. 10871. 26823.  26568.  47426. 79842.
2     8 225817. 638088.     NA   69376. 15779. 18469.  29056.  54764. 61757.
3     7  95055.     NA  320574.  38484.  8501. 11797.  17056.  29671. 44962.
4     6  57545.  67806.  39342. 312552.  6272. 13462.  10167.  16372. 34942.
5     5   8436.   9677.   4464    4407. 56864.  1487.   3069.   4505.  4357.
6     4  21216.  18373.  10975.  12218.  1030.  6192.   1388.   3596.  2728 
7     3  21139.  17790.  15469.   6713.  2643.   319. 169732.  30821. 19278.
8     2  34443.  46693.  27778.  16722.  2958.  1592.  31448. 501930. 47759.
9     1  54964.     NA  105688.  34361.  4657.  2657.  19839.  58811. 50278.


# ------------------------
# Flows Last year  --------------------------------------------------------------------------


HF_Flows_Aux._Long <- fread("HF_Flows_Aux._Long2.txt")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% inner_join(HF_Drug_Histories)

HF_Flows_Aux._Long %>% select(patient, weight) %>% distinct()  %>% summarise(n=sum(as.numeric(weight)))  # 12041998



HF_Flows_Aux._Long  %>% filter(p2==60) %>% select(patient, weight, s2) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(s2, n) %>% summarise(total=sum(as.numeric(weight)))  %>% ungroup() %>%
  spread(key=s2, value=total)


HF_Flows_Aux._Long  %>% filter(p2==60) %>% select(patient, weight, s2) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(s2, n)  %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
   ungroup() %>%
   group_by(s2) %>% summarise(mean=weighted.mean(n, weight))


HF_Flows_Aux._Long  %>%  
  filter(p2==60) %>% select(patient, weight) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
  summarise(mean=weighted.mean(n, weight)) # 1.998581



HF_Flows_Aux._Long  %>%  left_join(Groups) %>% filter(group=="Systolic")   %>%
  filter(p2==60) %>% select(patient, weight) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(n) %>% summarise(total=sum(as.numeric(weight)))  

HF_Flows_Aux._Long  %>%  left_join(Groups) %>% filter(group=="Systolic")   %>%
  filter(p2==60) %>% select(patient, weight) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
  summarise(mean=weighted.mean(n, weight)) # 1.998581

HF_Flows_Aux._Long  %>%  left_join(Groups) %>% filter(group=="Diastolic")   %>%
  filter(p2==60) %>% select(patient, weight) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(n) %>% summarise(total=sum(as.numeric(weight)))  


HF_Flows_Aux._Long  %>%  left_join(Groups) %>% filter(group=="Diastolic")   %>%
  filter(p2==60) %>% select(patient, weight) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
  summarise(mean=weighted.mean(n, weight)) # 1.90596


HF_Flows_Aux._Long %>% left_join(Groups) %>% filter(group=="Systolic")   %>%
  filter(p2==60) %>% select(patient, weight, s2) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(s2, n) %>% summarise(total=sum(as.numeric(weight)))  %>% ungroup() %>%
  spread(key=s2, value=total)


HF_Flows_Aux._Long   %>% left_join(Groups) %>% filter(group=="Systolic") %>% 
  filter(p2==60) %>% select(patient, weight, s2) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(s2, n)  %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
   ungroup() %>%
   group_by(s2) %>% summarise(mean=weighted.mean(n, weight))




HF_Flows_Aux._Long %>% left_join(Groups) %>% filter(group=="Diastolic")   %>%
  filter(p2==60) %>% select(patient, weight, s2) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(s2, n) %>% summarise(total=sum(as.numeric(weight)))  %>% ungroup() %>%
  spread(key=s2, value=total)

HF_Flows_Aux._Long   %>% left_join(Groups) %>% filter(group=="Diastolic") %>% 
  filter(p2==60) %>% select(patient, weight, s2) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(s2, n)  %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
   ungroup() %>%
   group_by(s2) %>% summarise(mean=weighted.mean(n, weight))


# -----------------------------------
# Drug Experience ~ stock month60 ------------------------------------------------------------------------------
HF_Flows_Aux._Long <- fread("HF_Flows_Aux._Long2.txt")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% inner_join(HF_Drug_Histories)
HF_Flows_Aux._Long %>% select(patient, weight) %>% distinct()  %>% summarise(n=sum(as.numeric(weight)))  # 12041998


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_AdvancedProcedure <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Cardiac Device"|
                                                                      HF_Ingredients$drug_group=="Hospitalization"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_OtherAdvanced <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Advanced Therapy"], collapse = "|"),")\\b")
string_Injectables <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")
string_Oral <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Oral Therapy"], collapse = "|"),")\\b")


HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_OralExp = ifelse(grepl(string_Oral,d1)|grepl(string_Oral,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = cumsum(p1_OralExp))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = ifelse(p1_OralExp==0,0,1))
 
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_InjExp = ifelse(grepl(string_Injectables,d1)|grepl(string_Injectables,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = cumsum(p1_InjExp))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = ifelse(p1_InjExp==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_OtherAdvanced = ifelse(grepl(string_OtherAdvanced,d1)|grepl(string_OtherAdvanced,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OtherAdvanced = cumsum(p1_OtherAdvanced))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OtherAdvanced = ifelse(p1_OtherAdvanced==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_ARNI = ifelse(grepl(string_ARNI,d1)|grepl(string_ARNI,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ARNI = cumsum(p1_ARNI))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ARNI = ifelse(p1_ARNI==0,0,1))
  
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_SGLT2 = ifelse(grepl(string_SGLT2,d1)|grepl(string_SGLT2,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SGLT2 = cumsum(p1_SGLT2))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SGLT2 = ifelse(p1_SGLT2==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_MRA = ifelse(grepl(string_MRA,d1)|grepl(string_MRA,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_MRA = cumsum(p1_MRA))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_MRA = ifelse(p1_MRA==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_AdvancedProcedure = ifelse(grepl(string_AdvancedProcedure,d1)|grepl(string_AdvancedProcedure,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AdvancedProcedure = cumsum(p1_AdvancedProcedure))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AdvancedProcedure = ifelse(p1_AdvancedProcedure==0,0,1))

Start_df <- HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2) %>% summarise(pats=sum(as.numeric(weight))) 
sum(Start_df$pats)

#      s2     pats
# 1     1  128557.
# 2     2  908719.
# 3     3  218181.
# 4     4   47706.
# 5     5  269880.
# 6     6 1040075.
# 7     7 1271602.
# 8     8 4449944.
# 9     9 3707335.

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_OralExp) %>% 
                                       summarise(pats_p1_OralExp=sum(as.numeric(weight))) %>% filter(p1_OralExp == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_InjExp) %>% 
                                       summarise(pats_p1_InjExp=sum(as.numeric(weight))) %>% filter(p1_InjExp == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_OtherAdvanced) %>% 
                                       summarise(pats_p1_OtherAdvanced=sum(as.numeric(weight))) %>% filter(p1_OtherAdvanced == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_ARNI) %>% 
                                       summarise(pats_p1_ARNI=sum(as.numeric(weight))) %>% filter(p1_ARNI == 1))

 Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_SGLT2) %>% 
                                       summarise(pats_p1_SGLT2=sum(as.numeric(weight))) %>% filter(p1_SGLT2 == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_MRA) %>% 
                                       summarise(pats_p1_MRA=sum(as.numeric(weight))) %>% filter(p1_MRA == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_AdvancedProcedure) %>% 
                                       summarise(pats_p1_AdvancedProcedure=sum(as.numeric(weight))) %>% filter(p1_AdvancedProcedure == 1))

Start_df <- Start_df[,c(1,2,4,6,8,10,12,14,16)]

fwrite(Start_df, "Drug_exp_m60.txt", sep="\t")

# change the order and format and all that

Drug_exp_m60 <- fread("HF_Drug_exp_m60.txt")
  
Drug_exp_m60 <- data.frame(lapply(Drug_exp_m60, function(x) if(is.numeric(x)) round(x, 0) else x))
  
  
row.names(Drug_exp_m60) <- Drug_exp_m60$Stock
  
Drug_exp_m60 <- Drug_exp_m60 %>% select(-c(Stock))
  
df <- Drug_exp_m60

  grid.bubble.plot <- function(df, 
                               axis_labels_size=9, 
                               aspect_ratio=1/1,
                               values_text_size=6,
                               values_text_color="black",
                               x_axis_position="top", # or "bottom",
                               bubble_size_range=c(5, 30),
                               bubble_alpha=0.7,
                               bubble_shape=21,
                               bubble_edge_stroke=0) {
    col_names <- colnames(df)
    row_names <- rownames(df)
    values <- as.vector(as.matrix(df))
    values_x <- as.vector(sapply(col_names, function(i) rep(i, nrow(df))))
    values_y <- as.vector(rep(row_names, dim(df)[2]))
    res_df <- data.frame(values = values, values_x = values_x, values_y)
    res_df <- data.frame(res_df %>% mutate(values_x=fct_relevel(values_x,c("Oral_exp","Advanced_Oral_exp","SGLT2_exp","Injectables_exp","ARNi_exp","MRA_exp","Adv_Procedure_exp"))) %>%
                           mutate(values_y=fct_relevel(values_y,c("Lapsed_Stock","Oral_Stock","Oral_ProcExp_Stock","Adv_Oral_Stock","SGLT2_Stock","Injectables_Stock","ARNi_Stock","MRA_Stock","Adv_Procedures_Stock"))))
    gg <- ggplot(res_df, aes(x=values_x, y=values_y, size = values, fill=factor(values_x))) +
      geom_point(alpha=bubble_alpha, shape=bubble_shape, stroke=bubble_edge_stroke) +
      scale_size(range = bubble_size_range) +
      scale_fill_viridis_d() +
      scale_x_discrete(position = x_axis_position) +
      scale_y_discrete(limits=rev)+
      geom_text(aes(label=paste0(values,"%")), fontface="bold", size=values_text_size, color=values_text_color,) +
      theme(line=element_blank(), 
            panel.background=element_blank(),
            legend.position="none",
            axis.title=element_blank(),
            axis.text=element_text(size=axis_labels_size),
            aspect.ratio=aspect_ratio)
    gg
  }
  
grid.bubble.plot(Drug_exp_m60)



# -----------------------
# Drug Experience ~ stock month60 Diastolic vs Systolic ------------------------------------------------------------------------------
HF_Flows_Aux._Long <- fread("HF_Flows_Aux._Long2.txt")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% inner_join(HF_Drug_Histories)
HF_Flows_Aux._Long %>% select(patient, weight) %>% distinct()  %>% summarise(n=sum(as.numeric(weight)))  # 12041998

HF_Flows_Aux._Long <- Groups%>% filter(group=="Systolic") %>% select(patient) %>% left_join(HF_Flows_Aux._Long)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_AdvancedProcedure <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Cardiac Device"|
                                                                      HF_Ingredients$drug_group=="Hospitalization"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_OtherAdvanced <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Advanced Therapy"], collapse = "|"),")\\b")
string_Injectables <- paste0("\\b(",paste0(HF_Ingredients$Drufgs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")
string_Oral <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Oral Therapy"], collapse = "|"),")\\b")


HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_OralExp = ifelse(grepl(string_Oral,d1)|grepl(string_Oral,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = cumsum(p1_OralExp))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = ifelse(p1_OralExp==0,0,1))
 
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_InjExp = ifelse(grepl(string_Injectables,d1)|grepl(string_Injectables,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = cumsum(p1_InjExp))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = ifelse(p1_InjExp==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_OtherAdvanced = ifelse(grepl(string_OtherAdvanced,d1)|grepl(string_OtherAdvanced,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OtherAdvanced = cumsum(p1_OtherAdvanced))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OtherAdvanced = ifelse(p1_OtherAdvanced==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_ARNI = ifelse(grepl(string_ARNI,d1)|grepl(string_ARNI,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ARNI = cumsum(p1_ARNI))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ARNI = ifelse(p1_ARNI==0,0,1))
  
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_SGLT2 = ifelse(grepl(string_SGLT2,d1)|grepl(string_SGLT2,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SGLT2 = cumsum(p1_SGLT2))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SGLT2 = ifelse(p1_SGLT2==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_MRA = ifelse(grepl(string_MRA,d1)|grepl(string_MRA,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_MRA = cumsum(p1_MRA))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_MRA = ifelse(p1_MRA==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_AdvancedProcedure = ifelse(grepl(string_AdvancedProcedure,d1)|grepl(string_AdvancedProcedure,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AdvancedProcedure = cumsum(p1_AdvancedProcedure))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AdvancedProcedure = ifelse(p1_AdvancedProcedure==0,0,1))

Start_df <- HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2) %>% summarise(pats=sum(as.numeric(weight))) 
sum(Start_df$pats)



Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_OralExp) %>% 
                                       summarise(pats_p1_OralExp=sum(as.numeric(weight))) %>% filter(p1_OralExp == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_InjExp) %>% 
                                       summarise(pats_p1_InjExp=sum(as.numeric(weight))) %>% filter(p1_InjExp == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_OtherAdvanced) %>% 
                                       summarise(pats_p1_OtherAdvanced=sum(as.numeric(weight))) %>% filter(p1_OtherAdvanced == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_ARNI) %>% 
                                       summarise(pats_p1_ARNI=sum(as.numeric(weight))) %>% filter(p1_ARNI == 1))

 Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_SGLT2) %>% 
                                       summarise(pats_p1_SGLT2=sum(as.numeric(weight))) %>% filter(p1_SGLT2 == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_MRA) %>% 
                                       summarise(pats_p1_MRA=sum(as.numeric(weight))) %>% filter(p1_MRA == 1))

Start_df <- Start_df %>% left_join(HF_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_AdvancedProcedure) %>% 
                                       summarise(pats_p1_AdvancedProcedure=sum(as.numeric(weight))) %>% filter(p1_AdvancedProcedure == 1))

Start_df <- Start_df[,c(1,2,4,6,8,10,12,14,16)]

fwrite(Start_df, "Drug_exp_m60.csv", sep=",")

# change the order and format and all that

Drug_exp_m60 <- fread("HF_Drug_exp_m60.txt")
  
Drug_exp_m60 <- data.frame(lapply(Drug_exp_m60, function(x) if(is.numeric(x)) round(x, 0) else x))
  
  
row.names(Drug_exp_m60) <- Drug_exp_m60$Stock
  
Drug_exp_m60 <- Drug_exp_m60 %>% select(-c(Stock))
  
df <- Drug_exp_m60

  grid.bubble.plot <- function(df, 
                               axis_labels_size=9, 
                               aspect_ratio=1/1,
                               values_text_size=6,
                               values_text_color="black",
                               x_axis_position="top", # or "bottom",
                               bubble_size_range=c(5, 30),
                               bubble_alpha=0.7,
                               bubble_shape=21,
                               bubble_edge_stroke=0) {
    col_names <- colnames(df)
    row_names <- rownames(df)
    values <- as.vector(as.matrix(df))
    values_x <- as.vector(sapply(col_names, function(i) rep(i, nrow(df))))
    values_y <- as.vector(rep(row_names, dim(df)[2]))
    res_df <- data.frame(values = values, values_x = values_x, values_y)
    res_df <- data.frame(res_df %>% mutate(values_x=fct_relevel(values_x,c("Oral_exp","Advanced_Oral_exp","SGLT2_exp","Injectables_exp","ARNi_exp","MRA_exp","Adv_Procedure_exp"))) %>%
                           mutate(values_y=fct_relevel(values_y,c("Lapsed_Stock","Oral_Stock","Oral_ProcExp_Stock","Adv_Oral_Stock","SGLT2_Stock","Injectables_Stock","ARNi_Stock","MRA_Stock","Adv_Procedures_Stock"))))
    gg <- ggplot(res_df, aes(x=values_x, y=values_y, size = values, fill=factor(values_x))) +
      geom_point(alpha=bubble_alpha, shape=bubble_shape, stroke=bubble_edge_stroke) +
      scale_size(range = bubble_size_range) +
      scale_fill_viridis_d() +
      scale_x_discrete(position = x_axis_position) +
      scale_y_discrete(limits=rev)+
      geom_text(aes(label=paste0(values,"%")), fontface="bold", size=values_text_size, color=values_text_color,) +
      theme(line=element_blank(), 
            panel.background=element_blank(),
            legend.position="none",
            axis.title=element_blank(),
            axis.text=element_text(size=axis_labels_size),
            aspect.ratio=aspect_ratio)
    gg
  }
  
grid.bubble.plot(Drug_exp_m60)



# --------------------------------------------------------------------
# Months to Class Stocks ------------

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_AdvancedProcedure <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Cardiac Device"|
                                                                      HF_Ingredients$drug_group=="Hospitalization"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_OtherAdvanced <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Advanced Therapy"], collapse = "|"),")\\b")
string_Injectables <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")
string_Oral <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Oral Therapy"], collapse = "|"),")\\b")

HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories_exp %>% inner_join(HF_Drug_Histories)



# Time to first  Oral
HF_Drug_Histories_Oral <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Oral,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Oral,Drugs)) else NA) 

HF_Drug_Histories_Oral <- HF_Drug_Histories_Oral %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Oral %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Oral %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.75
HF_Drug_Histories_Oral %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  Injectable
HF_Drug_Histories_Injectable <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Injectables,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Injectables,Drugs)) else NA) 

HF_Drug_Histories_Injectable <- HF_Drug_Histories_Injectable %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Injectable %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Injectable %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 24.8
HF_Drug_Histories_Injectable %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 23.5

# Time to first  Other Advanced
HF_Drug_Histories_OtherAdvanced <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_OtherAdvanced,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_OtherAdvanced, Drugs)) else NA) 

HF_Drug_Histories_OtherAdvanced <- HF_Drug_Histories_OtherAdvanced %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_OtherAdvanced %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_OtherAdvanced %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 16.6
HF_Drug_Histories_OtherAdvanced %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 8.5


# Time to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% group_by(patient, weight) %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 26.9
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 26.5


# Time to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 25.0
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 22.5
  
# Time to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 18.3
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 10.5


# Time to first Advanced procedure
HF_Drug_Histories_AdvancedProcedure <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_AdvancedProcedure,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_AdvancedProcedure,Drugs)) else NA) 

HF_Drug_Histories_AdvancedProcedure <- HF_Drug_Histories_AdvancedProcedure %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_AdvancedProcedure %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_AdvancedProcedure %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 22.6
HF_Drug_Histories_AdvancedProcedure %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 19.5

# ------------------------------------------
# Lines to Class Stocks  ------------

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_AdvancedProcedure <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Cardiac Device"|
                                                                      HF_Ingredients$drug_group=="Hospitalization"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_OtherAdvanced <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Advanced Therapy"], collapse = "|"),")\\b")
string_Injectables <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")
string_Oral <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Oral Therapy"], collapse = "|"),")\\b")

HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories_exp %>% inner_join(HF_Drug_Histories)



# Lines to first  Oral
HF_Drug_Histories_Oral <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Oral,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Oral,Drugs)) else NA) 

HF_Drug_Histories_Oral <- HF_Drug_Histories_Oral %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Oral %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Oral %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.06
HF_Drug_Histories_Oral %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Lines to first  Injectable
HF_Drug_Histories_Injectable <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Injectables,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Injectables,Drugs)) else NA) 

HF_Drug_Histories_Injectable <- HF_Drug_Histories_Injectable %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Injectable %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Injectable %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.19
HF_Drug_Histories_Injectable %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first  Other Advanced
HF_Drug_Histories_OtherAdvanced <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_OtherAdvanced,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_OtherAdvanced, Drugs)) else NA) 

HF_Drug_Histories_OtherAdvanced <- HF_Drug_Histories_OtherAdvanced %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_OtherAdvanced %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_OtherAdvanced %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 2.9
HF_Drug_Histories_OtherAdvanced %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5


# Lines to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 5.07
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 3.5


# Lines to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.13
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5
  
# Lines to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.44
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5


# Lines to first Advanced procedure
HF_Drug_Histories_AdvancedProcedure <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_AdvancedProcedure,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_AdvancedProcedure,Drugs)) else NA) 

HF_Drug_Histories_AdvancedProcedure <- HF_Drug_Histories_AdvancedProcedure %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_AdvancedProcedure %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_AdvancedProcedure %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.5
HF_Drug_Histories_AdvancedProcedure %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# -------------------
# Plot it --------------------------------------------------------------

Months_vs_Lines_to_class <- fread("Months_vs_Lines_to_class.txt")

library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(Months_vs_Lines_to_class, aes(x=average_months_to_class_all, y=average_lines_to_class_all,  
                                     fill=drug_class, colour=drug_class)) +
  geom_point(alpha=1, size=5, show.legend = F)+
  geom_text_repel(aes(label = drug_class), 
                  colour = "black", 
                  size = 4,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  theme(legend.position = "none",
        #panel.background = element_blank(),
        #panel.grid = element_blank(),
        #axis.ticks = element_blank(),
        text = element_text(size = 12))+
  theme_minimal() +
  xlim(0,30) +
  scale_colour_viridis_d(option = "D")+
  xlab("\nAverage Number of Months to Class Initiation")+
  ylab("Average Number of Therapy Lines to Class Initiation\n")

# -------------------





# Months to Class Indivudal ------------
HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")



HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories_exp %>% inner_join(HF_Drug_Histories)



# Time to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 8.75
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  Beta blcoker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 7.54
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 6.76
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% group_by(patient, weight) %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 26.9
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 26.5


# Time to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 25.0
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 22.5
  
# Time to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 18.3
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 10.5


# Time to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 9.76
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Time to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 22.6
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 19.5



# Time to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 17.9
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 10.5


# Time to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 

HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 22.4
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 18.5

# Time to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 21.9
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 18.5

# Time to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 26.5
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 25.5

# Time to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 26.3
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 25.5

# Time to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 22.2
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 19.5





# ------------------------------------------
# Lines to Class Indivudal  ------------

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")



HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories_exp %>% inner_join(HF_Drug_Histories)



# Lines to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.74
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Lines to first  Beta blcker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.6
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Lines to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.7
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Lines to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 5.1
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 3.5


# Lines to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.1
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5
  
# Lines to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.4
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5


# Lines to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 2.1
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Lines to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.04
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5



# Lines to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.18
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5


# Lines to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 


HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.33
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.67
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.94
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.26
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 6.00
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 4.5

# -------------------
# Plot it Individual --------------------------------------------------------------

Months_vs_Lines_to_class <- fread("Months_vs_Lines_to_class_individual.txt")

library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(Months_vs_Lines_to_class, aes(x=average_months_to_class_all, y=average_lines_to_class_all,  
                                     fill=drug_class, colour=drug_class)) +
  geom_point(alpha=1, size=5, show.legend = F)+
  geom_text_repel(aes(label = drug_class), 
                  colour = "black", 
                  size = 4,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  theme(legend.position = "none",
        #panel.background = element_blank(),
        #panel.grid = element_blank(),
        #axis.ticks = element_blank(),
        text = element_text(size = 12))+
  theme_minimal() +
  xlim(0,30) +
  scale_colour_viridis_d(option = "D")+
  xlab("\nAverage Number of Months to Class Initiation")+
  ylab("Average Number of Therapy Lines to Class Initiation\n")

# -------------------

# Months to Class Individual  Diastolic vs Systolic ------------
Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")



HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories_exp %>% inner_join(HF_Drug_Histories) %>% inner_join(Groups %>% filter(group=="Systolic") %>% select(patient))



# Time to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 9.30
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  Beta blcoker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 7.22
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 6.66
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% group_by(patient, weight) %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 26.1
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 25.5


# Time to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 28.2
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 28.5
  
# Time to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 17.2
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 8.5


# Time to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 10.7
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Time to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 15.2
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 4.5



# Time to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 17.2
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 9.5


# Time to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 

HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 24.2
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 22.5

# Time to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 21.3
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 16.5

# Time to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 24.3
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 21.5

# Time to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 25.0
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 23.5

# Time to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 24.3
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 19.5





# ------------------------------------------
# Lines to Class Individual Diastolic vs Systolic ------------
Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")



HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories_exp %>% inner_join(HF_Drug_Histories) %>% inner_join(Groups %>% filter(group=="Systolic") %>% select(patient))



# Lines to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.88
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Lines to first  Beta blcker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.61
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Lines to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.76
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Lines to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 5.04
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 3.5


# Lines to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 5.18
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 3.5
  
# Lines to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.35
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5


# Lines to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 2.43
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Lines to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.21
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5



# Lines to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.41
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5


# Lines to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 


HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 5.23
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 3.5

# Lines to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.96
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.86
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.26
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 7.14
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 5.5

# -------------------
# Plot it Individual  Diastolic vs Systolic --------------------------------------------------------------

Months_vs_Lines_to_class <- fread("Months_vs_Lines_to_class_individual_Diastolic_vs_Systolic.txt")

library(ggrepel)
library(hrbrthemes)
library(viridis)

Months_vs_Lines_to_class %>% filter(Group=="Systolic") %>%
ggplot(aes(x=average_months_to_class, y=average_lines_to_class,  
                                     fill=Group, colour=Group)) +
  geom_point(alpha=1, size=5, show.legend = F)+
  geom_text_repel(aes(label = drug_class, colour = Group), 
                  size = 4,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  theme(legend.position = "none",
        #panel.background = element_blank(),
        #panel.grid = element_blank(),
        #axis.ticks = element_blank(),
        text = element_text(size = 10))+
  theme_minimal() +
  xlim(0,35) +
  ggsci::scale_colour_nejm() +
  xlab("\nAverage Number of Months to Class Initiation")+
  ylab("Average Number of Therapy Lines to Class Initiation\n")

# -------------------


# Comorbidities Penetrance HF ----------------------------------------------------------


HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- HF_Drug_Histories_exp %>% inner_join(HF_Comorbidity_Inventories)

HF_Comorbidity_Inventories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 12041998

HF_Comorbidity_Inventories %>% filter(diagnosis=="N18") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #5320640 #0.4418403

HF_Comorbidity_Inventories %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #5692709 # 0.4727379

HF_Comorbidity_Inventories %>% filter(diagnosis=="K76") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #2426515 # 0.2015044

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"

DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 10083150 0.837332

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  #  7158731 0.5944803

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code ))
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code == max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% filter(code>=30) %>% select(patid) %>% distinct()
names(DANU_Events)[1] <- "patient"

DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 4676773 0.3883718

# Diastolic
Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")
Diastolic <- Groups %>% filter(group=="Diastolic") %>% select(patient)

HF_Comorbidity_Inventories %>% inner_join(Diastolic) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 3067220

HF_Comorbidity_Inventories %>% inner_join(Diastolic) %>% filter(diagnosis=="N18") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #1605518 #0.523444

HF_Comorbidity_Inventories %>% inner_join(Diastolic) %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #1663141 # 0.5422307

HF_Comorbidity_Inventories  %>% inner_join(Diastolic) %>% filter(diagnosis=="K76") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #646174.5 # 0.2106711

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"

DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>% inner_join(Diastolic) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 2652343 0.8647384

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>% inner_join(Diastolic) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  #  1890391 0.6163206

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code ))
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code == max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% filter(code>=30) %>% select(patid) %>% distinct()
names(DANU_Events)[1] <- "patient"

DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>% inner_join(Diastolic) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 1304308 0.4252411




# Systolic
Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")
Systolic <- Groups %>% filter(group=="Systolic") %>% select(patient)

HF_Comorbidity_Inventories %>% inner_join(Systolic) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 2165159

HF_Comorbidity_Inventories %>% inner_join(Systolic) %>% filter(diagnosis=="N18") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #1000459 #0.4620718

HF_Comorbidity_Inventories %>% inner_join(Systolic) %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #971185.2 # 0.4485514

HF_Comorbidity_Inventories  %>% inner_join(Systolic) %>% filter(diagnosis=="K76") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #387338.2 # 0.178896

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"

DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>% inner_join(Systolic) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 1762743 0.8141402

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>% inner_join(Systolic) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  #  1229130 0.5676858

DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code ))
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code == max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% filter(code>=30) %>% select(patid) %>% distinct()
names(DANU_Events)[1] <- "patient"

DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>% inner_join(Systolic) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 808936 0.3736151



# ----------------------------------------------
# LVEF Indexes Availability, Proportions ------------------

HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient, weight) %>% distinct()
sum(as.numeric(HF_Drug_Histories_exp$weight))

Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")

DANU_Measure_Codes <- fread("DANU Measure Codes.txt")
unique(DANU_Measure_Codes$test)
#"LVEF Fraction"
#"LVEF Reduced"      
#"NYHA Class"        

DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="LVEF Fraction"|test=="LVEF Reduced"|test=="NYHA Class")
DANU_Measures <- DANU_Measures %>% select(-c(source, description, vague_value, vague_date, metric))
DANU_Measures %>% select(patid, weight, test) %>% distinct() %>% group_by(test) %>% summarise(n=sum(weight))
 
#   test                  n
#   <chr>             <dbl>
# 1 LVEF Fraction 10610830.
# 2 LVEF Reduced   3462529.
# 3 NYHA Class      840275.

names(DANU_Measures)[1] <- "patient"

HF_Drug_Histories_exp %>% select(-weight) %>% inner_join(DANU_Measures) %>%
  select(patient, weight, test) %>% distinct() %>% group_by(test) %>% summarise(n=sum(weight))
 
# 1 LVEF Fraction 3033704.
# 2 LVEF Reduced  1354566.
# 3 NYHA Class     538748.

DANU_Measures <- DANU_Measures %>% mutate(claimed=as.Date(claimed)) %>% group_by(patient, test) %>% filter(claimed==max(claimed)) %>% slice(1) 

Groups %>% inner_join(DANU_Measures) %>%
  select(patient, weight, test, group) %>% distinct() %>% group_by(group, test) %>% summarise(n=sum(weight)) %>%
  mutate(percent=ifelse(group=="Diastolic", 100*n/3067220, 100*n/2165268))
 
#   group     test                n percent
# 1 Diastolic LVEF Fraction 834050.   27.2 
# 2 Diastolic LVEF Reduced  424406.   13.8 
# 3 Diastolic NYHA Class    137767.    4.49
# 4 Systolic  LVEF Fraction 706506.   32.6 
# 5 Systolic  LVEF Reduced  327141.   15.1 
# 6 Systolic  NYHA Class    195307.    9.02


Groups %>% inner_join(DANU_Measures) %>%
  filter(test=="LVEF Fraction") %>%
  select(patient, weight, value, group) %>% distinct() %>%
  mutate(value=ifelse(value<=40,1,0)) %>% group_by(group, value) %>% summarise(n=sum(weight)) 

#   group     value       n
# 1 Diastolic     0 782595.
# 2 Diastolic     1  51455.
# 3 Systolic      0 313685.
# 4 Systolic      1 392821.

Groups %>% inner_join(DANU_Measures) %>%
  filter(test=="LVEF Reduced") %>%
  select(patient, weight, value, group) %>% distinct() %>%
  group_by(group, value) %>% summarise(n=sum(weight)) 

#   group     value       n
# 1 Diastolic     0 399782.
# 2 Diastolic     1  24625.
# 3 Systolic      0 135546.
# 4 Systolic      1 191596.
# --------------------------------------
# LVEF Indexes Availability, Proportions  v2------------------

HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient, weight) %>% distinct()
sum(as.numeric(HF_Drug_Histories_exp$weight))

Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")

DANU_Measure_Codes <- fread("DANU Measure Codes.txt")
unique(DANU_Measure_Codes$test)
#"LVEF Fraction"
#"LVEF Reduced"      
#"NYHA Class"        

DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="LVEF Fraction"|test=="LVEF Reduced"|test=="NYHA Class")
DANU_Measures <- DANU_Measures %>% select(-c(source, description, vague_value, vague_date, metric))
DANU_Measures %>% select(patid, weight, test) %>% distinct() %>% group_by(test) %>% summarise(n=sum(weight))
names(DANU_Measures)[1] <- "patient" 

# 1 LVEF Fraction 10610830.
# 2 LVEF Reduced   3462529.
# 3 NYHA Class      840275.

HF_Drug_Histories_exp %>% select(-weight) %>% inner_join(DANU_Measures) %>%
  select(patient, weight, test) %>% distinct() %>% group_by(test) %>% summarise(n=sum(weight))
 
# 1 LVEF Fraction 3033704.
# 2 LVEF Reduced  1354566.
# 3 NYHA Class     538748.

# % of ALL Heart Failure
HF_Drug_Histories_exp %>% select(-weight) %>% inner_join(DANU_Measures)  %>%
  filter( (test== "LVEF Reduced")|(test== "LVEF Fraction")) %>% select(patient, weight)  %>% 
  distinct() %>% summarise(n=sum(weight)) # 3152431

HF_Drug_Histories_exp %>% select(-weight) %>% inner_join(DANU_Measures)  %>%
  filter( (test== "LVEF Reduced" & value==1)|(test== "LVEF Fraction" & value<=40)) %>% select(patient, weight)  %>% 
  distinct() %>% summarise(n=sum(weight))  # 1111438 # 0.3525654 only would be reduce EF

# % of Diastolic vs Systolic Heart Failure

Groups %>% inner_join(DANU_Measures) %>%
    filter( (test== "LVEF Reduced")|(test== "LVEF Fraction")) %>% select(patient, weight, group)  %>% 
  distinct() %>% group_by(group) %>% summarise(n=sum(weight))

# 1 Diastolic 874442. # 0.2850927 (3067220)
# 2 Systolic  720115. " 0.3325755 (2165268)

Groups %>% inner_join(DANU_Measures) %>%
   filter( (test== "LVEF Reduced" & value==1)|(test== "LVEF Fraction" & value<=35))  %>% select(patient, weight, group)  %>% 
  distinct() %>% group_by(group) %>% summarise(n=sum(weight))
 
# 1 Diastolic 119857. (0.1370668)
# 2 Systolic  528200. (0.733494)



Groups %>% inner_join(DANU_Measures) %>%
  filter( (test== "LVEF Fraction")) %>%
  group_by(patient) %>% filter(value==min(value)) %>% slice(1) %>%
  mutate(EF=ifelse(value<=40,"Reduced", "Preserved")) %>% filter(EF=="Reduced") %>%
  select(patient, group, weight) %>% distinct() %>% 
  group_by(group) %>% summarise(n=sum(weight))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

Groups %>% inner_join(DANU_Measures) %>%
   filter( (test== "LVEF Reduced" & value==1)|(test== "LVEF Fraction" & value<=35))  %>% select(patient, group)  %>% 
  distinct() %>% left_join(DANU_Measures) %>%
  filter(test=="LVEF Fraction") %>%
  select(patient, group, claimed, value) %>% mutate(claimed=as.Date(claimed)) %>%
  mutate(claimed=as.character(claimed)) %>%
  mutate(claimed=str_sub(claimed, 1L, 7L)) %>%
  left_join(Months_lookup, by=c("claimed"="Month")) %>%
  group_by(patient) %>% mutate(MAX=max(value)) %>%
  mutate(EF=100*value/MAX) %>%
  ungroup() %>%
  filter(Exact_Month>=48) %>%
  ggplot(aes(Exact_Month, EF)) +
  geom_smooth()


# Date-based 
First_Diastolic <- fread("First_Diastolic.txt")
First_Systolic <- fread("First_Systolic.txt")

FirstDx <- First_Diastolic %>% bind_rows(First_Systolic)

Groups %>% inner_join(DANU_Measures) %>% inner_join(FirstDx) %>%
  mutate(claimed=as.Date(claimed)) %>%
    filter( (test== "LVEF Reduced")|(test== "LVEF Fraction")) %>% filter(earliest<=claimed) %>% 
  select(patient, weight, group)  %>% 
  distinct() %>% group_by(group) %>% summarise(n=sum(weight))

# 1 Diastolic 689395.
# 2 Systolic  630126.

Groups %>% inner_join(DANU_Measures) %>%inner_join(FirstDx) %>%
  mutate(claimed=as.Date(claimed)) %>%
   filter( (test== "LVEF Reduced" & value==1)|(test== "LVEF Fraction" & value<=40)) %>% filter(earliest<=claimed)  %>%
  select(patient, weight, group)  %>% 
  distinct() %>% group_by(group) %>% summarise(n=sum(weight))
 
# 1 Diastolic  99616  # 0.1444977
# 2 Systolic  483393. # 0.767137

# ---------------------------------
# HEart Failure Staging ----------------------------------------------
# NYHA Stages
HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient, weight) %>% distinct()
sum(as.numeric(HF_Drug_Histories_exp$weight))

Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")


DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="NYHA Class")
DANU_Measures <- DANU_Measures %>% select(-c(source, description, vague_value, vague_date, metric))
DANU_Measures %>% select(patid, weight, test) %>% distinct() %>% group_by(test) %>% summarise(n=sum(weight))
names(DANU_Measures)[1] <- "patient" 

DANU_Measures <- HF_Drug_Histories_exp %>% select(-weight) %>% inner_join(DANU_Measures)

DANU_Measures <- DANU_Measures %>% group_by(patient) %>% filter(claimed==max(claimed)) %>% slice(1) %>%
  mutate(value=round(value)) %>%
  select(-c(test, unit)) %>% ungroup()

DANU_Measures %>% select(patient, weight, value) %>% distinct() %>% group_by(value) %>% summarise(n=sum(weight))


Stages <- DANU_Measures %>% select(patient, weight, value) %>% distinct()
names(Stages)[3] <- "Stages"

# --------------------------------------

# NYHA Stages % Class Ever tried  ----------------------


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

#HF_Drug_Histories <- Stages %>% select(patient) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 538748

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% inner_join(Stages %>% select(-weight)) %>% 
  group_by(Stages) %>% summarise(n=sum(as.numeric(weight))) 

# 1      1 110595.
# 2      2 268960.
# 3      3 133974.
# 4      4  25219.

HF_Drug_Histories %>%  select(patient, weight, drug_group) %>% distinct() %>%
  inner_join(Stages %>% select(-weight)) %>% 
  group_by(Stages, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>% spread(key=Stages, value=n)

#   drug_group             `1`     `2`     `3`    `4`
#   <chr>                <dbl>   <dbl>   <dbl>  <dbl>
# 1 Advanced Therapy    38077.  98246.  64735. 13120.
# 2 Cardiac Device      31831.  95082.  51195. 10238.
# 3 Hospitalization     17575.  53114.  36833.  7842.
# 4 Injectable Therapy  14909.  50518.  36658.  9134.
# 5 Oral Therapy       107703. 261120. 129029. 24682.
 


HF_Drug_Histories %>% 
  select(patient, weight, drug_class) %>% distinct() %>%
    inner_join(Stages %>% select(-weight)) %>% 
  group_by(Stages, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>% spread(key=Stages, value=n)

#  1 ACE                60073. 150612.  71358. 15217.
#  2 ARB                35616.  97620.  51070. 11313.
#  3 ARNI               12424.  46077.  20635.  4104.
#  4 Beta Blocker       96789. 239737. 117968. 22959.
#  5 Cardiac Device     31831.  95082.  51195. 10238.
#  6 Diuretic           79673. 217189. 118981. 23205.
#  7 Heart Transplant    1092.   2060.   2161.  1368.
#  8 Hospital Inpatient 10656.  38317.  27947.  4245.
#  9 Inotropic          12106.  32943.  22471.  4497.
# 10 MRA                29574.  96813.  51499.  9809.
# 11 Other               6958.  17565.  12111.  3119.
# 12 SGLT2               5268.  18797.   8485.  2777.
# 13 Surgery Inpatient   8614.  24765.  15633.  4223.
# 14 Vasodilator        25756.  66233.  46608.  9617.



temp <- HF_Drug_Histories %>% 
  select(patient, weight, drug_class) %>% distinct() %>%
    left_join(Stages %>% select(-weight)) %>% mutate(Treat=1) %>% select(-weight) %>%
  spread(key=drug_class, value=Treat)

temp[is.na(temp)] <- 0

temp$Stages <- as.factor(temp$Stages)
temp <- temp %>% select(-patient)


library(randomForest)
library(DALEX)

names(temp) <- str_replace_all(names(temp), " ", "_")

names(temp)

temp2 <- temp %>% group_by(Stages) %>% sample_n(178)

modelAll_1_randomForest <- randomForest(Stages ~ . , data = temp2)

data.frame(
  temp2 %>% select(Stages) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp2, type = 'prob')) 
) %>%
  gather( PredStage, Score, X1:X4, factor_key=TRUE) %>%
  ggplot(aes(Score, colour=PredStage, fill=PredStage)) +
  geom_density(alpha=0.5) +
  facet_wrap(~Stages) +
  theme_minimal() +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() 



# Lines

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- Stages %>% select(patient) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>%
   summarise(total=sum(as.numeric(weight))) # 11146437
  
HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>%
  group_by(patient, weight) %>% count() %>% ungroup() %>%
  inner_join(Stages %>% select(-weight)) %>%
  group_by(Stages) %>%
  summarise(n=weighted.mean(n,as.numeric(weight))) # 5.19


# -----------------------------
# Random forest cont --------------

temp

HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- temp %>% select(patient) %>% left_join(HF_Comorbidity_Inventories) %>% select(patient, diagnosis)

HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% mutate(Treat=1) %>%
  spread(key=diagnosis, value=Treat)

HF_Comorbidity_Inventories[is.na(HF_Comorbidity_Inventories)] <- 0

temp <- temp %>% left_join(HF_Comorbidity_Inventories)


temp$Stages <- as.factor(temp$Stages)
# temp <- temp %>% select(-c(patient, Stages))

library(randomForest)
library(DALEX)

names(temp) <- str_replace_all(names(temp), " ", "_")

names(temp)

temp2 <- temp 

temp2 <- temp2 %>% select(-`500`)
temp2 <- temp2 %>% select(-`695`)
temp2 <- temp2 %>% select(-`724`)
temp2 <- temp2 %>% select(-`728`)
temp2 <- temp2 %>% select(-`729`)
temp2 <- temp2 %>% select(-`999`)
temp2 <- temp2 %>% select(-`<NA>`)
 
temp2 <- temp2 %>% select(-patient)
modelAll_1_randomForest <- randomForest(Stages ~ . , data = temp2)

modelAll_1_randomForest$importance

data.frame(
  temp2 %>% select(Stages) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp2, type = 'prob')) 
) %>%
  gather( PredStage, Score, X1:X4, factor_key=TRUE) %>%
  ggplot(aes(Score, colour=PredStage, fill=PredStage)) +
  geom_density(alpha=0.5) +
  facet_wrap(~Stages) +
  theme_minimal() +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() 

temp %>% select(patient) %>% bind_cols(
  predict(modelAll_1_randomForest, temp2, type = 'prob')
) %>% 
  gather( PredStage, Score, `1`:`4`, factor_key=TRUE) %>%
  ungroup() %>% 
  group_by(patient) %>% filter(Score==max(Score)) %>%
   ggplot(aes(Score, fill=PredStage, Colour=PredStage)) +
  geom_density(alpha=0.5)

temp %>% select(patient) %>% bind_cols(
  predict(modelAll_1_randomForest, temp2, type = 'prob')
) %>% 
  gather( PredStage, Score, `1`:`4`, factor_key=TRUE) %>%
  ungroup() %>% 
  group_by(patient) %>% filter(Score==max(Score)) %>% 
  ungroup() %>% 
  left_join(HF_Drug_Histories %>% select(patient, weight) %>% distinct()) %>%
  group_by(PredStage) %>% summarise(n=sum(as.numeric(weight))) 
  

# ------------------------------
# Starting Groups ACE vs Beta blocker - No Months & No Lines to Class --------------------------------

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs != "-") %>% group_by(patient) %>% filter(Month==min(Month))


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")

HF_Drug_Histories %>% filter(grepl(string_BetaBlocker, Drugs)) %>% select(patient) %>% 
  anti_join(HF_Drug_Histories %>% filter(grepl(string_ACE, Drugs)) %>% select(patient))

HF_Drug_Histories %>% filter(grepl(string_ACE, Drugs)) %>% select(patient) %>%
    anti_join(HF_Drug_Histories %>% filter(grepl(string_BetaBlocker, Drugs)) %>% select(patient))





# Months to Class Indivudal
HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")



HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories_exp %>% inner_join(HF_Drug_Histories)



# Time to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 8.75
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  Beta blcoker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 7.54
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 6.76
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Time to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% group_by(patient, weight) %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 26.9
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 26.5


# Time to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 25.0
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 22.5
  
# Time to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 18.3
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 10.5


# Time to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 9.76
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Time to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 22.6
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 19.5



# Time to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 17.9
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 10.5


# Time to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 

HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 22.4
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 18.5

# Time to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 21.9
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 18.5

# Time to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 26.5
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 25.5

# Time to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 26.3
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 25.5

# Time to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 22.2
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 19.5


# Lines to Class Indivudal

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")



HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories_exp %>% inner_join(HF_Drug_Histories)



# Lines to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.74
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Lines to first  Beta blcker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.6
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Lines to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 1.7
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1


# Lines to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 5.1
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 3.5


# Lines to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.1
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5
  
# Lines to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.4
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5


# Lines to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 2.1
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1

# Lines to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.04
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5



# Lines to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.18
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 1.5


# Lines to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 


HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.33
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.67
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.94
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.26
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 2.5

# Lines to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 6.00
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight)) # 4.5

# Plot it Individual

Months_vs_Lines_to_class <- fread("Months_vs_Lines_to_class_individual.txt")

library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(Months_vs_Lines_to_class, aes(x=average_months_to_class_all, y=average_lines_to_class_all,  
                                     fill=drug_class, colour=drug_class)) +
  geom_point(alpha=1, size=5, show.legend = F)+
  geom_text_repel(aes(label = drug_class), 
                  colour = "black", 
                  size = 4,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  theme(legend.position = "none",
        #panel.background = element_blank(),
        #panel.grid = element_blank(),
        #axis.ticks = element_blank(),
        text = element_text(size = 12))+
  theme_minimal() +
  xlim(0,30) +
  scale_colour_viridis_d(option = "D")+
  xlab("\nAverage Number of Months to Class Initiation")+
  ylab("Average Number of Therapy Lines to Class Initiation\n")


# --------------------
# All Exp - All Drugs-  SGLT2 ARNi MRA exp per comorbidity -----------------------------------------------------

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Drugs) %>% distinct()

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(grepl(string_MRA, Drugs)) %>% select(patient) %>% distinct()
MRA_ARNi <- HF_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)) %>% select(patient) %>% distinct()


HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient, weight) %>% distinct()

HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- HF_Drug_Histories_exp %>% inner_join(HF_Comorbidity_Inventories %>% select(-weight))

HF_Comorbidity_Inventories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 12041998

HF_Drug_Histories_exp %>% left_join(
  HF_Comorbidity_Inventories %>% filter(diagnosis=="N18") %>% select(patient) %>% distinct() %>% 
  mutate(CKD="CKD"))  %>%
  left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(CKD, SGLT2) %>% summarise(n=sum(as.numeric(weight)))
   
# 1 CKD   SGLT2  344736.
# 2 CKD   NA    4975903.
# 3 NA    SGLT2  421960.
# 4 NA    NA    6299399.

HF_Drug_Histories_exp %>% left_join(
  HF_Comorbidity_Inventories %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% select(patient) %>% distinct() %>% 
  mutate(PAD="PAD"))  %>%
  left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(PAD, SGLT2) %>% summarise(n=sum(as.numeric(weight)))
   
# 1 PAD   SGLT2  353306.
# 2 PAD   NA    5339403.
# 3 NA    SGLT2  413390.
# 4 NA    NA    5935899.

HF_Drug_Histories_exp %>% left_join(
  HF_Comorbidity_Inventories %>% filter(diagnosis=="K76") %>% select(patient) %>% distinct() %>% 
  mutate(NAFLD="NAFLD"))  %>%
  left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(NAFLD, SGLT2) %>% summarise(n=sum(as.numeric(weight)))

# 1 NAFLD SGLT2  211890.
# 2 NAFLD NA    2214625.
# 3 NA    SGLT2  554806.
# 4 NA    NA    9060678.


DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"

HF_Drug_Histories_exp %>%
  left_join(
DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>% inner_join(HF_Comorbidity_Inventories) %>% mutate(Diabetes="Diabetes")
) %>%
  select(patient, weight, Diabetes) %>% distinct() %>% 
  left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(Diabetes, SGLT2) %>% summarise(n=sum(as.numeric(weight)))

# 1 Diabetes SGLT2  751428.
# 2 Diabetes NA    6407303.
# 3 NA       SGLT2   15267.
# 4 NA       NA    4868000.


DANU_Events <- fread("DANU Events.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code ))
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code == max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% filter(code>=30) %>% select(patid) %>% distinct()
names(DANU_Events)[1] <- "patient"

HF_Drug_Histories_exp %>%
  left_join(
    DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>%
  select(patient) %>% distinct() %>% mutate(Obesity="Obesity")
  ) %>% 
  left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(Obesity, SGLT2) %>% summarise(n=sum(as.numeric(weight)))

# 1 Obesity SGLT2  370564.
# 2 Obesity NA    4306210.
# 3 NA      SGLT2  396132.
# 4 NA      NA    6969093.

# -------------------------------
# gradient boost patients with NYHA  ------------------

HF_Drug_Histories_exp <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories_exp <- gather(HF_Drug_Histories_exp, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories_exp <- HF_Drug_Histories_exp %>% filter(Drugs!="-") %>% select(patient, weight) %>% distinct()
sum(as.numeric(HF_Drug_Histories_exp$weight))

Groups <- fread("Groups_Diastolic_vs_Systolic.txt", sep="\t")


DANU_Measures <- fread("DANU Measures.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="NYHA Class")
DANU_Measures <- DANU_Measures %>% select(-c(source, description, vague_value, vague_date, metric))
DANU_Measures %>% select(patid, weight, test) %>% distinct() %>% group_by(test) %>% summarise(n=sum(weight))
names(DANU_Measures)[1] <- "patient" 

DANU_Measures <- HF_Drug_Histories_exp %>% select(-weight) %>% inner_join(DANU_Measures)

DANU_Measures <- DANU_Measures %>% group_by(patient) %>% filter(claimed==max(claimed)) %>% slice(1) %>%
 # mutate(value=round(value)) %>%
  select(-c(test, unit)) %>% ungroup()

DANU_Measures %>% select(patient, weight, value) %>% distinct() %>% group_by(value) %>% summarise(n=sum(weight))


Stages <- DANU_Measures %>% select(patient, weight, value) %>% distinct()
names(Stages)[3] <- "Stages"



HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- Stages %>% select(patient) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, generic_name)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories$generic_name <- str_replace_all(HF_Drug_Histories$generic_name, " ", "_")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 538748

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% inner_join(Stages %>% select(-weight)) %>% 
  group_by(Stages) %>% summarise(n=sum(as.numeric(weight))) 

# 1      1 110595.
# 2      2 268960.
# 3      3 133974.
# 4      4  25219.

temp_short <- HF_Drug_Histories %>% 
  select(patient, weight, generic_name) %>% distinct() %>%
    left_join(Stages %>% select(-weight)) %>% mutate(Treat=1) %>% select(-weight) %>%
  spread(key=generic_name, value=Treat)

temp_short[is.na(temp_short)] <- 0


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- temp_short %>% select(patient) %>% left_join(HF_Comorbidity_Inventories) %>% select(patient, diagnosis)
HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% filter(grepl("C", diagnosis)|grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|
                                        grepl("H", diagnosis)|grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|
                                        grepl("M", diagnosis)|grepl("N", diagnosis)|grepl("R", diagnosis))


HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% mutate(Treat=1) %>%
  spread(key=diagnosis, value=Treat)

HF_Comorbidity_Inventories[is.na(HF_Comorbidity_Inventories)] <- 0

temp_short <- temp_short %>% left_join(HF_Comorbidity_Inventories)

temp_short_nopat <- temp_short[,2:1020]

temp_short_nopat # 3779

0.21*3779 = 794
0.71*3779 = 2683
0.95*3779 = 3590,  3779

lm_model <- lm(Stages ~ . , data=temp_short_nopat)
summary(lm_model)

data.frame(predict(lm_model, temp_short_nopat)) %>%
  arrange(predict.lm_model..temp_short_nopat.) %>%
  mutate(RowN = row_number()) %>%
  mutate(Predicted.Stage=ifelse(RowN<=794, "1", ifelse(RowN<=2683, "2", ifelse(RowN<=3590,"3", "4")))) %>%
  #group_by(ColorStage) %>% count()
  ggplot(aes(predict.lm_model..temp_short_nopat., fill=Predicted.Stage, colour=Predicted.Stage)) +
  geom_histogram(alpha=0.7,bins=100)  +
  theme_minimal() + 
  ggsci::scale_colour_jama() +
  ggsci::scale_fill_jama() +
  xlab("\n Predicted Score/Stage") +
  ylab("Patient Sample Count \n")

names(temp_short_nopat) <- paste0("A",names(temp_short_nopat))

modelAll_1_gbm <- gbm(AStages ~ . , data = temp_short_nopat)
summary(modelAll_1_gbm)

predict(modelAll_1_gbm, temp_short_nopat)


data.frame(predict(modelAll_1_gbm, temp_short_nopat)) %>%
  arrange(predict.modelAll_1_gbm..temp_short_nopat.) %>%
  mutate(RowN = row_number()) %>%
  mutate(Predicted.Stage=ifelse(RowN<=794, "1", ifelse(RowN<=2683, "2", ifelse(RowN<=3590,"3", "4")))) %>%
  #group_by(ColorStage) %>% count()
  ggplot(aes(predict.modelAll_1_gbm..temp_short_nopat., fill=Predicted.Stage, colour=Predicted.Stage)) +
  geom_histogram(alpha=0.7,bins=100)  +
  theme_minimal() + 
  ggsci::scale_colour_jama() +
  ggsci::scale_fill_jama() +
  xlab("\n Predicted Score/Stage") +
  ylab("Patient Sample Count \n")


# gradient boost all patients ------------------
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

# HF_Drug_Histories <- Stages %>% select(patient) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, generic_name)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories$generic_name <- str_replace_all(HF_Drug_Histories$generic_name, " ", "_")

temp_full <- HF_Drug_Histories %>% 
  select(patient, weight, generic_name) %>% distinct() %>%
    left_join(Stages %>% select(-weight)) %>% mutate(Treat=1) %>% select(-weight) %>%
  spread(key=generic_name, value=Treat)

temp_full[is.na(temp_full)] <- 0


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- temp_full %>% select(patient) %>% left_join(HF_Comorbidity_Inventories) %>% select(patient, diagnosis)
HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% filter(grepl("C", diagnosis)|grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|
                                        grepl("H", diagnosis)|grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|
                                        grepl("M", diagnosis)|grepl("N", diagnosis)|grepl("R", diagnosis))

HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% mutate(Treat=1) %>%
  spread(key=diagnosis, value=Treat)

HF_Comorbidity_Inventories[is.na(HF_Comorbidity_Inventories)] <- 0

temp_full <- temp_full %>% left_join(HF_Comorbidity_Inventories)

temp_full_nopat <- temp_full[,2:1099]
temp_full_nopat <- temp_full_nopat %>% select(-Stages)

83946

0.21*83946 = 17629
0.71*83946 = 59601
0.95*83946 = 79749,  83946


data.frame(
  temp_full %>% select(patient) %>% 
    bind_cols(
      data.frame(
        predict(lm_model, temp_full_nopat)
        )
      )
  ) %>%
  left_join(temp_full) %>%
  mutate(predict.lm_model..temp_full_nopat.=round(predict.lm_model..temp_full_nopat.)) %>%
  mutate(predict.lm_model..temp_full_nopat.=ifelse(predict.lm_model..temp_full_nopat.<=1,1,
                                                   ifelse(predict.lm_model..temp_full_nopat.>=4,4,
                                                          predict.lm_model..temp_full_nopat.))) %>%
  group_by(predict.lm_model..temp_full_nopat., Major_Surgery) %>% count()

summary(lm_model)

ignore <- data.frame(
  temp_full %>% select(patient) %>% 
    bind_cols(
      data.frame(
        predict(lm_model, temp_full_nopat)
        )
      )
  ) %>%
  mutate(predict.lm_model..temp_full_nopat.=round(predict.lm_model..temp_full_nopat.)) %>%
  mutate(predict.lm_model..temp_full_nopat.=ifelse(predict.lm_model..temp_full_nopat.<=1,1,
                                                   ifelse(predict.lm_model..temp_full_nopat.>=4,4,
                                                          predict.lm_model..temp_full_nopat.))) %>%
  rename("PredictedStage"="predict.lm_model..temp_full_nopat.")

fwrite(ignore, "Predicted_Stages.txt", sep="\t")

data.frame(predict(lm_model, temp_full_nopat)) %>%
  arrange(predict.lm_model..temp_full_nopat.) %>%
  mutate(RowN = row_number()) %>%
  mutate(Predicted.Stage=ifelse(RowN<=17629, "1", ifelse(RowN<=59601, "2", ifelse(RowN<=79749,"3", "4")))) %>%
  ggplot(aes(predict.lm_model..temp_full_nopat., fill=Predicted.Stage, colour=Predicted.Stage)) +
  geom_histogram(alpha=0.5,bins=100)  +
  theme_minimal() + 
  xlim(0,5) +
  ggsci::scale_colour_jama() +
  ggsci::scale_fill_jama() +
  xlab("\n Predicted Score/Stage") +
  ylab("Patient Sample Count \n")



data.frame(
  temp_full %>% select(patient) %>% 
    bind_cols(
      data.frame(
        predict(lm_model, temp_full_nopat)
        )
      )
  ) %>%
  left_join(temp_full) %>%
  mutate(predict.lm_model..temp_full_nopat.=round(predict.lm_model..temp_full_nopat.)) %>%
  mutate(predict.lm_model..temp_full_nopat.=ifelse(predict.lm_model..temp_full_nopat.<=1,1,
                                                   ifelse(predict.lm_model..temp_full_nopat.>=4,4,
                                                          predict.lm_model..temp_full_nopat.))) %>%
  group_by(predict.lm_model..temp_full_nopat., Propranolol) %>% count()

# Heart_Transplant 0% 0%  1%  2%
# Hospitalization 3% 5% 11% 14%
# Iron Injection 7% 6% 10% 17%
# Furosemide_Injection  6% 10% 20% 30%
# Hydralazine_Injection 6% 6% 9% 14%
# Major_Surgery 2% 4% 7% 7%




names(temp_full_nopat) <- paste0("A",names(temp_full_nopat))

data.frame(predict(modelAll_1_gbm, temp_full_nopat)) %>%
  arrange(predict.modelAll_1_gbm..temp_full_nopat.) %>%
  mutate(RowN = row_number()) %>%
  mutate(Predicted.Stage=ifelse(RowN<=17629, "1", ifelse(RowN<=59601, "2", ifelse(RowN<=79749,"3", "4")))) %>%
  ggplot(aes(predict.modelAll_1_gbm..temp_full_nopat., fill=Predicted.Stage, colour=Predicted.Stage)) +
  geom_histogram(alpha=0.5,bins=100)  +
  theme_minimal() + 
  #xlim(0,5) +
  ggsci::scale_colour_jama() +
  ggsci::scale_fill_jama() +
  xlab("\n Predicted Score/Stage") +
  ylab("Patient Sample Count \n")


data.frame(
  temp_full %>% select(patient) %>% 
    bind_cols(
      data.frame(
        predict(modelAll_1_gbm, temp_full_nopat)
        )
      )
  ) %>%
  left_join(temp_full) %>%
  arrange(predict.modelAll_1_gbm..temp_full_nopat.) %>%
  mutate(RowN = row_number()) %>%
 mutate(Predicted.Stage=ifelse(RowN<=17629, "1", ifelse(RowN<=59601, "2", ifelse(RowN<=79749,"3", "4")))) %>%
  group_by(Predicted.Stage, Torsemide) %>% count()



ignore <- data.frame(
  temp_full %>% select(patient) %>% 
    bind_cols(
      data.frame(
        predict(modelAll_1_gbm, temp_full_nopat)
        )
      )
  ) %>%
 arrange(predict.modelAll_1_gbm..temp_full_nopat.) %>%
  mutate(RowN = row_number()) %>%
  mutate(Predicted.Stage=ifelse(RowN<=17629, "1", ifelse(RowN<=59601, "2", ifelse(RowN<=79749,"3", "4")))) %>%
  rename("PredictedStage"="predict.modelAll_1_gbm..temp_full_nopat.") %>%
  select(1,4)

fwrite(ignore, "Predicted_Stages_gbm.txt", sep="\t")

summary(modelAll_1_gbm)

ignore %>% left_join(temp_full) %>%
  group_by(Predicted.Stage) %>% count()

# ------------------------------------------
# Estimate Sizing - DEAD - Pats Systolic vs Diastolic ------------------------------------------------------

DANU_Demographics_Dead <- fread("DANU Demographics Dead.txt", colClasses = "character")

Dead_pats <- DANU_Demographics_Dead %>% filter(heart_failure_condition !="-") %>%
  mutate(death_date=as.Date(death_date)) %>% filter(death_date>"2020-05-01") %>%
  select(patid, weight)

names(Dead_pats)[1] <- "patient"

sum(as.numeric(Dead_pats$weight)) # 1027214

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis == "Heart Failure")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, description)

DANU_Dossiers <- fread("DANU Dossiers Full.txt")
names(DANU_Dossiers)[1] <- "patient"

DANU_Dossiers <- Dead_pats %>% select(patient) %>% inner_join(DANU_Dossiers)

DANU_Dossiers <- DANU_Dossiers %>% select(patient, weight, code, earliest, latest, frequency)

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1027214

DANU_Dossiers <- DANU_Dossiers %>% left_join(DANU_Diagnosis_Codes)

DANU_Dossiers <- DANU_Dossiers %>% drop_na()

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1027214

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs!="104")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% inner_join(Dead_pats)

HF_Drug_Histories %>% select(-weight) %>% inner_join(DANU_Dossiers) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))  # 945607

DANU_Dossiers <- HF_Drug_Histories %>% select(-weight) %>%  inner_join(DANU_Dossiers) 


# Must have specified systolic or dyastolic 
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 656595.3
  
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 607122

unique(DANU_Dossiers$description)

# Must have at least 2 different codes
DANU_Dossiers %>%  # filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 518797

# Must have specified systolic or dyastolic and have at least 2 different codes
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%  
select(patient, code) %>% distinct() %>%
  group_by(patient) %>% count() %>% filter(n>1) %>%
  select(patient) %>% distinct() %>% ungroup() %>%
  left_join(DANU_Dossiers) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 185252

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

#   Diastolic Systolic      n
#       <dbl>    <dbl>  <dbl>
# 1         1        1 60605.
# 2         1       NA 77104.
# 3        NA        1 47543.

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

#   Diastolic Systolic       n
#       <dbl>    <dbl>   <dbl>
# 1         1        1 112960.
# 2         1       NA 298804.
# 3        NA        1 195358..

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
  filter(Diastolic==1 & is.na(Systolic)) %>% select(patient) %>% distinct()

fwrite(Diastolic_Pats, "Diastolic_Pats_Dead.txt")

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
  filter(Systolic==1 & is.na(Diastolic)) %>% select(patient) %>% distinct()

fwrite(Systolic_Pats, "Systolic_Pats_Dead.txt")


Systolic_Dead <- fread("Systolic_Pats_Dead.txt", sep="\t")
Systolic_Dead$group <- "Systolic"
Diastolic_Dead <- fread("Diastolic_Pats_Dead.txt", sep="\t")
Diastolic_Dead$group <- "Diastolic"

Groups_Diastolic_vs_Systolic <- fread("Groups_Diastolic_vs_Systolic.txt")
Groups_Diastolic_vs_Systolic_ALL <-  Groups_Diastolic_vs_Systolic %>% bind_rows(Systolic_Dead) %>% bind_rows(Diastolic_Dead)

fwrite(Groups_Diastolic_vs_Systolic_ALL, "Groups_Diastolic_vs_Systolic_ALL.txt")
Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt")


# ------------------------------------
# DEAD Patients - All Drugs-  Drug Usage Ever & Last 12 months Systolic vs Diastolic -----------------------------------------------------
Systolic <- fread("Systolic_Pats_Dead.txt", sep="\t")
Diastolic <- fread("Diastolic_Pats_Dead.txt", sep="\t")

Systolic$group <- "Systolic"
Diastolic$group <- "Diastolic"

Groups <- Systolic %>% bind_rows(Diastolic)


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- Groups %>% left_join(HF_Drug_Histories)

HF_Drug_Histories %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 


data.frame(HF_Drug_Histories %>% 
  select(patient, weight, group, drug_class, drug_group) %>% distinct() %>%
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_")) 





# ----------------------------
# Estimate Sizing All Pats Systolic vs Diastolic INCLUDING DEAD ------------------------------------------------------
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
  filter(Diastolic==1 & is.na(Systolic)) %>% select(patient) %>% distinct()

First_Diastolic <- Diastolic_Pats %>% left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>%
  mutate(earliest=as.Date(earliest)) %>%
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>% ungroup() %>% select(patient, earliest)

fwrite(First_Diastolic, "First_Diastolic_All.txt")


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
  filter(Systolic==1 & is.na(Diastolic)) %>% select(patient) %>% distinct()

First_Systolic <- Systolic_Pats %>% left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>%
  mutate(earliest=as.Date(earliest)) %>%
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>% ungroup() %>% select(patient, earliest)

fwrite(First_Systolic, "First_Systolic_All.txt")


# ---------------------------------------------
# gradient boost patients with NYHA - ALL Patients inc DEAD  ------------------

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

HF_Drug_Histories_exp <- HF_Demographics %>% inner_join(HF_Drug_Histories) %>% select(patient)


DANU_Measures <- fread("DANU Measures Full.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="NYHA Class")
DANU_Measures <- DANU_Measures %>% select(-c(source, description, vague_value, vague_date, metric))
DANU_Measures %>% select(patid, weight, test) %>% distinct() %>% group_by(test) %>% summarise(n=sum(weight)) # 1078741
names(DANU_Measures)[1] <- "patient" 

DANU_Measures <- HF_Drug_Histories_exp %>% inner_join(DANU_Measures)

DANU_Measures <- DANU_Measures %>% group_by(patient) %>% filter(claimed==max(claimed)) %>% slice(1) %>%
 # mutate(value=round(value)) %>%
  select(-c(test, unit)) %>% ungroup()

DANU_Measures %>% select(patient, weight, value) %>% distinct() %>% group_by(value) %>% summarise(n=sum(weight))

DANU_Measures %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 591517
 
Stages <- DANU_Measures %>% select(patient, weight, value) %>% distinct()
names(Stages)[3] <- "Stages"

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 17277771

HF_Drug_Histories <- Stages %>% select(patient) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs != "104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, generic_name)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

unique(HF_Drug_Histories$generic_name)

HF_Drug_Histories$generic_name <- str_replace_all(HF_Drug_Histories$generic_name, " ", "_")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 591517

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% inner_join(Stages %>% select(-weight)) %>% 
  mutate(Stages=round(Stages)) %>% group_by(Stages) %>%
  summarise(n=sum(as.numeric(weight))) 

# 1      1 117201. 20%
# 2      2 289569. 49%
# 3      3 153269. 26%
# 4      4  31477. 5%

temp_short <- HF_Drug_Histories %>% 
  select(patient, weight, generic_name) %>% distinct() %>%
    left_join(Stages %>% select(-weight)) %>% mutate(Treat=1) %>% select(-weight) %>%
  spread(key=generic_name, value=Treat)

temp_short[is.na(temp_short)] <- 0

HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- temp_short %>% select(patient) %>% left_join(HF_Comorbidity_Inventories) %>% select(patient, diagnosis)
HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% filter(grepl("C", diagnosis)|grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|
                                        grepl("H", diagnosis)|grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|
                                        grepl("M", diagnosis)|grepl("N", diagnosis)|grepl("R", diagnosis))

HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% mutate(Treat=1) %>%
  spread(key=diagnosis, value=Treat)

HF_Comorbidity_Inventories[is.na(HF_Comorbidity_Inventories)] <- 0

temp_short <- temp_short %>% left_join(HF_Comorbidity_Inventories)

temp_short_nopat <- temp_short[,2:1027]

temp_short_nopat # 4179

0.20*4179 = 836
0.69*4179 = 2884
0.95*4179 = 3970,  4179

names(temp_short_nopat) <- paste0("A",names(temp_short_nopat))

modelAll_1_gbm <- gbm(AStages ~ . , data = temp_short_nopat)
summary(modelAll_1_gbm)

predict(modelAll_1_gbm, temp_short_nopat)

data.frame(predict(modelAll_1_gbm, temp_short_nopat)) %>%
  arrange(predict.modelAll_1_gbm..temp_short_nopat.) %>%
  mutate(RowN = row_number()) %>%
  mutate(Predicted.Stage=ifelse(RowN<=836, "1", ifelse(RowN<=2884, "2", ifelse(RowN<=3970,"3", "4")))) %>%
  #group_by(ColorStage) %>% count()
  ggplot(aes(predict.modelAll_1_gbm..temp_short_nopat., fill=Predicted.Stage, colour=Predicted.Stage)) +
  geom_histogram(alpha=0.7,bins=100)  +
  theme_minimal() + 
  ggsci::scale_colour_jama() +
  ggsci::scale_fill_jama() +
  xlab("\n Predicted Score/Stage") +
  ylab("Patient Sample Count \n")


# gradient boost all patients - ALL Patients inc DEAD ------------------
HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"

sum(HF_Demographics$weight) # 17277771

HF_Demographics %>% filter(died=="N" | death_date>"2020-05-01" ) %>% summarise(n=sum(weight)) # 14363507

HF_Demographics <- HF_Demographics %>% filter(died=="N" | death_date>"2020-05-01" )

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs != "104")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient) %>% distinct()

HF_Demographics %>% inner_join(HF_Drug_Histories) %>% summarise(n=sum(weight)) # 13035527

HF_Drug_Histories_exp <- HF_Demographics %>% inner_join(HF_Drug_Histories) %>% select(patient)


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- HF_Drug_Histories_exp %>% select(patient) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs != "104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, generic_name)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

unique(HF_Drug_Histories$generic_name)

HF_Drug_Histories$generic_name <- str_replace_all(HF_Drug_Histories$generic_name, " ", "_")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 12987606

temp_full <- HF_Drug_Histories %>% 
  select(patient, weight, generic_name) %>% distinct() %>%
    left_join(Stages %>% select(-weight)) %>% mutate(Treat=1) %>% select(-weight) %>%
  spread(key=generic_name, value=Treat)

temp_full[is.na(temp_full)] <- 0


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- temp_full %>% select(patient) %>% left_join(HF_Comorbidity_Inventories) %>% select(patient, diagnosis)
HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% filter(grepl("C", diagnosis)|grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|
                                        grepl("H", diagnosis)|grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|
                                        grepl("M", diagnosis)|grepl("N", diagnosis)|grepl("R", diagnosis))

HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% mutate(Treat=1) %>%
  spread(key=diagnosis, value=Treat)

HF_Comorbidity_Inventories[is.na(HF_Comorbidity_Inventories)] <- 0

temp_full <- temp_full %>% left_join(HF_Comorbidity_Inventories)

temp_full_nopat <- temp_full[,2:1101]
temp_full_nopat <- temp_full_nopat %>% select(-Stages)

91336

0.20*91336 = 18267
0.69*91336 = 63022
0.95*91336 = 86769,  91336

names(temp_full_nopat) <- paste0("A",names(temp_full_nopat))

data.frame(predict(modelAll_1_gbm, temp_full_nopat)) %>%
  arrange(predict.modelAll_1_gbm..temp_full_nopat.) %>%
  mutate(RowN = row_number()) %>%
  mutate(Predicted.Stage=ifelse(RowN<=18267, "1", ifelse(RowN<=63022, "2", ifelse(RowN<=86769,"3", "4")))) %>%
  ggplot(aes(predict.modelAll_1_gbm..temp_full_nopat., fill=Predicted.Stage, colour=Predicted.Stage)) +
  geom_histogram(alpha=0.5,bins=100)  +
  theme_minimal() + 
  #xlim(0,5) +
  ggsci::scale_colour_jama() +
  ggsci::scale_fill_jama() +
  xlab("\n Predicted Score/Stage") +
  ylab("Patient Sample Count \n")


data.frame(
  temp_full %>% select(patient) %>% 
    bind_cols(
      data.frame(
        predict(modelAll_1_gbm, temp_full_nopat)
        )
      )
  ) %>%
  left_join(temp_full) %>%
  arrange(predict.modelAll_1_gbm..temp_full_nopat.) %>%
  mutate(RowN = row_number()) %>%
 mutate(Predicted.Stage=ifelse(RowN<=18267, "1", ifelse(RowN<=63022, "2", ifelse(RowN<=86769,"3", "4")))) %>%
  group_by(Predicted.Stage, Hospital_Stay) %>% count()



ignore <- data.frame(
  temp_full %>% select(patient) %>% 
    bind_cols(
      data.frame(
        predict(modelAll_1_gbm, temp_full_nopat)
        )
      )
  ) %>%
 arrange(predict.modelAll_1_gbm..temp_full_nopat.) %>%
  mutate(RowN = row_number()) %>%
  mutate(Predicted.Stage=ifelse(RowN<=18267, "1", ifelse(RowN<=63022, "2", ifelse(RowN<=86769,"3", "4")))) %>%
  rename("PredictedStage"="predict.modelAll_1_gbm..temp_full_nopat.") %>%
  select(1,4)

fwrite(ignore, "Predicted_Stages_gbm_All.txt", sep="\t")

summary(modelAll_1_gbm)

ignore %>% left_join(temp_full) %>%
  left_join(HF_Drug_Histories %>% select(patient, weight) %>% distinct()) %>%
  group_by(Predicted.Stage) %>% summarise(n=sum(as.numeric(weight)))

# ------------------------------------------

# Age in Systolic vs Diastolic - ALL patients inc. DEAD --------------------------------------
Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")
HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% select(patient, weight, age)

Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Demographics) %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))


data.frame(Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Demographics) %>%
  group_by(age)  %>% summarise(n=sum(as.numeric(weight))))

  Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Demographics) %>%
  group_by(group)  %>% summarise(n=weighted.mean(age, as.numeric(weight)))
  
  

Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Demographics)  %>%
  ggplot(aes(age, colour=group, fill=group)) +
  geom_histogram(bins=72, alpha=0.9) +
  theme_minimal() +
  scale_color_manual(values = c("blue4", "brown3")) +
  scale_fill_manual(values = c("blue4", "brown3")) +
  facet_wrap(~group, ncol = 1) +
  scale_x_continuous(breaks=seq(18,89,5)) +
  ylab("Number of Patient Samples \n") +
  xlab("\n Age (years) 18 to 89 y/o")


# -----------------------------------
# All PAts inc death Drugs-  Drug Usage Ever & Last 12 months & last month  -----------------------------------------------------

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

TO_track <- HF_Demographics %>% inner_join(HF_Drug_Histories) %>% select(patient)


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- TO_track %>% left_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

#HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, drug_group) %>% distinct() %>%
  group_by(drug_group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                   mutate(drug_group=str_replace(drug_group, " ", "_")) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))


# -------------------------------------------------
# Create new MECE stocks All target pats inc. dead -----------------

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
TO_track <- HF_Demographics %>% inner_join(HF_Drug_Histories) %>% select(patient)


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838

HF_Drug_Histories <- TO_track %>% left_join(HF_Drug_Histories)
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)


string_AdvancedProcedure <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Cardiac Device"|
                                                                      HF_Ingredients$drug_group=="Hospitalization"], collapse = "|"),")\\b")

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")

string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")

string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

string_OtherAdvanced <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Advanced Therapy"], collapse = "|"),")\\b")

string_Injectables <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")

string_Oral <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Oral Therapy"], collapse = "|"),")\\b")

# Surgery/Transplant/Hospital/Devices 1
# Neprilysin inhibitor 2
# MRA 3
# Injectables 4
# SGLT2 5
# Other Advanced 6
# Orals* 7
# Orals 8
# dead 9
# lapsed 10

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(Stock = ifelse(grepl(string_AdvancedProcedure, Drugs), 1,
                                            ifelse(grepl(string_MRA, Drugs), 2, 
                                                   ifelse(grepl(string_ARNI, Drugs), 3,
                                                      ifelse(grepl(string_Injectables, Drugs), 4, 
                                                             ifelse(grepl(string_SGLT2, Drugs), 5,
                                                                    ifelse(grepl(string_OtherAdvanced, Drugs), 6, 
                                                                           ifelse(grepl(string_Oral, Drugs), 8, 
                                                                                  ifelse(grepl("104", Drugs), 9, 10)))))))))


HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient) %>% mutate(AdvExp = cumsum(Stock==1))

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(Stock2=ifelse(Stock==8&AdvExp!=0,7,Stock))
HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(disease, patient, weight, Month, Stock2)
HF_Drug_Histories <- HF_Drug_Histories %>% spread(key=Month, value=Stock2)

fwrite(HF_Drug_Histories, "HF_Box_Histories_Paulo2.txt", sep="\t")


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- TO_track %>% left_join(HF_Drug_Histories)
HF_Box_Histories     <- fread("HF_Box_Histories_Paulo2.txt", colClasses = "character")




# Flows table in long format
flHF <- HF_Drug_Histories
flHF <- flHF[,disease := NULL]

flHF <- melt(flHF, id = c("patient","weight"))
names(flHF)[c(3,4)] <- c("p1","v1")
flHF <- flHF[, p1 := str_extract(p1,"[:digit:]+")]
flHF$p1 <- as.numeric(flHF$p1)
flHF <- data.frame(cbind(flHF[p1 < 60], flHF[p1 > 1,.(p2 = p1, v2 = v1)]), stringsAsFactors = F)
flHF <- flHF[,c(1:3,5,4,6)]

# Any flow flag and stops flag
flHF <- setDT(flHF)[, flow := (v1 != v2)*1]
flHF <- flHF[, stops := (flow == 1 & v2 == "-")*1]

# Treatment experience
RxExp <- data.frame(HF_Drug_Histories, stringsAsFactors = F)
RxExp$month1 <- (RxExp$month1 != "-")*1

for(i in 2:60){
  cat(i)
  RxExp[,i+2] <- (((RxExp[,i+2] != "-")*1 + RxExp[,i+2-1]) > 0)*1
}

RxExp <- setDT(RxExp)
RxExp <- melt(RxExp, id = c("patient","weight"))
RxExp <- RxExp[, month := str_extract(variable,"[:digit:]+")]
RxExp$month <- as.numeric(RxExp$month)
names(RxExp)[4] <- "HF_RxExp"

flHF <- RxExp[,.(patient,month,HF_RxExp)][flHF, on = .(patient, month = p1)]
flHF <- flHF[,.(patient, weight, p1 = month, p2, v1, v2, p1_RxExp = HF_RxExp, flow, stops)]

# Starts and re-starts flag
flHF <- flHF[, starts := (flow == 1 & v1 == "-" & p1_RxExp == 0)*1]
flHF <- flHF[, re_starts := (flow == 1 & v1 == "-" & p1_RxExp == 1)*1]
flHF <- flHF[, disease := "HF US"]
flHF <- flHF[,c(12,1:11)]


# Bring Therapy classes (Stocks) to the table
HF_Box_Histories <- HF_Box_Histories[,disease := NULL]
HF_Box_Histories <- data.frame(HF_Box_Histories, stringsAsFactors = F)


#  We have 10 stocks now !!!

# for(i in 1:60){
#   cat(i)
#   HF_Box_Histories[,i+2] <- unlist(lapply(HF_Box_Histories[,i+2],function(x) str_sub(x, 1L, 1L)))
# }


setDT(HF_Box_Histories) 
HF_Box_Histories <- melt(HF_Box_Histories, id = c("patient","weight"))
names(HF_Box_Histories)[c(3,4)] <- c("p","s")
HF_Box_Histories <- HF_Box_Histories[, p := str_extract(p,"[:digit:]+")]
HF_Box_Histories$p <- as.numeric(HF_Box_Histories$p)


flHF <- flHF %>% mutate(weight=as.numeric(weight)) %>% 
  left_join(HF_Box_Histories %>% mutate(weight=as.numeric(weight)) , by=c("patient"="patient", "weight"="weight", "p1"="p")) %>%
  rename("s1"="s")


flHF <- flHF %>% mutate(weight=as.numeric(weight)) %>% 
  left_join(HF_Box_Histories %>% mutate(weight=as.numeric(weight)), by=c("patient"="patient", "weight"="weight", "p2"="p")) %>%
  rename("s2"="s")

names(flHF)[c(6,7)] <- c("d1","d2")

fwrite(flHF,"HF_Flows_Aux._Long2.txt")



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
TO_track <- HF_Demographics %>% inner_join(HF_Drug_Histories) %>% select(patient)


TO_track %>% left_join(flHF) %>% filter(p2==60) %>% group_by(s2) %>% summarise(pats=sum(as.numeric(weight))) 



# ------------------------------------------
# All PAts inc death Drugs-  Drug Usage Ever & Last 12 months & last month SYTOLIC vs DIASTOLIC  -----------------------------------------------------

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)

sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

#HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

#HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

#HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, drug_group, group) %>% distinct() %>%
  group_by(group, drug_group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                   mutate(drug_group=str_replace(drug_group, " ", "_")) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))


# -----------------------------------------------------------
# All PAts inc death Drugs-  Number of patient son each class over time - SYTOLIC vs DIASTOLIC  -----------------------------------------------------

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)

sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Month, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients %>% select(-drug_group)) 

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Month, drug_class, group) %>% distinct() 

data.frame(HF_Drug_Histories %>% mutate(Month=parse_number(as.character(Month))) %>%
  group_by(group, Month, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>%
  spread(key=Month, value=n)) %>% filter(group=="Systolic") %>%
  arrange(group, -X60)






HF_Drug_Histories %>% mutate(Month=parse_number(as.character(Month))) %>%
  mutate(drug_class=str_replace(drug_class, " ", "_")) %>%
  group_by(group, Month, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>%
  filter(!is.na(drug_class) & drug_class!="Death") %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Month, n, colour=drug_class)) +
  geom_line(size=1) +
  facet_wrap(~group) +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7","#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))

# -----------------------------------------
# All PAts inc death Drugs-  per stock on month60 - SYTOLIC vs DIASTOLIC  -----------------------------------------------------

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

HF_Flows_Aux._Long2 <- fread("HF_Flows_Aux._Long2.txt")

Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Flows_Aux._Long2) %>% filter(p2==60) %>% group_by(group, s2) %>% summarise(pats=sum(as.numeric(weight))) 

# -----------------------------------------
# All PAts inc death Drugs-  Lines And Classes - SYTOLIC vs DIASTOLIC  -----------------------------------------------------

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)

sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Drug_Histories %>% select(patient, weight, group, Drugs) %>% distinct() %>%
  group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  mutate(n=ifelse(n>=10,10,n)) %>%
  group_by(group,n) %>% summarise(total=sum(as.numeric(weight)))

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 



HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
   group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  mutate(n=ifelse(n>=5,5,n)) %>%
  group_by(group, n) %>% summarise(total=sum(as.numeric(weight)))




# ----------------------
# All PAts inc death Drugs-  Lines And Classes Exc Procedures - SYTOLIC vs DIASTOLIC  -----------------------------------------------------

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)

sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 5726651 treat-experienced

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

unique(HF_Ingredients$drug_group)

HF_Ingredients <- HF_Ingredients %>% filter(drug_group!="Cardiac Device"&drug_group!="Hospitalization")

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
unique(HF_Ingredients$drug_class)

string_Short_Drugs        <- paste0("\\b(",paste0(HF_Ingredients$molecule, collapse = "|"),")\\b")

HF_Drug_Histories <- HF_Drug_Histories %>% filter(grepl(string_Short_Drugs, Drugs))

HF_Drug_Histories <- HF_Drug_Histories %>% arrange(patient, weight, Month, Drugs) %>%
  group_by(patient, weight, Month) %>% mutate(treat_new = paste(Drugs, collapse=",")) 

HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(patient, weight, Month, treat_new) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% spread(key=Month, value=treat_new)

HF_Drug_Histories[is.na(HF_Drug_Histories)] <- "-"

sum(as.numeric(HF_Drug_Histories$weight))

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Drug_Histories %>% left_join(Groups_Diastolic_vs_Systolic_ALL) %>%
  select(patient, weight, group, Drugs) %>% distinct() %>%
  group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  mutate(n=ifelse(n>=10,10,n)) %>%
  group_by(group,n) %>% summarise(total=sum(as.numeric(weight)))

HF_Drug_Histories %>% left_join(Groups_Diastolic_vs_Systolic_ALL) %>%
  select(patient, weight, group, Drugs) %>% distinct() %>%
  group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  group_by(group) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60")


HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>%  left_join(Groups_Diastolic_vs_Systolic_ALL) %>%
  select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 



HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
   group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  group_by(group) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
   group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  mutate(n=ifelse(n>=5,5,n)) %>%
  group_by(group, n) %>% summarise(total=sum(as.numeric(weight)))

# ----------------------------------------
# All PAts inc death  - Class penetrance vs LoT  - Systolic vs Diastolic ----------------------

# No of lines of therapy
Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)

sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-"&Drugs!="104")

LoT <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>%
  group_by(patient, weight, group) %>% count() 

# Classes on month 60

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-"&Drugs!="104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month=="month60") %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 7547588


LoT <- LoT %>% left_join(HF_Drug_Histories) %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, n, drug_class) %>% 
  distinct() %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"none", drug_class)) %>%
  mutate(Treat=1) %>%
  spread(key=drug_class, value=Treat)


data.frame(LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% select(patient, weight, n, group) %>% 
             distinct() %>% group_by(group, n) %>% summarise(totalpats=sum(as.numeric(weight))))
 
names(LoT)

data.frame(
  LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, ACE) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("ACE"="totalpats") %>%
    full_join(
        LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, ARB) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("ARB"="totalpats")) %>%
    full_join(
        LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, ARNI) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("ARNI"="totalpats") ) %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Beta Blocker`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`Beta Blocker`="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Cardiac Device`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Cardiac Device"="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Diuretic  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Diuretic"  ="totalpats")) %>%
      full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Injectables  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Injectables"  ="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Heart Transplant`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Heart Transplant"  ="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Hospital Inpatient`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Hospital Inpatient"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Inotropic  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Inotropic"  ="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, MRA ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("MRA"  ="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, SGLT2  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("SGLT2"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Surgery Inpatient`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Surgery Inpatient"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Vasodilator  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Vasodilator"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Systolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, none  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("none"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Other  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Other"  ="totalpats")) 
  )

# ----------------------------------------
# All PAts inc death  - Fows last 12 months---------- 
HF_Flows_Aux._Long <- fread("HF_Flows_Aux._Long2.txt")

HF_Flows_Aux._Long %>% filter(p2>=49)  %>% filter(flow==1) %>% group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>% spread(key=s2, value=n) %>% 
  arrange(-s1) %>% select(1,11,10:2)

#      s1     `10`     `9`      `8`      `7`      `6`     `5`     `4`     `3`      `2`     `1`
#   <int>    <dbl>   <dbl>    <dbl>    <dbl>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl>
# 1    10      NA  554492. 2040783.  362001.  436517.  59936. 133471.  45282.  173572. 319404.
# 2     8 1967906.  97024. 4833620.      NA   545315. 121365. 157890.  56227.  253754. 310074.
# 3     7  447066.  69669.      NA  1328389.  158045.  27522.  49384.  31745.   79779. 238453.
# 4     6  433497.  32629.  512689.  168085. 1727536.  29878.  66293.  22432.   72627. 149498.
# 5     5   47097.   2445.   91538.   17496.   19754. 330845.   6385.   5465.   15607.  13935.
# 6     4  109792    2912.  150510.   45440.   68635.   6465.  31124.   3734.   11452.  11234.
# 7     3   35346.   3082.   34449.   31079.   14126.   3460.   1042. 298436.   49421.  32702.
# 8     2  161477.  19365.  221946.   85733.   65274.  13859.   6145.  47414. 1465154. 128532.
# 9     1  286687.  23992.      NA   499711.  140034.  14059.  12866.  34473.  138928. 175919.


Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")
HF_Flows_Aux._Long <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Flows_Aux._Long)

data.frame(HF_Flows_Aux._Long %>% filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight, group) %>% count()  %>%
  ungroup() %>% group_by(group, n) %>% summarise(total=sum(weight)))


HF_Flows_Aux._Long %>%  filter(group=="Diastolic")   %>% filter(p2==60) %>% select(patient, weight, s2) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(s2, n) %>% summarise(total=sum(as.numeric(weight)))  %>% ungroup() %>%
  spread(key=s2, value=total)

HF_Flows_Aux._Long  %>%  filter(group=="Diastolic")   %>% filter(p2==60) %>% select(patient, weight, s2) %>% distinct() %>%
  left_join(
    HF_Flows_Aux._Long %>%  filter(p2>=49)  %>% filter(flow==1) %>% group_by(patient, weight) %>% count()) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
   group_by(s2, n)  %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
   ungroup() %>%
   group_by(s2) %>% summarise(mean=weighted.mean(n, weight))




HF_Flows_Aux._Long %>% filter(group=="Diastolic") %>%
  filter(p2>=49)  %>% filter(flow==1) %>% group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>% spread(key=s2, value=n) %>% 
  arrange(-s1) %>% select(1,11,10:2)

#      s1    `10`     `9`      `8`     `7`     `6`    `5`    `4`    `3`     `2`     `1`
#   <int>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>
# 1    10     NA  170907.  480988. 116613. 122330. 13351. 45314.  1925.  37630. 108784.
# 2     8 469445.  26103. 1386239.     NA  173648. 28444. 64825.  2155.  72665. 122780.
# 3     7 143622.  27502.      NA  483646.  58881.  6365. 20247.  2041.  23941. 115898.
# 4     6 129909.  11040.  159804.  65287. 574371.  8776. 28146.  2177.  25540.  63995.
# 5     5   9185.    416.   22346    6019.   5216. 75854.  1643.   268.   3924.   3826.
# 6     4  36635.   2057.   60852.  17974.  31275.  2126. 12662.   127.   3675.   4318.
# 7     3   1737.    471.    1522.   1746.    780.    NA    131. 11662.   1017.   1823.
# 8     2  39997.   6859.   61836.  26224.  21752.  2904.  2337.  1293. 362684.  34390.
# 9     1 107682.   7247.      NA  204807.  60694.  4213.  6404.  2750.  37047.  68615.


HF_Flows_Aux._Long %>% filter(group=="Systolic") %>%
  filter(p2>=49)  %>% filter(flow==1) %>% group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>% spread(key=s2, value=n) %>% 
  arrange(-s1) %>% select(1,11,10:2)

#      s1    `10`     `9`     `8`     `7`     `6`    `5`    `4`     `3`     `2`    `1`
#   <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>   <dbl>   <dbl>  <dbl>
# 1    10     NA  107855. 277428.  92601.  73862. 11004. 31081.  26860.  49258. 96279.
# 2     8 256635.  15743. 674764.     NA   74388. 16306. 20782.  29922.  56954. 72909.
# 3     7 116316.  19493.     NA  350800.  41622.  8885. 13523.  17941.  31095. 62832.
# 4     6  73889.   6547.  74071.  45969. 339052.  6272. 16263.  11441.  17197. 44406.
# 5     5   8998.    755.  10423.   4984.   4407. 59281.  1487.   3069.   4505.  5126.
# 6     4  25885.    628.  20833.  13470.  14526.  1030.  7890.   1531.   3826.  3567.
# 7     3  22193.   1987.  18667.  17713.   8498.  2643.   636. 176208.  31374. 21964.
# 8     2  40476.   4846.  49908.  30337.  18120.  3206.  1972.  32184. 527177. 55045.
# 9     1  79760.   7666.     NA  126166.  42218.  5146.  3310.  21906.  64519. 64389.



# ------------------------------
# All PAts inc death  -Comorbidities Penetrance HF ----------------------------------------------------------

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
TO_track <- HF_Demographics %>% inner_join(HF_Drug_Histories) %>% select(patient)



HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- TO_track %>% inner_join(HF_Comorbidity_Inventories)

HF_Comorbidity_Inventories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 12041998

HF_Comorbidity_Inventories %>% filter(diagnosis=="N18") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #5947969 

HF_Comorbidity_Inventories %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #6288444 #

HF_Comorbidity_Inventories %>% filter(diagnosis=="K76") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #2640718 #

DANU_Demographics <- fread("DANU Demographics Full.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"

DANU_Demographics %>% filter(grepl("Obesity", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 10817775 

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  #  7767878 

DANU_Events <- fread("DANU Events Full.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code ))
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code == max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% filter(code>=30) %>% select(patid) %>% distinct()
names(DANU_Events)[1] <- "patient"

DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 5006559 



# Diastolic
Groups <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", sep=",")
Diastolic <- Groups %>% filter(group=="Diastolic") %>% select(patient)

HF_Comorbidity_Inventories %>% inner_join(Diastolic) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 3366024

HF_Comorbidity_Inventories %>% inner_join(Diastolic) %>% filter(diagnosis=="N18") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #1820457 #

HF_Comorbidity_Inventories %>% inner_join(Diastolic) %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #1863226 # 

HF_Comorbidity_Inventories  %>% inner_join(Diastolic) %>% filter(diagnosis=="K76") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #713195.7 # 


DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>% inner_join(Diastolic) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  #  2087703 

DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>% inner_join(Diastolic) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 1423131 




# Systolic
Groups <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", sep=",")
Systolic <- Groups %>% filter(group=="Systolic") %>% select(patient)

HF_Comorbidity_Inventories %>% inner_join(Systolic) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 2360517

HF_Comorbidity_Inventories %>% inner_join(Systolic) %>% filter(diagnosis=="N18") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #1137513 #

HF_Comorbidity_Inventories %>% inner_join(Systolic) %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #1097944 #

HF_Comorbidity_Inventories  %>% inner_join(Systolic) %>% filter(diagnosis=="K76") %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #433428.1

DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>% inner_join(Systolic) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  #  1359270 


DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>% inner_join(Systolic) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))  # 875600 



# ------------------------
# All PAts inc death  - Class Penetrance 12 months before and after 1st Dx --------------------------------------------------

HF_Demographics <- fread("HF Demographics.txt", colClasses = "character")
HF_Demographics <- HF_Demographics %>% select(patid, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))
names(HF_Demographics)[1] <- "patient"

Groups <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", sep=",")
HF_Demographics <- Groups %>% left_join(HF_Demographics)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, group, Exact_Month) %>% distinct()


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- HF_Demographics %>% select(patient) %>% inner_join(HF_Drug_Histories)

HF_Demographics %>%
  ggplot(aes(Exact_Month)) +
  geom_density(fill="darkslategray4", colour="darkslategray", size=1, alpha=0.5) +
  theme_minimal() +
  xlab("\n Month of 1st Heart Failure Dx") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

names(HF_Demographics)[3] <- "First_HF_Dx"

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs!="104")
HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) 

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients %>% select(-drug_group)) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Demographics) %>% 
  mutate(Lapsed=Month-First_HF_Dx) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(HF_Drug_Histories)

HF_Drug_Histories%>% left_join(Groups) %>% filter(group=="Systolic") %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 1004756

HF_Drug_Histories %>% left_join(Groups) %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


HF_Drug_Histories  %>% left_join(Groups) %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/338792) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=.8) +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))


HF_Drug_Histories %>% left_join(Groups) %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))



HF_Drug_Histories %>% left_join(Groups) %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
    mutate(n=ifelse(drug_class=="ACE", n/193625,
                    ifelse(drug_class=="ARB", n/143340,
                           ifelse(drug_class=="ARNI", n/37922,
                    ifelse(drug_class=="Beta Blocker", n/311873,
                           ifelse(drug_class=="MRA",n/89930,
                                  ifelse(drug_class=="SGLT2",n/19058,
                                         ifelse(drug_class=="Cardiac Device", n/75796,
                                                ifelse(drug_class=="Diuretic", n/261423 ,
                                                       ifelse(drug_class=="Heart Transplant", n/583,
                                                              ifelse(drug_class=="Hospital Inpatient", n/45365,
                                                                     ifelse(drug_class=="Inotropic", n/32848,
                                                                            ifelse(drug_class=="Other", n/12459,
                                                                                   ifelse(drug_class=="Surgery Inpatient", n/31055,
                                                                                          ifelse(drug_class=="Vasodilator", n/70944,n/37185))))))))))))))) %>%
mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class,)) +
  geom_line(size=2, alpha=1) +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % (of Class-experienced) \n") +
    theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))
                                        
# -----------------------------------------------------------------
# All PAts inc death  - Drug Experience ~ stock month60 Diastolic vs Systolic ------------------------------------------------------------------------------
HF_Flows_Aux._Long <- fread("HF_Flows_Aux._Long2.txt")

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")
HF_Flows_Aux._Long <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Flows_Aux._Long)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_AdvancedProcedure <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Cardiac Device"|
                                                                      HF_Ingredients$drug_group=="Hospitalization"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_OtherAdvanced <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Advanced Therapy"], collapse = "|"),")\\b")
string_Injectables <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")
string_Oral <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Oral Therapy"], collapse = "|"),")\\b")


HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_OralExp = ifelse(grepl(string_Oral,d1)|grepl(string_Oral,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = cumsum(p1_OralExp))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = ifelse(p1_OralExp==0,0,1))
 
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_InjExp = ifelse(grepl(string_Injectables,d1)|grepl(string_Injectables,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = cumsum(p1_InjExp))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = ifelse(p1_InjExp==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_OtherAdvanced = ifelse(grepl(string_OtherAdvanced,d1)|grepl(string_OtherAdvanced,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OtherAdvanced = cumsum(p1_OtherAdvanced))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OtherAdvanced = ifelse(p1_OtherAdvanced==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_ARNI = ifelse(grepl(string_ARNI,d1)|grepl(string_ARNI,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ARNI = cumsum(p1_ARNI))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ARNI = ifelse(p1_ARNI==0,0,1))
  
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_SGLT2 = ifelse(grepl(string_SGLT2,d1)|grepl(string_SGLT2,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SGLT2 = cumsum(p1_SGLT2))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SGLT2 = ifelse(p1_SGLT2==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_MRA = ifelse(grepl(string_MRA,d1)|grepl(string_MRA,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_MRA = cumsum(p1_MRA))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_MRA = ifelse(p1_MRA==0,0,1))

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% mutate(p1_AdvancedProcedure = ifelse(grepl(string_AdvancedProcedure,d1)|grepl(string_AdvancedProcedure,d2),1,0))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AdvancedProcedure = cumsum(p1_AdvancedProcedure))
HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AdvancedProcedure = ifelse(p1_AdvancedProcedure==0,0,1))

Start_df_Diastolic <- HF_Flows_Aux._Long %>% filter(group=="Diastolic") %>% filter(p2==60) %>% group_by(s2) %>% summarise(pats=sum(as.numeric(weight))) 
sum(Start_df_Diastolic$pats)

Start_df_Diastolic <- Start_df_Diastolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Diastolic") %>% filter(p2==60) %>% group_by(s2, p1_OralExp) %>% 
                                       summarise(pats_p1_OralExp=sum(as.numeric(weight))) %>% filter(p1_OralExp == 1))

Start_df_Diastolic <- Start_df_Diastolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Diastolic") %>% filter(p2==60) %>% group_by(s2, p1_InjExp) %>% 
                                       summarise(pats_p1_InjExp=sum(as.numeric(weight))) %>% filter(p1_InjExp == 1))

Start_df_Diastolic <- Start_df_Diastolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Diastolic") %>% filter(p2==60) %>% group_by(s2, p1_OtherAdvanced) %>% 
                                       summarise(pats_p1_OtherAdvanced=sum(as.numeric(weight))) %>% filter(p1_OtherAdvanced == 1))

Start_df_Diastolic <- Start_df_Diastolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Diastolic") %>% filter(p2==60) %>% group_by(s2, p1_ARNI) %>% 
                                       summarise(pats_p1_ARNI=sum(as.numeric(weight))) %>% filter(p1_ARNI == 1))

 Start_df_Diastolic <- Start_df_Diastolic %>% left_join(HF_Flows_Aux._Long%>% filter(group=="Diastolic")  %>% filter(p2==60) %>% group_by(s2, p1_SGLT2) %>% 
                                       summarise(pats_p1_SGLT2=sum(as.numeric(weight))) %>% filter(p1_SGLT2 == 1))

Start_df_Diastolic <- Start_df_Diastolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Diastolic") %>% filter(p2==60) %>% group_by(s2, p1_MRA) %>% 
                                       summarise(pats_p1_MRA=sum(as.numeric(weight))) %>% filter(p1_MRA == 1))

Start_df_Diastolic <- Start_df_Diastolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Diastolic") %>% filter(p2==60) %>% group_by(s2, p1_AdvancedProcedure) %>% 
                                       summarise(pats_p1_AdvancedProcedure=sum(as.numeric(weight))) %>% filter(p1_AdvancedProcedure == 1))

Start_df_Diastolic <- Start_df_Diastolic[,c(1,2,4,6,8,10,12,14,16)]

fwrite(Start_df_Diastolic, "Start_df_Diastolic.csv", sep=",")

# change the order and format and all that

Start_df_Diastolic <- fread("Start_df_Diastolic.txt")
  
Start_df_Diastolic <- data.frame(lapply(Start_df_Diastolic, function(x) if(is.numeric(x)) round(x, 0) else x))
  
row.names(Start_df_Diastolic) <- Start_df_Diastolic$Stock
  
Start_df_Diastolic <- Start_df_Diastolic %>% select(-c(Stock))
  
df <- Start_df_Diastolic

  grid.bubble.plot <- function(df, 
                               axis_labels_size=9, 
                               aspect_ratio=1/1,
                               values_text_size=6,
                               values_text_color="black",
                               x_axis_position="top", # or "bottom",
                               bubble_size_range=c(5, 30),
                               bubble_alpha=0.7,
                               bubble_shape=21,
                               bubble_edge_stroke=0) {
    col_names <- colnames(df)
    row_names <- rownames(df)
    values <- as.vector(as.matrix(df))
    values_x <- as.vector(sapply(col_names, function(i) rep(i, nrow(df))))
    values_y <- as.vector(rep(row_names, dim(df)[2]))
    res_df <- data.frame(values = values, values_x = values_x, values_y)
    res_df <- data.frame(res_df %>% mutate(values_x=fct_relevel(values_x,c("Oral_exp","Advanced_Oral_exp","SGLT2_exp","Injectables_exp","ARNi_exp","MRA_exp","Adv_Procedure_exp"))) %>%
                           mutate(values_y=fct_relevel(values_y,c("Dead", "Lapsed_Stock","Oral_Stock","Oral_ProcExp_Stock","Adv_Oral_Stock","SGLT2_Stock","Injectables_Stock","ARNi_Stock","MRA_Stock","Adv_Procedures_Stock"))))
    gg <- ggplot(res_df, aes(x=values_x, y=values_y, size = values, fill=factor(values_x))) +
      geom_point(alpha=bubble_alpha, shape=bubble_shape, stroke=bubble_edge_stroke) +
      scale_size(range = bubble_size_range) +
      scale_colour_manual(values=c("#0099BD","#FFEFCA",  "#F1AC02", "#B482DA", "#FFBFBF", "#AE1641", "#E8F8E8")) +
            scale_fill_manual(values=c("#0099BD","#FFEFCA",  "#F1AC02", "#B482DA", "#FFBFBF", "#AE1641", "#E8F8E8")) +
      scale_x_discrete(position = x_axis_position) +
      scale_y_discrete(limits=rev)+
      geom_text(aes(label=paste0(values,"%")), fontface="bold", size=values_text_size, color=values_text_color,) +
      theme(line=element_blank(), 
            panel.background=element_blank(),
            legend.position="none",
            axis.title=element_blank(),
            axis.text=element_text(size=axis_labels_size),
            aspect.ratio=aspect_ratio)
    gg
  }
  
grid.bubble.plot(Start_df_Diastolic)




Start_df_Systolic <- HF_Flows_Aux._Long %>% filter(group=="Systolic") %>% filter(p2==60) %>% group_by(s2) %>% summarise(pats=sum(as.numeric(weight))) 
sum(Start_df_Systolic$pats)

Start_df_Systolic <- Start_df_Systolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Systolic") %>% filter(p2==60) %>% group_by(s2, p1_OralExp) %>% 
                                       summarise(pats_p1_OralExp=sum(as.numeric(weight))) %>% filter(p1_OralExp == 1))

Start_df_Systolic <- Start_df_Systolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Systolic") %>% filter(p2==60) %>% group_by(s2, p1_InjExp) %>% 
                                       summarise(pats_p1_InjExp=sum(as.numeric(weight))) %>% filter(p1_InjExp == 1))

Start_df_Systolic <- Start_df_Systolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Systolic") %>% filter(p2==60) %>% group_by(s2, p1_OtherAdvanced) %>% 
                                       summarise(pats_p1_OtherAdvanced=sum(as.numeric(weight))) %>% filter(p1_OtherAdvanced == 1))

Start_df_Systolic <- Start_df_Systolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Systolic") %>% filter(p2==60) %>% group_by(s2, p1_ARNI) %>% 
                                       summarise(pats_p1_ARNI=sum(as.numeric(weight))) %>% filter(p1_ARNI == 1))

 Start_df_Systolic <- Start_df_Systolic %>% left_join(HF_Flows_Aux._Long%>% filter(group=="Systolic")  %>% filter(p2==60) %>% group_by(s2, p1_SGLT2) %>% 
                                       summarise(pats_p1_SGLT2=sum(as.numeric(weight))) %>% filter(p1_SGLT2 == 1))

Start_df_Systolic <- Start_df_Systolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Systolic") %>% filter(p2==60) %>% group_by(s2, p1_MRA) %>% 
                                       summarise(pats_p1_MRA=sum(as.numeric(weight))) %>% filter(p1_MRA == 1))

Start_df_Systolic <- Start_df_Systolic %>% left_join(HF_Flows_Aux._Long %>% filter(group=="Systolic") %>% filter(p2==60) %>% group_by(s2, p1_AdvancedProcedure) %>% 
                                       summarise(pats_p1_AdvancedProcedure=sum(as.numeric(weight))) %>% filter(p1_AdvancedProcedure == 1))

Start_df_Systolic <- Start_df_Systolic[,c(1,2,4,6,8,10,12,14,16)]

fwrite(Start_df_Systolic, "Start_df_Systolic.csv", sep=",")

# change the order and format and all that

Start_df_Systolic <- fread("Start_df_Systolic.txt")
  
Start_df_Systolic <- data.frame(lapply(Start_df_Systolic, function(x) if(is.numeric(x)) round(x, 0) else x))
  
row.names(Start_df_Systolic) <- Start_df_Systolic$Stock
  
Start_df_Systolic <- Start_df_Systolic %>% select(-c(Stock))
  
df <- Start_df_Systolic

  grid.bubble.plot <- function(df, 
                               axis_labels_size=9, 
                               aspect_ratio=1/1,
                               values_text_size=6,
                               values_text_color="black",
                               x_axis_position="top", # or "bottom",
                               bubble_size_range=c(5, 30),
                               bubble_alpha=0.7,
                               bubble_shape=21,
                               bubble_edge_stroke=0) {
    col_names <- colnames(df)
    row_names <- rownames(df)
    values <- as.vector(as.matrix(df))
    values_x <- as.vector(sapply(col_names, function(i) rep(i, nrow(df))))
    values_y <- as.vector(rep(row_names, dim(df)[2]))
    res_df <- data.frame(values = values, values_x = values_x, values_y)
    res_df <- data.frame(res_df %>% mutate(values_x=fct_relevel(values_x,c("Oral_exp","Advanced_Oral_exp","SGLT2_exp","Injectables_exp","ARNi_exp","MRA_exp","Adv_Procedure_exp"))) %>%
                           mutate(values_y=fct_relevel(values_y,c("Dead", "Lapsed_Stock","Oral_Stock","Oral_ProcExp_Stock","Adv_Oral_Stock","SGLT2_Stock","Injectables_Stock","ARNi_Stock","MRA_Stock","Adv_Procedures_Stock"))))
    gg <- ggplot(res_df, aes(x=values_x, y=values_y, size = values, fill=factor(values_x))) +
      geom_point(alpha=bubble_alpha, shape=bubble_shape, stroke=bubble_edge_stroke) +
      scale_size(range = bubble_size_range) +
      scale_colour_manual(values=c("#0099BD","#FFEFCA",  "#F1AC02", "#B482DA", "#FFBFBF", "#AE1641", "#E8F8E8")) +
            scale_fill_manual(values=c("#0099BD","#FFEFCA",  "#F1AC02", "#B482DA", "#FFBFBF", "#AE1641", "#E8F8E8")) +
      scale_x_discrete(position = x_axis_position) +
      scale_y_discrete(limits=rev)+
      geom_text(aes(label=paste0(values,"%")), fontface="bold", size=values_text_size, color=values_text_color,) +
      theme(line=element_blank(), 
            panel.background=element_blank(),
            legend.position="none",
            axis.title=element_blank(),
            axis.text=element_text(size=axis_labels_size),
            aspect.ratio=aspect_ratio)
    gg
  }
  
grid.bubble.plot(Start_df_Systolic)


# ----------------------
# All PAts inc death  -  All Exp - All Drugs-  SGLT2 ARNi MRA exp per comorbidity -----------------------------------------------------

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
TO_track <- HF_Demographics %>% inner_join(HF_Drug_Histories) %>% select(patient)

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- TO_track %>% left_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Drugs) %>% distinct()

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(grepl(string_MRA, Drugs)) %>% select(patient) %>% distinct()
MRA_ARNi <- HF_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)) %>% select(patient) %>% distinct()


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- TO_track %>% inner_join(HF_Comorbidity_Inventories)

HF_Comorbidity_Inventories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 12041998

TO_track %>% left_join(HF_Comorbidity_Inventories %>% filter(diagnosis=="N18") %>% select(patient, weight) %>% distinct() %>% 
  mutate(CKD="CKD"))  %>%
  left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(CKD, SGLT2) %>% count()
   
#   CKD   SGLT2     n
#   <chr> <chr> <int>
# 1 CKD   SGLT2  2556
# 2 CKD   NA    38839
# 3 NA    SGLT2  3193
# 4 NA    NA    46748

TO_track %>% left_join(HF_Comorbidity_Inventories %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% select(patient, weight) %>% distinct() %>% 
  mutate(PAD="PAD"))  %>%
  left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(PAD, SGLT2) %>% count()
   
#   PAD   SGLT2     n
#   <chr> <chr> <int>
# 1 PAD   SGLT2  2612
# 2 PAD   NA    41109
# 3 NA    SGLT2  3137
# 4 NA    NA    44478

TO_track %>% left_join(HF_Comorbidity_Inventories %>% filter(diagnosis=="K76") %>% select(patient, weight) %>% distinct() %>% 
  mutate(NAFLD="NAFLD"))  %>%
  left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(NAFLD, SGLT2) %>% count()

#   NAFLD SGLT2     n
#   <chr> <chr> <int>
# 1 NAFLD SGLT2  1630
# 2 NAFLD NA    17164
# 3 NA    SGLT2  4119
# 4 NA    NA    68423


DANU_Demographics <- fread("DANU Demographics Full.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"


TO_track %>% left_join(DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>%
  inner_join(HF_Comorbidity_Inventories) %>%
  select(patient, weight) %>% distinct() %>% mutate(Diabetes="Diabetes")) %>% 
    left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(Diabetes, SGLT2) %>% count()

# 
#   Diabetes SGLT2     n
#   <chr>    <chr> <int>
# 1 Diabetes SGLT2  5636
# 2 Diabetes NA    48971
# 3 NA       SGLT2   113
# 4 NA       NA    36616


DANU_Events <- fread("DANU Events Full.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code ))
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code == max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% filter(code>=30) %>% select(patid) %>% distinct()
names(DANU_Events)[1] <- "patient"


TO_track %>% left_join(DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>%
  select(patient, weight) %>% distinct() %>% mutate(Obesity="Obesity")) %>% 
    left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(Obesity, SGLT2) %>% count()


#   Obesity SGLT2     n
#   <chr>   <chr> <int>
# 1 Obesity SGLT2  2811
# 2 Obesity NA    32607
# 3 NA      SGLT2  2938
# 4 NA      NA    52980



# ------------------------------------------------
#  All PAts inc death  - Months to Class Individual  Diastolic vs Systolic ------------
Groups <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", sep=",")

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))
HF_Drug_Histories <- Groups %>% filter(group=="Systolic") %>% left_join(HF_Drug_Histories)

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")


# Time to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first  Beta blcoker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight))


# Time to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% group_by(patient, weight) %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight))
  
# Time to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight))

# Time to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 



# Time to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 

HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Time to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Time to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Time to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Time to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight))






# ------------------------------------------
#  All PAts inc death  - Lines to Class Individual Diastolic vs Systolic ------------
Groups <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", sep=",")

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))
HF_Drug_Histories <- Groups %>% filter(group=="Systolic") %>% left_join(HF_Drug_Histories)

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")



# Lines to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Lines to first  Beta blcker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 
# Lines to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Lines to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Lines to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 
  
# Lines to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Lines to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Lines to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 



# Lines to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight))


# Lines to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 


HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Lines to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight))

# Lines to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Lines to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Lines to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# -------------------
#  All PAts inc death  - Plot it Individual  Diastolic vs Systolic --------------------------------------------------------------

Months_vs_Lines_to_class <- fread("Months_vs_Lines_to_class_individual_Diastolic_vs_Systolic.txt")

library(ggrepel)
library(hrbrthemes)
library(viridis)

Months_vs_Lines_to_class %>%
ggplot(aes(x=average_months_to_class, y=average_lines_to_class,  
                                     fill=Group, colour=Group)) +
  geom_point(alpha=1, size=5, show.legend = F)+
  geom_text_repel(aes(label = drug_class, colour = Group), 
                  size = 4,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  theme(legend.position = "none",
        #panel.background = element_blank(),
        #panel.grid = element_blank(),
        #axis.ticks = element_blank(),
        text = element_text(size = 10))+
  theme_minimal() +
  xlim(0,35) +
  scale_colour_manual(values=c("#00B0F0", "#F72E19")) +
  xlab("\nAverage Number of Months to Class Initiation")+
  ylab("Average Number of Therapy Lines to Class Initiation\n")

# --------------------------------------------------------
# All PAts inc death Drugs-  Drug Usage Ever & Last 12 months & last month NYHA Stages-----------------------------------------------------

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
Stages <- fread("Predicted_Stages_gbm_All.txt", colClasses = "character", sep="\t")

HF_Drug_Histories <- Stages %>% left_join(HF_Drug_Histories)

sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories %>% group_by(Predicted.Stage) %>% summarise(n=sum(as.numeric(weight)))

#   Predicted.Stage        n
#   <chr>              <dbl>
# 1 1               2589935.
# 2 2               6393928.
# 3 3               3366416.
# 4 4                637326.


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

#HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

#HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

#HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, Predicted.Stage) %>% distinct() %>% left_join(HF_Ingredients) 

data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, drug_group, Predicted.Stage) %>% distinct() %>%
  group_by(Predicted.Stage, drug_group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                   mutate(drug_group=str_replace(drug_group, " ", "_")) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))




# ----------------------------------
# All PAts inc death Drugs-  First Month of Therapy after 12 months lapsed SYTOLIC vs DIASTOLIC  -----------------------------------------------------

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)

sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month<=12) %>% filter(Drugs=="-") %>% group_by(patient) %>% count() %>%
  filter(n==12) %>% select(patient) %>% left_join(HF_Drug_Histories)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month))

HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, group, weight) %>% distinct() %>% group_by(group) %>% summarise(N=sum(as.numeric(weight)))
 
# 1 Diastolic 771164.
# 2 Systolic  628553.


data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

# -------------------------

# All PAts inc death Drugs-  On first Dx -  SYTOLIC vs DIASTOLIC  -----------------------------------------------------

DANU_Demographics <- fread("DANU Demographics Full.txt")
DANU_Demographics <- DANU_Demographics %>% filter(!is.na(heart_failure_onset)) %>% select(patid, heart_failure_onset)
names(DANU_Demographics)[1] <- "patient"

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")
Groups_Diastolic_vs_Systolic_ALL <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(DANU_Demographics)
Groups_Diastolic_vs_Systolic_ALL <- Groups_Diastolic_vs_Systolic_ALL %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

Groups_Diastolic_vs_Systolic_ALL <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month"))
Groups_Diastolic_vs_Systolic_ALL <- Groups_Diastolic_vs_Systolic_ALL %>% select(-heart_failure_onset)
names(Groups_Diastolic_vs_Systolic_ALL)[3] <- "Onset"

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories  %>% filter(Month==Onset) %>% select(-disease)

HF_Drug_Histories %>% select(patient, group, weight) %>% distinct() %>% group_by(group) %>% summarise(N=sum(as.numeric(weight)))
 
#   group            N
# 1 Diastolic 2628678.
# 2 Systolic  1706563.

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 


data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))




# ----------------------------
# All PAts inc death Drugs-  Path to MRA -----------------------------------------------------

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")
HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_ALL %>% left_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

To_track <- HF_Drug_Histories %>% filter(Month<=12) %>% filter(Drugs=="-") %>% group_by(patient) %>% count() %>%
  filter(n==12) %>% select(patient) 

HF_Flows_Aux._Long <- fread("HF_Flows_Aux._Long2.txt")


HF_Flows_Aux._Long <- To_track %>% left_join(HF_Flows_Aux._Long %>% select(patient, weight, s2) %>% distinct())
HF_Flows_Aux._Long <- HF_Flows_Aux._Long 

HF_Flows_Aux._Long <- HF_Flows_Aux._Long %>% filter(s2==2) %>% select(patient) %>% distinct() %>%
  left_join(HF_Flows_Aux._Long) %>% group_by(patient, weight) %>%
  slice(if(any(s2 == 2)) 1:which.max(s2 == 2) else row_number())   

temp <- HF_Flows_Aux._Long %>% 
  group_by(patient, weight) %>%
  mutate(s2=ifelse(s2==1, "P", 
                   ifelse(s2==2, "M",
                          ifelse(s2==3,"A",
                                 ifelse(s2==4,"I",
                                        ifelse(s2==5,"S",
                                               ifelse(s2==6,"oth",
                                                      ifelse(s2==7,"Or*",
                                                             ifelse(s2==8,"Or",
                                                                    ifelse(s2==9,"D","x")))))))))) %>%
  mutate(paths = paste(s2  , collapse=" -> "))  %>%
  ungroup() %>% group_by(paths) %>% summarise(n=sum(weight)) %>% arrange(-n) 

temp %>% mutate(sum=ifelse(grepl("A",paths),"ARNi",
                                 ifelse(grepl("P", paths), "Procedures",
                                              ifelse(grepl("I", paths), "Injectables", "Other Orals")))) %>%
  group_by(sum) %>% summarise(n=sum(n))

#   sum               n
#   <chr>         <dbl>
# 1 ARNi         55490.
# 2 Injectables  57705.
# 3 Other Orals 385894.
# 4 Procedures  194464.

fwrite(temp, "PAths_to_MRA.csv", sep=",")



# ------------------------------------------------
# All PAts inc death  - Persistency  Diastolic vs Systolic ------------
Groups <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", sep=",")

HF_Drug_Histories     <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories     <- HF_Drug_Histories %>% select(-c(disease))
HF_Drug_Histories <- Groups %>% filter(group=="Diastolic") %>% left_join(HF_Drug_Histories)

HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")


# Time to first  Diuretic
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Diuretic,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 25.6


# Time to first  Beta blcoker
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_BetaBlocker,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 32.6


# Time to first  ACE
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_ACE,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 26.1


# Time to first  ARNi
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_ARNI,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 15.9


# Time to first  SGLT2
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_SGLT2,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 13.3

  
# Time to first  MRA
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_MRA,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 20.3


# Time to first ARB
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_ARB,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 25.9


# Time to first Inotropic
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Inotropic,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 17.5


# Time to first Vasodilator
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Vasodilator,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 17.2



# Time to first Other
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Other,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 8.60



# Time to first Cardiac Device
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Cardiac_Device,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 1.58


# Time to first Hospital_Inpatient
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Hospital_Inpatient,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 2.03



# Time to first Surgery_Inpatient
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Surgery_Inpatient,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 1.25


# Time to first Heart_Transplant
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Heart_Transplant,Drugs)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  summarise(mean=weighted.mean(n, as.numeric(weight))) # 1.40


# ------------------
# Patients with both systolic & diastolic ----------------------

Systolic_Plus_Diastolic_Pats <- fread("Systolic_Plus_Diastolic_Pats.txt")


DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis == "Heart Failure")
DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, description)
DANU_Dossiers <- fread("DANU Dossiers Full.txt")
names(DANU_Dossiers)[1] <- "patient"
DANU_Dossiers <- DANU_Dossiers %>% inner_join(DANU_Diagnosis_Codes) %>% inner_join(Systolic_Plus_Diastolic_Pats)

DANU_Dossiers <- DANU_Dossiers %>% select(patient, weight, code, earliest, latest, description)
DANU_Dossiers$earliest <- as.Date(DANU_Dossiers$earliest)
DANU_Dossiers$latest <- as.Date(DANU_Dossiers$latest)

DANU_Dossiers %>% filter(grepl("ystolic", description)) %>% 
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>% rename("FirstSystolic"="earliest") %>% select(-description) %>%
  select(patient, weight, FirstSystolic) %>%
  full_join(
    DANU_Dossiers %>% filter(grepl("iastolic", description)) %>% 
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>%  rename("FirstDiastolic"="earliest") %>% select(-description)  %>%
  select(patient, weight, FirstDiastolic)
  ) %>%
  mutate(Elapsed=as.numeric(FirstSystolic-FirstDiastolic)/365) %>% ungroup() %>%
  ggplot(aes(Elapsed)) +
  geom_density(colour="#024A86", fill="#BBA9BB", size=2) +
  theme_classic() +
  xlab("\n Elapsed Time (Months) from 1st Systolic to 1st Diastolic HF Dx") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 



DANU_Dossiers %>% filter(grepl("ystolic", description)) %>% 
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>% rename("FirstSystolic"="earliest") %>% select(-description) %>%
  select(patient, weight, FirstSystolic) %>%
  full_join(
    DANU_Dossiers %>% filter(grepl("iastolic", description)) %>% 
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>%  rename("FirstDiastolic"="earliest") %>% select(-description)  %>%
  select(patient, weight, FirstDiastolic)
  ) %>%
  mutate(Elapsed=as.numeric(FirstSystolic-FirstDiastolic)/365) %>% ungroup() %>%
  summarise(n=sum(as.numeric(weight)))
# -------------------------------------------
# Filter for Patients Dx Last 5y -------------------------------

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
TO_track <- HF_Demographics %>% inner_join(HF_Drug_Histories) %>% select(patient)

min(HF_Demographics$heart_failure_onset)

HF_Demographics %>% mutate(heart_failure_onset=as.Date(heart_failure_onset)) %>%
  filter(heart_failure_onset>="2016-05-01") %>% select(patient, weight) %>% distinct() %>%
  inner_join(TO_track) %>% summarise(n=sum(weight)) # 10149523

Groups_Diastolic_vs_Systolic_ALL <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", colClasses = "character", sep=",")

Groups_Diastolic_vs_Systolic_ALL %>% inner_join(
  HF_Demographics %>% mutate(heart_failure_onset=as.Date(heart_failure_onset)) %>%
  filter(heart_failure_onset>="2016-05-01") %>% select(patient, weight) %>% distinct()) %>%
  group_by(group) %>% summarise(n=sum(weight))


# 1 Diastolic 3366024.
# 2 Systolic  2360626.

# 1 Diastolic 2628678.
# 2 Systolic  1706563.


TO_track_Dx_LastYear <- HF_Demographics %>% mutate(heart_failure_onset=as.Date(heart_failure_onset)) %>%
  filter(heart_failure_onset>="2016-05-01") %>% select(patient, weight, heart_failure_onset) %>% distinct() %>%
  inner_join(TO_track) 

# -----------------------
# Estimate NEW Sizing Last 5y ------------------------------------------------------
HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"

sum(HF_Demographics$weight) # 17277771


HF_Demographics %>% filter(died=="N" | death_date>"2020-05-01") %>% filter(heart_failure_onset >= "2016-05-01") %>% summarise(n=sum(weight)) # 11282410

HF_Demographics <- HF_Demographics %>% filter(died=="N" | death_date>"2020-05-01") %>% filter(heart_failure_onset >= "2016-05-01")

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs != "104")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient) %>% distinct()

HF_Demographics %>% inner_join(HF_Drug_Histories) %>% summarise(n=sum(weight)) # 10149523


HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset, heart_failure_condition)

HF_Demographics <- HF_Demographics %>% drop_na()

# ALL Heart Failure
sum(HF_Demographics$weight) # 14363507

# ALL Chronic Failure
HF_Demographics %>% filter(heart_failure_condition=="Chronic Heart Failure") %>% summarise(n=sum(weight)) # 11282410

DANU_Diagnosis_Codes <- fread("DANU Diagnosis Codes.txt")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% filter(diagnosis == "Heart Failure")

DANU_Diagnosis_Codes <- DANU_Diagnosis_Codes %>% select(code, description)

DANU_Dossiers <- fread("DANU Dossiers Full.txt")
names(DANU_Dossiers)[1] <- "patient"

DANU_Dossiers <- HF_Demographics %>% select(patient) %>% inner_join(DANU_Dossiers)

DANU_Dossiers <- DANU_Dossiers %>% select(patient, weight, code, earliest, latest, frequency)

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 11282410

DANU_Dossiers <- DANU_Dossiers %>% left_join(DANU_Diagnosis_Codes)

DANU_Dossiers <- DANU_Dossiers %>% drop_na()

DANU_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 11282410

HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-"&Drugs!="104")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient) %>% distinct()

HF_Drug_Histories %>% inner_join(DANU_Dossiers) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))  # 10149523

DANU_Dossiers <- HF_Drug_Histories %>% inner_join(DANU_Dossiers) 

# Must have specified systolic or dyastolic 
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 5894540
  
DANU_Dossiers %>% filter(grepl("ystolic", description)|grepl("iastolic", description)) %>%
  group_by(patient) %>% summarise(n=sum(frequency)) %>% filter(n>1) %>%
  left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)|grepl(5039290)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 5039290




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
# 1         1        1  704049.
# 2         1       NA 2628678.
# 3        NA        1 1706563.

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
  filter(Diastolic==1 & is.na(Systolic)) %>% select(patient) %>% distinct()

First_Diastolic <- Diastolic_Pats %>% left_join(DANU_Dossiers) %>% filter(grepl("iastolic", description)) %>%
  mutate(earliest=as.Date(earliest)) %>%
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>% ungroup() %>% select(patient, earliest)

fwrite(First_Diastolic, "First_Diastolic_All_L5y.txt")


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
  filter(Systolic==1 & is.na(Diastolic)) %>% select(patient) %>% distinct()

First_Systolic <- Systolic_Pats %>% left_join(DANU_Dossiers) %>% filter(grepl("ystolic", description)) %>%
  mutate(earliest=as.Date(earliest)) %>%
  group_by(patient) %>% filter(earliest==min(earliest)) %>% slice(1) %>% ungroup() %>% select(patient, earliest)

fwrite(First_Systolic, "First_Systolic_All_L5y.txt")


# -------------------------------
# All PAts inc death Drugs-  Drug Usage Ever & Last 12 months & last month  Last 5y   -----------------------------------------------------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)




HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-c(disease, Exact_Month))

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, drug_group, group) %>% distinct() %>%
  group_by(group, drug_group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                   mutate(drug_group=str_replace(drug_group, " ", "_")) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
             mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB"|drug_class=="ARNI", "RAAS", drug_class)) %>%
  select(patient, weight, drug_class, drug_group, group) %>% distinct() %>%
  group_by(group, drug_group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                   mutate(drug_group=str_replace(drug_group, " ", "_")) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

# ----------------------------------------------------------------

# All PAts inc death Drugs-  First Month of Therapy after 12 months lapsed Last 5y  -----------------------------------------------------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month<=12) %>% filter(Drugs=="-") %>% group_by(patient) %>% count() %>%
  filter(n==12) %>% select(patient) %>% left_join(HF_Drug_Histories)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient) %>% filter(Month==min(Month))

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, group, weight) %>% distinct() %>% group_by(group) %>% summarise(N=sum(as.numeric(weight)))
 

# 1 Diastolic 646533.
# 2 Systolic  534495.


data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))
# ------------------------------

# Patients on ARNI, SGLT2 or MRA -  Concomitant Classes ON Month 60 - Dx Last 5y  ---------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, month60)) %>% select(patient) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(grepl(string_MRA, month60)) %>% select(patient) %>% distinct()
MRA_ARNi <- HF_Drug_Histories %>% filter(grepl(string_ARNI, month60)) %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, month60)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(month60!="-")
names(HF_Drug_Histories)[4] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")
HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()

MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
1 Diastolic 153829.
2 Systolic  214514.
data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% group_by(group, drug_class) %>%
             mutate(drug_class=str_replace(drug_class, " ", "_")) %>% summarise(n=sum(weight)))


# ------------------------------


# Patients on ARNI, SGLT2 or MRA -   Classes and Lines ever tried After 1st Dx - Dx Last 5y  ---------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, month60)) %>% select(patient) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(grepl(string_MRA, month60)) %>% select(patient) %>% distinct()
ARNI_Pats <- HF_Drug_Histories %>% filter(grepl(string_ARNI, month60)) %>% select(patient) %>% distinct()


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)

names(HF_Drug_Histories)[5] <- "Drugs"
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")


SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight)) 
# 1 Diastolic 55222.
# 2 Systolic  61746.

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=10,10,n)) %>% group_by(group, n) %>% summarise(lines=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=lines))

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))



ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight)) 
# 1 Diastolic   8390.
# 2 Systolic  146421.

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=10,10,n)) %>% group_by(group, n) %>% summarise(lines=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=lines))

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))




MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight)) 
# 1 Diastolic 153829.
# 2 Systolic  214514.

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=10,10,n)) %>% group_by(group, n) %>% summarise(lines=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=lines))

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))



HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()



SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
# 1 Diastolic 55222.
# 2 Systolic  61746.

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

# 1 1   1178.45   474.62
# 2 2   3770.73  1750.28
# 3 3   8832.38  5154.50
# 4 4  16522.32  9644.98
# 5 5  24918.38 44721.56

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))
# 1 Diastolic 4.49226
# 2  Systolic 5.58539




ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
# 1 Diastolic   8390.
# 2 Systolic  146421.

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

# 1 1    223.49    337.72
# 2 2    160.35   4404.52
# 3 3    669.77  14571.21
# 4 4   1170.50  19360.79
# 5 5   6166.39 107746.71

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))
# 1 Diastolic 5.693773
# 2  Systolic 5.596102





MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
# 1 Diastolic 153829.
# 2 Systolic  214514.

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

# 1 1   2057.12    112.02
# 2 2   9925.34   3805.77
# 3 3  25412.17  17092.86
# 4 4  41363.15  40347.44
# 5 5  75070.82 153155.78

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))
# 1 Diastolic 4.524051
# 2  Systolic 5.485027
# ------------------
# Number Different Classes ON month60 - Dx Last 5y  ---------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, group, month60)

names(HF_Drug_Histories)[4] <- "Drugs"
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")


HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()

HF_Drug_Histories %>% group



data.frame(HF_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

# 1 1   1178.45   474.62
# 2 2   3770.73  1750.28
# 3 3   8832.38  5154.50
# 4 4  16522.32  9644.98
# 5 5  24918.38 44721.56

data.frame(HF_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))
# 1 Diastolic 2.110008
# 2  Systolic 2.397244



# ---------------------
#  Classes and Lines ever tried After 1st Dx - Dx Last 5y  ---------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)

names(HF_Drug_Histories)[5] <- "Drugs"
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")


HF_Drug_Histories %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight)) 
# 1 Diastolic 2535004.
# 2 Systolic  1662835.

data.frame(HF_Drug_Histories %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=10,10,n)) %>% group_by(group, n) %>% summarise(lines=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=lines))

data.frame(HF_Drug_Histories %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))
# 1 Diastolic 4.144380
# 2  Systolic 4.710941




HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()



HF_Drug_Histories %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
# 1 Diastolic 2535004.
# 2 Systolic  1662835.

data.frame(HF_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

# 1 1  345689.4 150513.8
# 2 2  515519.5 245506.3
# 3 3  616580.1 347527.1
# 4 4  508987.5 328733.3
# 5 5  548227.9 590554.1

data.frame(HF_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))
# 1 Diastolic 3.301859
# 2  Systolic 3.927493

# ------------------------------
#  Class penetrance vs LoT  - Dx Last 5y ----------------------

# No of lines of therapy
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 



HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-"&Drugs!="104")

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)

LoT <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>%
  group_by(patient, weight, group) %>% count() 

# Classes on month 60

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-"&Drugs!="104")


HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)


HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month==60) %>% left_join(HF_Ingredients) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 7547588


LoT <- LoT %>% left_join(HF_Drug_Histories) %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, n, drug_class) %>% 
  distinct() %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"none", drug_class)) %>%
  mutate(Treat=1) %>%
  spread(key=drug_class, value=Treat)


data.frame(LoT %>% mutate(n=ifelse(n>=15,15,n)) %>% select(patient, weight, n, group) %>% 
             distinct() %>% group_by(group, n) %>% summarise(totalpats=sum(as.numeric(weight))))
 
names(LoT)

data.frame(
  LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, ACE) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("ACE"="totalpats") %>%
    full_join(
        LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, ARB) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("ARB"="totalpats")) %>%
    full_join(
        LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, ARNI) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("ARNI"="totalpats") ) %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Beta Blocker`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename(`Beta Blocker`="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Cardiac Device`) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Cardiac Device"="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Diuretic  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Diuretic"  ="totalpats")) %>%
      full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Injectables  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Injectables"  ="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Heart Transplant`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Heart Transplant"  ="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Hospital Inpatient`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Hospital Inpatient"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Inotropic  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Inotropic"  ="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, MRA ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("MRA"  ="totalpats")) %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, SGLT2  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("SGLT2"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, `Surgery Inpatient`  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Surgery Inpatient"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Vasodilator  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Vasodilator"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, none  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("none"  ="totalpats"))  %>%
  full_join(
    LoT %>% filter(group=="Diastolic") %>% mutate(n=ifelse(n>=15,15,n)) %>% group_by(n, Other  ) %>% summarise(totalpats=sum(as.numeric(weight))) %>% 
  drop_na() %>% select(1,3) %>% rename("Other"  ="totalpats")) 
  )




# -----------------------------------
# Patients on ARNI, SGLT2 or MRA -  Concomitant Classes ON First Start for that class - Dx Last 5y  ---------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)

names(HF_Drug_Histories)[5] <- "Drugs"
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")


SGLT2_Pats <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(grepl(string_MRA, Drugs)) %>% select(patient) %>% distinct()
ARNi_Pats <- HF_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)) %>% select(patient) %>% distinct()





SGLT2_Pats <- SGLT2_Pats %>% left_join(HF_Drug_Histories)
MRA_Pats <- MRA_Pats %>% left_join(HF_Drug_Histories)
ARNi_Pats <- ARNi_Pats %>% left_join(HF_Drug_Histories)

SGLT2_Pats <- SGLT2_Pats %>% group_by(patient, weight) %>% 
  filter(grepl(string_SGLT2, Drugs)) %>% filter(Month==min(Month))
SGLT2_Pats <- separate_rows(SGLT2_Pats, Drugs, sep = ",", convert=T)
SGLT2_Pats <- SGLT2_Pats %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 
SGLT2_Pats <- SGLT2_Pats %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()

SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
# 1 Diastolic 108508.
# 2 Systolic  111194.
data.frame(SGLT2_Pats  %>% group_by(group, drug_class) %>%
             mutate(drug_class=str_replace(drug_class, " ", "_")) %>% summarise(n=sum(weight)))

# 1  Diastolic                ACE  38447.04
# 2  Diastolic                ARB  29210.49
# 3  Diastolic               ARNI    820.19
# 4  Diastolic       Beta_Blocker  62254.21
# 5  Diastolic     Cardiac_Device   1447.52
# 6  Diastolic           Diuretic  59905.38
# 7  Diastolic Hospital_Inpatient   1272.30
# 8  Diastolic        Injectables   1812.87
# 9  Diastolic          Inotropic   2405.28
# 10 Diastolic                MRA   9166.07
# 11 Diastolic              Other    681.25
# 12 Diastolic              SGLT2 108508.27
# 13 Diastolic  Surgery_Inpatient    541.60
# 14 Diastolic        Vasodilator  14385.63
# 15  Systolic                ACE  36140.87
# 16  Systolic                ARB  28707.22
# 17  Systolic               ARNI  22384.11
# 18  Systolic       Beta_Blocker  81033.99
# 19  Systolic     Cardiac_Device   3791.53
# 20  Systolic           Diuretic  55580.39
# 21  Systolic Hospital_Inpatient   2071.83
# 22  Systolic        Injectables    906.82
# 23  Systolic          Inotropic   6763.95
# 24  Systolic                MRA  29294.47
# 25  Systolic              Other    582.08
# 26  Systolic              SGLT2 111194.24
# 27  Systolic  Surgery_Inpatient   1632.12
# 28  Systolic        Vasodilator  12569.54



MRA_Pats <- MRA_Pats %>% group_by(patient, weight) %>% 
  filter(grepl(string_MRA, Drugs)) %>% filter(Month==min(Month))
MRA_Pats <- separate_rows(MRA_Pats, Drugs, sep = ",", convert=T)
MRA_Pats <- MRA_Pats %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 
MRA_Pats <- MRA_Pats %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()

MRA_Pats %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
# 1 Diastolic 371393.
# 2 Systolic  457760.
data.frame(MRA_Pats  %>% group_by(group, drug_class) %>%
             mutate(drug_class=str_replace(drug_class, " ", "_")) %>% summarise(n=sum(weight)))

# 1  Diastolic                ACE  97510.36
# 2  Diastolic                ARB 100470.28
# 3  Diastolic               ARNI   2402.95
# 4  Diastolic       Beta_Blocker 240095.06
# 5  Diastolic     Cardiac_Device   6214.37
# 6  Diastolic           Diuretic 280646.56
# 7  Diastolic Hospital_Inpatient  24594.79
# 8  Diastolic        Injectables  18284.62
# 9  Diastolic          Inotropic  14102.01
# 10 Diastolic                MRA 371392.54
# 11 Diastolic              Other   3678.56
# 12 Diastolic              SGLT2   6910.43
# 13 Diastolic  Surgery_Inpatient   4871.43
# 14 Diastolic        Vasodilator  62457.15
# 15  Systolic                ACE 202151.74
# 16  Systolic                ARB 117996.78
# 17  Systolic               ARNI  58138.53
# 18  Systolic       Beta_Blocker 388092.25
# 19  Systolic     Cardiac_Device  25676.77
# 20  Systolic           Diuretic 308715.93
# 21  Systolic Hospital_Inpatient  28510.42
# 22  Systolic        Injectables  23477.61
# 23  Systolic          Inotropic  39462.83
# 24  Systolic                MRA 457760.45
# 25  Systolic              Other   4334.04
# 26  Systolic              SGLT2  15484.50
# 27  Systolic  Surgery_Inpatient  14103.24
# 28  Systolic        Vasodilator  52593.00




ARNi_Pats <- ARNi_Pats %>% group_by(patient, weight) %>% 
  filter(grepl(string_ARNI, Drugs)) %>% filter(Month==min(Month))
ARNi_Pats <- separate_rows(ARNi_Pats, Drugs, sep = ",", convert=T)
ARNi_Pats <- ARNi_Pats %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 
ARNi_Pats <- ARNi_Pats %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()

ARNi_Pats %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
# 1 Diastolic  16784.
# 2 Systolic  248765.
data.frame(ARNi_Pats  %>% group_by(group, drug_class) %>%
             mutate(drug_class=str_replace(drug_class, " ", "_")) %>% summarise(n=sum(weight)))

# 1  Diastolic                ACE   4577.95
# 2  Diastolic                ARB   2663.38
# 3  Diastolic               ARNI  16784.37
# 4  Diastolic       Beta_Blocker  13880.72
# 5  Diastolic     Cardiac_Device    430.89
# 6  Diastolic           Diuretic  10888.95
# 7  Diastolic Hospital_Inpatient    770.14
# 8  Diastolic        Injectables    766.00
# 9  Diastolic          Inotropic    524.47
# 10 Diastolic                MRA   3918.77
# 11 Diastolic              Other    673.53
# 12 Diastolic              SGLT2   1340.98
# 13 Diastolic  Surgery_Inpatient    249.08
# 14 Diastolic        Vasodilator   3125.65
# 15  Systolic                ACE  55393.73
# 16  Systolic                ARB  38866.47
# 17  Systolic               ARNI 248764.51
# 18  Systolic       Beta_Blocker 211719.48
# 19  Systolic     Cardiac_Device   9572.20
# 20  Systolic           Diuretic 151376.78
# 21  Systolic Hospital_Inpatient   6002.50
# 22  Systolic        Injectables   8494.14
# 23  Systolic          Inotropic  18036.18
# 24  Systolic                MRA  91774.46
# 25  Systolic              Other   3247.71
# 26  Systolic              SGLT2  12449.75
# 27  Systolic  Surgery_Inpatient   3404.41
# 28  Systolic        Vasodilator  21274.44



# -------------------------------------
# Class Penetrance 12 months before and after Surgery Hospitalization Dx Last 5y --------------------------------------------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs!="104")


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")



# HOSPITALIZATION

First_Hospital <- HF_Drug_Histories %>% filter(grepl(string_Hospital_Inpatient, Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, Month) %>% rename("First_Hospital"="Month")

First_Hospital <- First_Hospital %>% left_join(HF_Drug_Histories)

First_Hospital <- separate_rows(First_Hospital, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) 

First_Hospital <- First_Hospital %>% left_join(HF_Ingredients %>% select(-drug_group)) 

First_Hospital %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight)))  # 717938

First_Hospital <- First_Hospital %>% mutate(Lapsed=Month-First_Hospital) %>% filter((Lapsed>=(-6)) & (Lapsed<=(6)))

First_Hospital <- First_Hospital %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=13) %>%
  select(patient) %>% left_join(First_Hospital)

First_Hospital%>% filter(group=="Diastolic") %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 66729


First_Hospital %>% filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


First_Hospital %>% filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/66729) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=.8) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st Hospitalization)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))


First_Hospital %>% filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))



First_Hospital %>%  filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
    mutate(n=ifelse(drug_class=="ACE", n/26397,
                    ifelse(drug_class=="ARB", n/20519 ,
                           ifelse(drug_class=="ARNI", n/280,
                    ifelse(drug_class=="Beta Blocker", n/56390,
                           ifelse(drug_class=="MRA",n/14434,
                                  ifelse(drug_class=="SGLT2",n/1256,
                                         ifelse(drug_class=="Cardiac Device", n/5058,
                                                ifelse(drug_class=="Diuretic", n/60227 ,
                                                       ifelse(drug_class=="Heart Transplant", n/66729,
                                                              ifelse(drug_class=="Hospital Inpatient", n/66729,
                                                                     ifelse(drug_class=="Inotropic", n/4855,
                                                                            ifelse(drug_class=="Other", n/3882,
                                                                                   ifelse(drug_class=="Surgery Inpatient", n/6406,
                                                                                          ifelse(drug_class=="Vasodilator", n/17800,n/11277))))))))))))))) %>%
mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class,)) +
  geom_line(size=2, alpha=1) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st 1st Hospitalization)") +
  ylab("Population % (of Class-experienced) \n") +
    theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))
                                        


# SURGERY



First_Surgery <- HF_Drug_Histories %>% filter(grepl(string_Surgery_Inpatient, Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, Month) %>% rename("First_Surgery"="Month")

First_Surgery <- First_Surgery %>% left_join(HF_Drug_Histories)

First_Surgery <- separate_rows(First_Surgery, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) 

First_Surgery <- First_Surgery %>% left_join(HF_Ingredients %>% select(-drug_group)) 

First_Surgery %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight)))  # 330018

First_Surgery <- First_Surgery %>% mutate(Lapsed=Month-First_Surgery) %>% filter((Lapsed>=(-6)) & (Lapsed<=(6)))

First_Surgery <- First_Surgery %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=13) %>%
  select(patient) %>% left_join(First_Surgery)

First_Surgery%>% filter(group=="Diastolic") %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 35490


First_Surgery %>% filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


First_Surgery %>% filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/35490) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=.8) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st Surgery)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))


First_Surgery %>% filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))



First_Surgery %>%  filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
    mutate(n=ifelse(drug_class=="ACE", n/11357,
                    ifelse(drug_class=="ARB", n/12375 ,
                           ifelse(drug_class=="ARNI", n/573,
                    ifelse(drug_class=="Beta Blocker", n/29811,
                           ifelse(drug_class=="MRA",n/6086,
                                  ifelse(drug_class=="SGLT2",n/789,
                                         ifelse(drug_class=="Cardiac Device", n/9446,
                                                ifelse(drug_class=="Diuretic", n/31023 ,
                                                       ifelse(drug_class=="Heart Transplant", n/35490,
                                                              ifelse(drug_class=="Hospital Inpatient", n/12365,
                                                                     ifelse(drug_class=="Inotropic", n/3056,
                                                                            ifelse(drug_class=="Other", n/2396,
                                                                                   ifelse(drug_class=="Surgery Inpatient", n/35490,
                                                                                          ifelse(drug_class=="Vasodilator", n/9032,n/5161))))))))))))))) %>%
mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class,)) +
  geom_line(size=2, alpha=1) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st 1st Surgery)") +
  ylab("Population % (of Class-experienced) \n") +
    theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))
                                        

# -----------------------------
# Class Penetrance 12 months before and after 1st Dx Last 5y --------------------------------------------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) 
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs!="104")

names(HF_Drug_Histories)[4] <- "First_HF_Dx"


HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) 

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients %>% select(-drug_group)) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 


HF_Drug_Histories <- HF_Drug_Histories %>% 
  mutate(Lapsed=Month-First_HF_Dx) %>% filter((Lapsed<=(12)))

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=13) %>%
  select(patient) %>% left_join(HF_Drug_Histories)

HF_Drug_Histories %>%  filter(group=="Systolic") %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 744702


HF_Drug_Histories %>%  filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


HF_Drug_Histories  %>%  filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/744702) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=.8) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))


HF_Drug_Histories %>%  filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))



HF_Drug_Histories %>% filter(group=="Diastolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
    mutate(n=ifelse(drug_class=="ACE", n/494294,
                    ifelse(drug_class=="ARB", n/440675,
                           ifelse(drug_class=="ARNI", n/3924,
                    ifelse(drug_class=="Beta Blocker", n/929925,
                           ifelse(drug_class=="MRA",n/157302,
                                  ifelse(drug_class=="SGLT2",n/36713,
                                         ifelse(drug_class=="Cardiac Device", n/102514,
                                                ifelse(drug_class=="Diuretic", n/949576 ,
                                                       ifelse(drug_class=="Heart Transplant", n/175,
                                                              ifelse(drug_class=="Hospital Inpatient", n/168552,
                                                                     ifelse(drug_class=="Inotropic", n/62382,
                                                                            ifelse(drug_class=="Other", n/50702,
                                                                                   ifelse(drug_class=="Surgery Inpatient", n/61425,
                                                                                          ifelse(drug_class=="Vasodilator", n/254382,n/113906))))))))))))))) %>%
mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class,)) +
  geom_line(size=2, alpha=1) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % (of Class-experienced) \n") +
    theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))
                                        
# ------------------------------------------
# Months to Class Individual Last 5y ------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) 


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")


HF_Drug_Histories <- HF_Drug_Histories %>% filter(group=="Systolic") 

# Time to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 3.32
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first  Beta blcoker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 3.44
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 4.12
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight))


# Time to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% group_by(patient, weight) %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 18.3
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)) 
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 13.9
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight))
  
# Time to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 9.90
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 4.97
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight))

# Time to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 9.00
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 



# Time to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 8.38
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Time to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 

HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) # 13.3
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Time to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 9.91
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Time to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 8.46
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Time to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 9.32
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Time to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% group_by(patient, weight) %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))  # 8.15
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight))






# ------------------------------------------
# Lines to Class Individual  Last 5y ------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) 


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")


HF_Drug_Histories <- HF_Drug_Histories %>% filter(group=="Systolic") 


# Lines to first  Diuretic
HF_Drug_Histories_Diuretic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Diuretic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Diuretic,Drugs)) else NA) 

HF_Drug_Histories_Diuretic <- HF_Drug_Histories_Diuretic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Diuretic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Diuretic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Lines to first  Beta blcker
HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_BetaBlocker,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_BetaBlocker,Drugs)) else NA) 

HF_Drug_Histories_BetaBlocker <- HF_Drug_Histories_BetaBlocker %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_BetaBlocker %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 
# Lines to first  ACE
HF_Drug_Histories_ACE <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ACE,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ACE, Drugs)) else NA) 

HF_Drug_Histories_ACE <- HF_Drug_Histories_ACE %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ACE %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_ACE %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Lines to first  ARNi
HF_Drug_Histories_ARNi <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARNI,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)) else NA) 

HF_Drug_Histories_ARNi <- HF_Drug_Histories_ARNi %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
data.frame(HF_Drug_Histories_ARNi %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight)))
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_ARNi %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Lines to first  SGLT2
HF_Drug_Histories_SGLT2 <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_SGLT2,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_SGLT2,Drugs)) else NA) 

HF_Drug_Histories_SGLT2 <- HF_Drug_Histories_SGLT2 %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_SGLT2 %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(mean=weighted.mean(n, weight))
HF_Drug_Histories_SGLT2 %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 
  
# Lines to first  MRA
HF_Drug_Histories_MRA <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_MRA,Drugs)) else NA) 

HF_Drug_Histories_MRA <- HF_Drug_Histories_MRA %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_MRA %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_MRA %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 


# Lines to first ARB
HF_Drug_Histories_ARB <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_ARB,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARB,Drugs)) else NA) 

HF_Drug_Histories_ARB <- HF_Drug_Histories_ARB %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_ARB %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_ARB %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Lines to first Inotropic
HF_Drug_Histories_Inotropic <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Inotropic,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Inotropic,Drugs)) else NA) 

HF_Drug_Histories_Inotropic <- HF_Drug_Histories_Inotropic %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Inotropic %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Inotropic %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 



# Lines to first Vasodilator
HF_Drug_Histories_Vasodilator <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Vasodilator,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Vasodilator,Drugs)) else NA) 

HF_Drug_Histories_Vasodilator <- HF_Drug_Histories_Vasodilator %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Vasodilator %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Vasodilator %>% ungroup() %>% summarise(median=weighted.median(n, weight))


# Lines to first Other
HF_Drug_Histories_Other <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Other,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Other,Drugs)) else NA) 


HF_Drug_Histories_Other <- HF_Drug_Histories_Other %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Other %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Other %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Other %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Lines to first Cardiac Device
HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Cardiac_Device,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Cardiac_Device,Drugs)) else NA) 

HF_Drug_Histories_CardiacDevice <- HF_Drug_Histories_CardiacDevice %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_CardiacDevice %>% ungroup() %>% summarise(median=weighted.median(n, weight))

# Lines to first Hospital_Inpatient
HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Hospital_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Hospital_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Hospital_Inpatient <- HF_Drug_Histories_Hospital_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Hospital_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Lines to first Surgery_Inpatient
HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Surgery_Inpatient,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Surgery_Inpatient,Drugs)) else NA) 

HF_Drug_Histories_Surgery_Inpatient <- HF_Drug_Histories_Surgery_Inpatient %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Surgery_Inpatient %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# Lines to first Heart_Transplant
HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl(string_Heart_Transplant,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_Heart_Transplant,Drugs)) else NA) 

HF_Drug_Histories_Heart_Transplant <- HF_Drug_Histories_Heart_Transplant %>% select(-c(Month)) %>% filter(Drugs!="-") %>% group_by(patient, weight) %>% distinct() %>% count() %>% arrange(-n)
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(weight))
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(mean=weighted.mean(n, weight)) 
HF_Drug_Histories_Heart_Transplant %>% ungroup() %>% summarise(median=weighted.median(n, weight)) 

# -------------------
# Plot it Individual  Last 5y --------------------------------------------------------------

Months_vs_Lines_to_class <- fread("Months_vs_Lines_to_class_individual_Diastolic_vs_Systolic_L5y.txt")

library(ggrepel)
library(hrbrthemes)
library(viridis)

Months_vs_Lines_to_class <- Months_vs_Lines_to_class %>% filter(drug_class != "Cardiac_Device" & 
                                      drug_class != "Hospital_Inpatient" & 
                                      drug_class != "Surgery_Inpatient" & 
                                      drug_class != "Heart_Transplant" )

Months_vs_Lines_to_class %>%
ggplot(aes(x=average_months_to_class, y=average_lines_to_class,  
                                     fill=Group, colour=Group)) +
  geom_point(alpha=1, size=5, show.legend = F)+
  geom_text_repel(aes(label = drug_class, colour = Group), 
                  size = 4,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  theme(legend.position = "none",
        #panel.background = element_blank(),
        #panel.grid = element_blank(),
        #axis.ticks = element_blank(),
        text = element_text(size = 10))+
  theme_minimal() +
  xlim(0,20) +
  scale_colour_manual(values=c("#00B0F0", "#F72E19")) +
  xlab("\nAverage Number of Months to Class Initiation")+
  ylab("Average Number of Therapy Lines to Class Initiation\n")
# ------------------------
# Drug Usage Last 12 months - NYHA Stages - Dx Last 5y -----------------------------------------------------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

New_Pats <- HF_Drug_Histories %>% select(patient, group, weight)

Stages <- fread("Predicted_Stages_gbm_All.txt", colClasses = "character", sep="\t")


New_Pats <- Stages %>% inner_join(New_Pats)

sum(as.numeric(New_Pats$weight)) 

New_Pats %>% group_by(group, Predicted.Stage) %>% summarise(n=sum(as.numeric(weight)))

# 1 Diastolic 1                322824.
# 2 Diastolic 2               1238141 
# 3 Diastolic 3                887841.
# 4 Diastolic 4                179873.
# 5 Systolic  1                249297.
# 6 Systolic  2                835347.
# 7 Systolic  3                521055.
# 8 Systolic  4                100865.

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(Stages)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, group, Predicted.Stage, Exact_Month, month49:month60)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) 
HF_Drug_Histories <- HF_Drug_Histories %>%  select(-Exact_Month)

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, group, Drugs, Predicted.Stage) %>% distinct() %>% left_join(HF_Ingredients) 

data.frame(HF_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class, Predicted.Stage) %>% distinct() %>%
  group_by(group, Predicted.Stage, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"
CKD <- HF_Comorbidity_Inventories %>% filter(diagnosis=="N18") %>% select(patient) %>% distinct() 
CKD$CKD <- "CKD"

New_Pats %>% left_join(CKD) %>% group_by(group, CKD, Predicted.Stage) %>% summarise(n=sum(as.numeric(weight)))


data.frame(HF_Drug_Histories %>% left_join(CKD) %>%
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, CKD, drug_class) %>% distinct() %>%
  group_by(group, CKD, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

# ---------------
# Class Penetrance 12 months before and after SGLT2/MRA/ARNI Dx Last 5y --------------------------------------------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs!="104")


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")



# ARNI

First_SGLT2 <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, Month) %>% rename("First_SGLT2"="Month")

First_SGLT2 <- First_SGLT2 %>% left_join(HF_Drug_Histories)

First_SGLT2 <- separate_rows(First_SGLT2, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) 

First_SGLT2 <- First_SGLT2 %>% left_join(HF_Ingredients %>% select(-drug_group)) 

First_SGLT2 %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight)))  # 717938

First_SGLT2 <- First_SGLT2 %>% mutate(Lapsed=Month-First_SGLT2) %>% filter((Lapsed>=(-6)) & (Lapsed<=(6)))

First_SGLT2 <- First_SGLT2 %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=13) %>%
  select(patient) %>% left_join(First_SGLT2)

First_SGLT2%>% filter(group=="Systolic") %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 30362

First_SGLT2 %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)

First_SGLT2 %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/30362) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=.8) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st MRA)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))

First_SGLT2 %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class) %>% distinct()  %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))

First_SGLT2 %>%  filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
    mutate(n=ifelse(drug_class=="ACE", n/13101,
                    ifelse(drug_class=="ARB", n/10631 ,
                           ifelse(drug_class=="ARNI", n/8459,
                    ifelse(drug_class=="Beta Blocker", n/28368,
                           ifelse(drug_class=="MRA",n/11907,
                                  ifelse(drug_class=="SGLT2",n/30362,
                                         ifelse(drug_class=="Cardiac Device", n/4080,
                                                ifelse(drug_class=="Diuretic", n/22737 ,
                                                       ifelse(drug_class=="Heart Transplant", n/30362,
                                                              ifelse(drug_class=="Hospital Inpatient", n/1650,
                                                                     ifelse(drug_class=="Inotropic", n/3643,
                                                                            ifelse(drug_class=="Other", n/887,
                                                                                   ifelse(drug_class=="Surgery Inpatient", n/935,
                                                                                          ifelse(drug_class=="Vasodilator", n/5569,n/1487))))))))))))))) %>%
mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class,)) +
  geom_line(size=2, alpha=1) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st 1st SGLT2)") +
  ylab("Population % (of Class-experienced) \n") +
    theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))
                                        

# -----------------------------
# % patients that had already seen that physician upon class initiation ------------------

HF_Doses <- fread("HF Doses.txt")

HF_Doses <- HF_Doses %>% select(drug_class, pat_id, weight, from_dt, prov) %>% mutate(from_dt=as.Date(from_dt))
HF_Doses <- HF_Doses %>% filter(drug_class=="ARNI") %>% select(pat_id) %>% distinct() %>% left_join(HF_Doses)

HF_Doses <- HF_Doses %>% arrange(pat_id, weight,from_dt, desc(drug_class))

HF_Doses <- HF_Doses %>% group_by(pat_id, weight) %>% 
  slice(if(any(grepl("ARNI",drug_class))) which.max(!grepl("-",drug_class)):which.max(grepl("ARNI",drug_class)) else NA) 


FirstARNI <- HF_Doses %>% filter(drug_class=="ARNI") %>% select(pat_id, weight, prov) %>% rename("FirstARNI"="prov") %>% ungroup()


HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)


HF_Doses %>% 
  anti_join(HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)) %>%
  left_join(FirstARNI) %>% ungroup() %>% filter(drug_class!="ARNI") %>% select(-from_dt) %>% distinct() %>%
  mutate(SeenBefore=ifelse(prov==FirstARNI,1,0)) %>%
  select(pat_id, weight, SeenBefore) %>% distinct() %>%
  #select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))  %>% # 729659
  filter(SeenBefore==1) %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 454357

# 0.6226977 had already had scripts from that same physician



HF_Doses <- fread("HF Doses.txt")

HF_Doses <- HF_Doses %>% select(drug_class, pat_id, weight, from_dt, prov) %>% mutate(from_dt=as.Date(from_dt))
HF_Doses <- HF_Doses %>% filter(drug_class=="MRA") %>% select(pat_id) %>% distinct() %>% left_join(HF_Doses)

HF_Doses <- HF_Doses %>% arrange(pat_id, weight,from_dt, desc(drug_class))

HF_Doses <- HF_Doses %>% group_by(pat_id, weight) %>% 
  slice(if(any(grepl("MRA",drug_class))) which.max(!grepl("-",drug_class)):which.max(grepl("MRA",drug_class)) else NA) 

FirstMRA <- HF_Doses %>% filter(drug_class=="MRA") %>% select(pat_id, weight, prov) %>% rename("FirstMRA"="prov") %>% ungroup()

HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)


HF_Doses %>% 
    anti_join(HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)) %>%
  left_join(FirstMRA) %>% ungroup() %>% filter(drug_class!="MRA") %>% select(-from_dt) %>% distinct() %>%
  mutate(SeenBefore=ifelse(prov==FirstMRA,1,0)) %>%
  select(pat_id, weight, SeenBefore) %>% distinct() %>%
 # select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))  %>% # 2429788
  filter(SeenBefore==1) %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1285261

# 0.5289601 had already had scripts from that same physician





HF_Doses <- fread("HF Doses.txt")

HF_Doses <- HF_Doses %>% select(drug_class, pat_id, weight, from_dt, prov) %>% mutate(from_dt=as.Date(from_dt))
HF_Doses <- HF_Doses %>% filter(drug_class=="SGLT2") %>% select(pat_id) %>% distinct() %>% left_join(HF_Doses)

HF_Doses <- HF_Doses %>% arrange(pat_id, weight,from_dt, desc(drug_class))

HF_Doses <- HF_Doses %>% group_by(pat_id, weight) %>% 
  slice(if(any(grepl("SGLT2",drug_class))) which.max(!grepl("-",drug_class)):which.max(grepl("SGLT2",drug_class)) else NA) 

FirstSGLT2 <- HF_Doses %>% filter(drug_class=="SGLT2") %>% select(pat_id, weight, prov) %>% rename("FirstSGLT2"="prov") %>% ungroup()

HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)


HF_Doses %>% 
  anti_join(HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)) %>%
  left_join(FirstSGLT2) %>% ungroup() %>% filter(drug_class!="SGLT2") %>% select(-from_dt) %>% distinct() %>%
  mutate(SeenBefore=ifelse(prov==FirstSGLT2,1,0)) %>%
  select(pat_id, weight, SeenBefore) %>% distinct() %>%
  #select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))  %>% # 777778
  filter(SeenBefore==1) %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 462771

# 0.5949911 had already had scripts from that same physician








HF_Doses <- fread("HF Doses.txt")

HF_Doses <- HF_Doses %>% select(drug_class, pat_id, weight, from_dt, prov) %>% mutate(from_dt=as.Date(from_dt))
HF_Doses <- HF_Doses %>% filter(drug_class=="Inotropic") %>% select(pat_id) %>% distinct() %>% left_join(HF_Doses)

HF_Doses <- HF_Doses %>% arrange(pat_id, weight,from_dt, desc(drug_class))

HF_Doses <- HF_Doses %>% group_by(pat_id, weight) %>% 
  slice(if(any(grepl("Inotropic",drug_class))) which.max(!grepl("-",drug_class)):which.max(grepl("Inotropic",drug_class)) else NA) 

FirstInotropic <- HF_Doses %>% filter(drug_class=="Inotropic") %>% select(pat_id, weight, prov) %>% rename("FirstInotropic"="prov") %>% ungroup()

HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)


HF_Doses %>% 
  anti_join(HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)) %>%
  left_join(FirstInotropic) %>% ungroup() %>% filter(drug_class!="Inotropic") %>% select(-from_dt) %>% distinct() %>%
  mutate(SeenBefore=ifelse(prov==FirstInotropic,1,0)) %>%
  select(pat_id, weight, SeenBefore) %>% distinct() %>%
  #select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))  %>% # 1096621
  filter(SeenBefore==1) %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 461952

# 0.4212504 had already had scripts from that same physician



HF_Doses <- fread("HF Doses.txt")

HF_Doses <- HF_Doses %>% select(drug_class, pat_id, weight, from_dt, prov) %>% mutate(from_dt=as.Date(from_dt))
HF_Doses <- HF_Doses %>% filter(drug_class=="ACE") %>% select(pat_id) %>% distinct() %>% left_join(HF_Doses)

HF_Doses <- HF_Doses %>% arrange(pat_id, weight,from_dt, desc(drug_class))

HF_Doses <- HF_Doses %>% group_by(pat_id, weight) %>% 
  slice(if(any(grepl("ACE",drug_class))) which.max(!grepl("-",drug_class)):which.max(grepl("ACE",drug_class)) else NA) 

FirstACE <- HF_Doses %>% filter(drug_class=="ACE") %>% select(pat_id, weight, prov) %>% rename("FirstACE"="prov") %>% ungroup()

HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)


HF_Doses %>% 
  anti_join(HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)) %>%
  left_join(FirstACE) %>% ungroup() %>% filter(drug_class!="ACE") %>% select(-from_dt) %>% distinct() %>%
  mutate(SeenBefore=ifelse(prov==FirstACE,1,0)) %>%
  select(pat_id, weight, SeenBefore) %>% distinct() %>%
  #select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))  %>% # 3687952
  filter(SeenBefore==1) %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1925361

# 0.5220678 had already had scripts from that same physician





HF_Doses <- fread("HF Doses.txt")

HF_Doses <- HF_Doses %>% select(drug_class, pat_id, weight, from_dt, prov) %>% mutate(from_dt=as.Date(from_dt))
HF_Doses <- HF_Doses %>% filter(drug_class=="ARB") %>% select(pat_id) %>% distinct() %>% left_join(HF_Doses)

HF_Doses <- HF_Doses %>% arrange(pat_id, weight,from_dt, desc(drug_class))

HF_Doses <- HF_Doses %>% group_by(pat_id, weight) %>% 
  slice(if(any(grepl("ARB",drug_class))) which.max(!grepl("-",drug_class)):which.max(grepl("ARB",drug_class)) else NA) 

FirstARB <- HF_Doses %>% filter(drug_class=="ARB") %>% select(pat_id, weight, prov) %>% rename("FirstARB"="prov") %>% ungroup()

HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)


HF_Doses %>% 
  anti_join(HF_Doses %>% ungroup() %>% select(pat_id, from_dt) %>% group_by(pat_id) %>% filter(from_dt==max(from_dt)) %>% slice(1)) %>%
  left_join(FirstARB) %>% ungroup() %>% filter(drug_class!="ARB") %>% select(-from_dt) %>% distinct() %>%
  mutate(SeenBefore=ifelse(prov==FirstARB,1,0)) %>%
  select(pat_id, weight, SeenBefore) %>% distinct() %>%
  #select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight))  %>% # 3259619
  filter(SeenBefore==1) %>% select(pat_id, weight) %>% distinct() %>% summarise(n=sum(weight)) # 1962351

# 0.6020185 had already had scripts from that same physician


# ---------------------------------------
# Profiles of ARNi, MRA, SGLT2 ----------------------------------------------------------------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% select(-Exact_Month) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs != "104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, generic_name)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

unique(HF_Drug_Histories$generic_name)

HF_Drug_Histories$generic_name <- str_replace_all(HF_Drug_Histories$generic_name, " ", "_")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 4335241

temp_short <- HF_Drug_Histories %>% 
  select(patient, weight, group, generic_name) %>% distinct() %>%
     mutate(Treat=1) %>% select(-weight) %>%
  spread(key=generic_name, value=Treat)

temp_short[is.na(temp_short)] <- 0

HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- temp_short %>% select(patient) %>% left_join(HF_Comorbidity_Inventories) %>% select(patient, diagnosis)
HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% filter(grepl("C", diagnosis)|grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|
                                        grepl("H", diagnosis)|grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|
                                        grepl("M", diagnosis)|grepl("N", diagnosis)|grepl("R", diagnosis))

HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% mutate(Treat=1) %>%
  spread(key=diagnosis, value=Treat)

HF_Comorbidity_Inventories[is.na(HF_Comorbidity_Inventories)] <- 0

temp_short <- temp_short %>% left_join(HF_Comorbidity_Inventories)
temp_short <- temp_short %>% mutate(group=ifelse(group=="Systolic",1,0))

temp_short <- temp_short %>% mutate(SGLT2_exp = ifelse(Canagliflozin==1|Dapagliflozin==1|Empagliflozin==1|Ertugliflozin,1,0)) %>% select(-c(Canagliflozin, Dapagliflozin, Empagliflozin, Ertugliflozin))

temp_short_nopat <- temp_short[,2:1077]

#names(temp_short_nopat) <- paste0("A",names(temp_short_nopat))



modelAll_1_gbm <- gbm(SGLT2_exp ~ . , data = temp_short_nopat)
summary(modelAll_1_gbm)










# Per Class
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% select(-Exact_Month) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs != "104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>% select(-drug_group)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, Drugs) %>% distinct() %>% left_join(HF_Ingredients) 

unique(HF_Drug_Histories$drug_class)

HF_Drug_Histories$drug_class <- str_replace_all(HF_Drug_Histories$generic_name, " ", "_")

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 4335241

temp_short <- HF_Drug_Histories %>% 
  select(patient, weight, group, drug_class) %>% distinct() %>%
     mutate(Treat=1) %>% select(-weight) %>%
  spread(key=drug_class, value=Treat)

temp_short[is.na(temp_short)] <- 0

HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- temp_short %>% select(patient) %>% left_join(HF_Comorbidity_Inventories) %>% select(patient, diagnosis)
HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% filter(grepl("C", diagnosis)|grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|
                                        grepl("H", diagnosis)|grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|
                                        grepl("M", diagnosis)|grepl("N", diagnosis)|grepl("R", diagnosis))

HF_Comorbidity_Inventories <- HF_Comorbidity_Inventories %>% mutate(Treat=1) %>%
  spread(key=diagnosis, value=Treat)

HF_Comorbidity_Inventories[is.na(HF_Comorbidity_Inventories)] <- 0

temp_short <- temp_short %>% left_join(HF_Comorbidity_Inventories)
temp_short <- temp_short %>% mutate(group=ifelse(group=="Systolic",1,0))

temp_short_nopat <- temp_short[,2:1015]

modelAll_1_gbm <- gbm(SGLT2 ~ . , data = temp_short_nopat)
summary(modelAll_1_gbm)

# ------------------------------
# Whats comes first ARNi or MRA ? -----------------------------------------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% select(-Exact_Month) %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs != "104")
HF_Drug_Histories <-  HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient, weight) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(grepl(string_MRA, Drugs)) %>% select(patient,weight) %>% distinct()
ARNI_pats <- HF_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)) %>% select(patient,weight) %>% distinct()


SGLT2_Pats %>% summarise(n=sum(weight)) # 269041.3
MRA_Pats %>% anti_join(ARNI_pats) %>% inner_join(SGLT2_Pats) %>% summarise(n=sum(weight)) # 153432.5

ARNI_pats %>% inner_join(MRA_Pats) %>%  left_join(HF_Drug_Histories) %>%
  group_by(patient, weight) %>%
  slice(if(any(grepl(string_ARNI,Drugs)|grepl(string_MRA,Drugs))) which.max(!grepl("-",Drugs)):which.max(grepl(string_ARNI,Drugs)|grepl(string_MRA,Drugs)) else NA) %>%
  filter(Month==max(Month)) %>% filter(grepl(string_MRA,Drugs)) %>% ungroup() %>% summarise(n=sum(weight))


# ----------------------------------
# Lines of therapy ACEs/ARBs/MRA/ARNi/Diur/Beta Block-----------------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(HF_Drug_Histories)


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(Drugs=ifelse(Month<Exact_Month,"-",Drugs))

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 5726651 treat-experienced

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

unique(HF_Ingredients$drug_class)

HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="ACE"|drug_class=="ARB"|drug_class=="MRA"|drug_class=="ARNI"|drug_class=="Beta Blocker"|drug_class=="SGLT2"|drug_class=="Diuretic")

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
unique(HF_Ingredients$drug_class)

string_Short_Drugs        <- paste0("\\b(",paste0(HF_Ingredients$molecule, collapse = "|"),")\\b")

HF_Drug_Histories <- HF_Drug_Histories %>% filter(grepl(string_Short_Drugs, Drugs))

HF_Drug_Histories <- HF_Drug_Histories %>% arrange(patient, weight, Month, Drugs) %>%
  group_by(patient, weight, Month) %>% mutate(treat_new = paste(Drugs, collapse=",")) 

HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(patient, weight, group, Month,  treat_new) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% spread(key=Month, value=treat_new)

HF_Drug_Histories[is.na(HF_Drug_Histories)] <- "-"

names(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, `1`:`60`, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Drug_Histories %>% 
  select(patient, weight, group, Drugs) %>% distinct() %>%
  group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  mutate(n=ifelse(n>=10,10,n)) %>%
  group_by(group,n) %>% summarise(total=sum(as.numeric(weight)))

HF_Drug_Histories %>% 
  select(patient, weight, group, Drugs) %>% distinct() %>%
  group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  group_by(group) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))



HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(Month==60) %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(Month==60) %>% filter(grepl(string_MRA, Drugs)) %>% select(patient) %>% distinct()
ARNI_Pats <- HF_Drug_Histories %>% filter(Month==60) %>% filter(grepl(string_ARNI, Drugs)) %>% select(patient) %>% distinct()



SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight)) 

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=10,10,n)) %>% group_by(group, n) %>% summarise(lines=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=lines))


data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))



ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight)) 

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=10,10,n)) %>% group_by(group, n) %>% summarise(lines=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=lines))



data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))


MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight)) 

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=10,10,n)) %>% group_by(group, n) %>% summarise(lines=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=lines))



data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, Drugs) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()




data.frame(HF_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))



SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))


data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))


ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))


data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))



data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))




MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))


data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))
# ----------------------------------
# Drug penetrance per comorbidity ----------------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) # 13359838
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Drugs) %>% distinct()

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(grepl(string_MRA, Drugs)) %>% select(patient) %>% distinct()
ARNi_Pats <- HF_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)) %>% select(patient) %>% distinct()


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(HF_Comorbidity_Inventories %>% select(-weight))

HF_Comorbidity_Inventories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 

Groups_Diastolic_vs_Systolic_L5Y %>% left_join(
  HF_Comorbidity_Inventories %>% filter(diagnosis=="N18") %>% select(patient) %>% distinct() %>% 
  mutate(CKD="CKD"))  %>%
  left_join(ARNi_Pats %>% mutate(ARNI="ARNI")) %>%
  group_by(group, CKD, ARNI) %>% summarise(n=sum(as.numeric(weight)))
   
1 Diastolic CKD   ARNI    10077.
2 Diastolic CKD   NA    1347286.
3 Diastolic NA    ARNI     6971.
4 Diastolic NA    NA    1264344.
5 Systolic  CKD   ARNI   100534.
6 Systolic  CKD   NA     667676.
7 Systolic  NA    ARNI   151460.
8 Systolic  NA    NA     786893.

Groups_Diastolic_vs_Systolic_L5Y %>% left_join(
  HF_Comorbidity_Inventories %>% filter(diagnosis=="I70"|diagnosis=="I73") %>% select(patient) %>% distinct() %>% 
  mutate(PAD="PAD"))  %>%
  left_join(ARNi_Pats %>% mutate(ARNI="ARNI")) %>%
  group_by(group, PAD, ARNI) %>% summarise(n=sum(as.numeric(weight)))
   
1 Diastolic PAD   ARNI    10046.
2 Diastolic PAD   NA    1412167.
3 Diastolic NA    ARNI     7002.
4 Diastolic NA    NA    1199463.
5 Systolic  PAD   ARNI   102391.
6 Systolic  PAD   NA     662115.
7 Systolic  NA    ARNI   149603.
8 Systolic  NA    NA     792454.

Groups_Diastolic_vs_Systolic_L5Y %>% left_join(
  HF_Comorbidity_Inventories %>% filter(diagnosis=="K76") %>% select(patient) %>% distinct() %>% 
  mutate(NAFLD="NAFLD"))  %>%
  left_join(ARNi_Pats %>% mutate(ARNI="ARNI")) %>%
  group_by(group, NAFLD, ARNI) %>% summarise(n=sum(as.numeric(weight)))

1 Diastolic NAFLD ARNI     4155.
2 Diastolic NAFLD NA     554473.
3 Diastolic NA    ARNI    12892.
4 Diastolic NA    NA    2057158.
5 Systolic  NAFLD ARNI    49932.
6 Systolic  NAFLD NA     265085.
7 Systolic  NA    ARNI   202062.
8 Systolic  NA    NA    1189485.

DANU_Demographics <- fread("DANU Demographics Full.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"

Groups_Diastolic_vs_Systolic_L5Y %>%
  left_join(
DANU_Demographics %>% filter(grepl("Diabetes", diagnosis)) %>% select(patient) %>% inner_join(HF_Comorbidity_Inventories) %>% mutate(Diabetes="Diabetes")
) %>%
  select(patient, weight, group, Diabetes) %>% distinct() %>% 
  left_join(ARNi_Pats %>% mutate(ARNI="ARNI")) %>%
  group_by(group, Diabetes, ARNI) %>% summarise(n=sum(as.numeric(weight)))

1 Diastolic Diabetes ARNI    11704.
2 Diastolic Diabetes NA    1566695.
3 Diastolic NA       ARNI     5344.
4 Diastolic NA       NA    1044935.
5 Systolic  Diabetes ARNI   139009.
6 Systolic  Diabetes NA     807618.
7 Systolic  NA       ARNI   112984.
8 Systolic  NA       NA     646952.

DANU_Events <- fread("DANU Events Full.txt")
DANU_Events <- DANU_Events %>% filter(grepl("BMI", code ))
DANU_Events$code <- parse_number(DANU_Events$code)
DANU_Events <- DANU_Events %>% group_by(patid) %>% filter(code == max(code)) %>% slice(1)
DANU_Events <- DANU_Events %>% filter(code>=30) %>% select(patid) %>% distinct()
names(DANU_Events)[1] <- "patient"

Groups_Diastolic_vs_Systolic_L5Y %>%
  left_join(
    DANU_Events %>% 
  inner_join(HF_Comorbidity_Inventories) %>% ungroup() %>%
  select(patient, group) %>% distinct() %>% mutate(Obesity="Obesity")
  ) %>% 
  left_join(ARNi_Pats %>% mutate(ARNI="ARNI")) %>%
  group_by(group, Obesity, ARNI) %>% summarise(n=sum(as.numeric(weight)))

1 Diastolic Obesity ARNI     6897.
2 Diastolic Obesity NA    1094802.
3 Diastolic NA      ARNI    10150.
4 Diastolic NA      NA    1516829.
5 Systolic  Obesity ARNI    97646.
6 Systolic  Obesity NA     530398.
7 Systolic  NA      ARNI   154348.
8 Systolic  NA      NA     924171.



Predicted_Stages_gbm_ALL <- fread("Predicted_Stages_gbm_ALL.txt")
Predicted_Stages_gbm_ALL <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(Predicted_Stages_gbm_ALL)

Predicted_Stages_gbm_ALL  %>% left_join(SGLT2_Pats %>% mutate(SGLT2="SGLT2")) %>%
  group_by(group, Predicted.Stage, SGLT2) %>% summarise(n=sum(as.numeric(weight)))

1 Diastolic               1 SGLT2   16135.
 2 Diastolic               1 NA     306689.
 3 Diastolic               2 SGLT2   65218.
 4 Diastolic               2 NA    1172923.
 5 Diastolic               3 SGLT2   44155.
 6 Diastolic               3 NA     843685.
 7 Diastolic               4 SGLT2   10708.
 8 Diastolic               4 NA     169164.
 9 Systolic                1 SGLT2   13124.
10 Systolic                1 NA     236172.
11 Systolic                2 SGLT2   64172.
12 Systolic                2 NA     771175.
13 Systolic                3 SGLT2   42031.
14 Systolic                3 NA     479025.
15 Systolic                4 SGLT2    9246.
16 Systolic                4 NA      91619.

# ----------------------------------
# Lines of therapy Beta Block|RAAS|MRA|SGLT2 only -----------------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(HF_Drug_Histories)


HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(Drugs=ifelse(Month<Exact_Month,"-",Drugs))

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 4335241 treat-experienced


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(Month==60) %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct()
MRA_Pats <- HF_Drug_Histories %>% filter(Month==60) %>% filter(grepl(string_MRA, Drugs)) %>% select(patient) %>% distinct()
ARNI_Pats <- HF_Drug_Histories %>% filter(Month==60) %>% filter(grepl(string_ARNI, Drugs)) %>% select(patient) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs != "104" & Drugs != "-")

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% mutate(Drugs=as.numeric(Drugs)) %>% left_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, group, drug_class) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% filter(drug_class=="Beta Blocker"|drug_class=="ACE"|drug_class=="ARB"|drug_class=="ARNI"|drug_class=="MRA")
HF_Drug_Histories <- HF_Drug_Histories %>%  mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB"|drug_class=="ARNI", "RAAS", drug_class)) 

data.frame(HF_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

#       group    lines
# 1 Diastolic 1.748532
# 2  Systolic 2.124983

data.frame(HF_Drug_Histories %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

#   n  Diastolic  Systolic
# 1 1  871065.18 311966.28
# 2 2 1097342.14 781857.36
# 3 3  269585.64 409707.62
# 4 4   17187.24  48092.66


SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))

#   group          n
#   <chr>      <dbl>
# 1 Diastolic 55222.
# 2 Systolic  61746.

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

#   n Diastolic Systolic
# 1 1   3206.82  1368.03
# 2 2  11720.22  4452.79
# 3 3  31143.81 27688.93
# 4 4   9151.41 28236.19


data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

#       group   lines
# 1 Diastolic 2.83734
# 2  Systolic 3.34087

ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
#   group           n
#   <chr>       <dbl>
# 1 Diastolic   8390.
# 2 Systolic  146421.

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

#   n Diastolic Systolic
# 1 1    223.49  1963.86
# 2 2   4302.76 54674.34
# 3 3   3382.73 71256.11
# 4 4    481.52 18526.64

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

#       group    lines
# 1 Diastolic 2.491303
# 2  Systolic 2.726300


MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))

# 1 Diastolic 153829.
# 2 Systolic  214514

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

#   n Diastolic  Systolic
# 1 1  13871.11   2912.28
# 2 2  47852.00  16324.85
# 3 3  83715.36 166712.39
# 4 4   8390.13  28564.35

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, drug_class) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

#       group    lines
# 1 Diastolic 2.563124
# 2  Systolic 3.029905








HF_Drug_Histories <- fread("HF Drug Histories.txt")

HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(HF_Drug_Histories)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(Drugs=ifelse(Month<Exact_Month,"-",Drugs))

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs != "104" & Drugs != "-")

HF_Drug_Histories <- HF_Drug_Histories %>%  mutate(Drugs=as.numeric(Drugs)) %>% left_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class))

HF_Drug_Histories <- HF_Drug_Histories %>% filter(drug_class=="Beta Blocker"|drug_class=="ACE"|drug_class=="ARB"|drug_class=="ARNI"|drug_class=="MRA"|drug_class=="SGLT2")
HF_Drug_Histories <- HF_Drug_Histories %>%  mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB"|drug_class=="ARNI", "RAAS", drug_class)) 
HF_Drug_Histories <- HF_Drug_Histories %>%  select(-drug_group)
HF_Drug_Histories <- HF_Drug_Histories %>%  select(-Exact_Month)

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, Month, drug_class) %>% distinct() %>% arrange(patient, group, weight, Month, drug_class)

HF_Drug_Histories <- HF_Drug_Histories %>% 
  group_by(patient, group, weight, Month) %>% mutate(treat_new = paste(drug_class  , collapse=",")) 

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, Month, treat_new) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% ungroup() %>% select(patient, group, weight, treat_new) %>% distinct()




data.frame(HF_Drug_Histories %>% select(patient, weight, group, treat_new) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

#       group    lines
# 1 Diastolic 1.796272
# 2  Systolic 2.153227

data.frame(HF_Drug_Histories %>% select(patient, weight, group, treat_new) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

#   n  Diastolic  Systolic
# 1 1 1117032.65 549315.51
# 2 2  672228.25 512784.06
# 3 3  343739.20 307657.83
# 4 4   76573.28 104734.07
# 5 5   45606.82  77132.45


SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))

#   group          n
#   <chr>      <dbl>
# 1 Diastolic 55222.
# 2 Systolic  61746.

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, treat_new) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

#   n Diastolic Systolic
# 1 1   9751.23  6676.27
# 2 2  16582.29 15021.36
# 3 3  13911.00 16857.71
# 4 4   7516.09 12607.22
# 5 5   7461.65 10583.38

data.frame(SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, treat_new) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

#       group    lines
# 1 Diastolic 2.857952
# 2  Systolic 3.217764

ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))
#   group           n
#   <chr>       <dbl>
# 1 Diastolic   8390.
# 2 Systolic  146421.

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, treat_new) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

#   n Diastolic Systolic
# 1 1   1555.35 30919.79
# 2 2   2790.57 47730.34
# 3 3   2393.33 36110.39
# 4 4   1179.87 20013.53
# 5 5    471.38 11646.90

data.frame(ARNI_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, treat_new) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

#       group    lines
# 1 Diastolic 2.575491
# 2  Systolic 2.578789


MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group) %>% distinct() %>% group_by(group) %>% summarise(n=sum(weight))

# 1 Diastolic 153829.
# 2 Systolic  214514

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, treat_new) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>% mutate(n=ifelse(n>=5,5,n)) %>% group_by(group, n) %>% summarise(classes=sum(weight)) %>%
  ungroup() %>% spread(key=group, value=classes))

#   n Diastolic Systolic
# 1 1  32759.07 36920.63
# 2 2  49749.92 62156.20
# 3 3  39937.52 53800.62
# 4 4  17289.68 32835.55
# 5 5  14092.41 28800.87

data.frame(MRA_Pats %>% left_join(HF_Drug_Histories) %>% select(patient, weight, group, treat_new) %>% distinct() %>% group_by(patient, weight, group) %>% count() %>%
  ungroup() %>%  group_by(group) %>% summarise(lines=weighted.mean(n,weight)))

# 1 Diastolic 2.598992
# 2  Systolic 2.849325

# --------------------------------------
# All PAts inc death  - Persistency  ------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs!="104")


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Diuretic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_Inotropic <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Inotropic"], collapse = "|"),")\\b")
string_Vasodilator <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Vasodilator"], collapse = "|"),")\\b")
string_Other <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Other"], collapse = "|"),")\\b")
string_Cardiac_Device <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Cardiac Device"], collapse = "|"),")\\b")
string_Hospital_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")
string_Injectable <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")


# Time to first  Diuretic
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Diuretic,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
  group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 


# Time to first  Beta blcoker
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_BetaBlocker,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 


# Time to first  ACE
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_ACE,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 

# Time to first  ARNi
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_ARNI,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 


# Time to first  SGLT2
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_SGLT2,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 

  
# Time to first  MRA
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_MRA,Drugs)) %>% group_by(patient, group, weight) %>% count() %>% ungroup() %>%
  group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 


# Time to first ARB
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_ARB,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 


# Time to first Inotropic
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Inotropic,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 


# Time to first Vasodilator
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Vasodilator,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 



# Time to first Other
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Other,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight)))



# Time to first Cardiac Device
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Cardiac_Device,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight)))


# Time to first Hospital_Inpatient
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Hospital_Inpatient,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 



# Time to first Surgery_Inpatient
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Surgery_Inpatient,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight))) 


# Time to first Heart_Transplant
HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Heart_Transplant,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight)))


HF_Drug_Histories %>% group_by(patient, weight) %>% 
  filter(grepl(string_Injectable,Drugs)) %>% group_by(patient, weight, group) %>% count() %>% ungroup() %>%
    group_by(group) %>%
  summarise(mean=weighted.median(n, as.numeric(weight)))

# --------------------------------
# SGLT2 Molecules Usage Ever & Last 12 months & last month  Last 5y   -----------------------------------------------------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)




HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-c(disease, Exact_Month))

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="SGLT2")

HF_Ingredients <- HF_Ingredients %>% select(molecule, generic_name)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% inner_join(HF_Ingredients) 

data.frame(HF_Drug_Histories %>% 
  select(patient, weight, generic_name, group) %>% distinct() %>%
  group_by(group, generic_name) %>% summarise(n=sum(as.numeric(weight)))) 

# 1 Diastolic Canagliflozin 10899.42
# 2 Diastolic Dapagliflozin 21395.42
# 3 Diastolic Empagliflozin 53786.19
# 4 Diastolic Ertugliflozin  2064.66
# 5  Systolic Canagliflozin  8033.95
# 6  Systolic Dapagliflozin 29975.35
# 7  Systolic Empagliflozin 53851.18
# 8  Systolic Ertugliflozin   947.77



HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"

HF_Comorbidity_Inventories <- Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(HF_Comorbidity_Inventories %>% select(-weight))

CKD_Pats <- HF_Comorbidity_Inventories %>% filter(diagnosis=="N18") %>% select(patient) %>% distinct()

CKD_Pats %>% left_join(Groups_Diastolic_vs_Systolic_L5Y) %>%
  group_by(group) %>% summarise(n=sum(weight))

1 Diastolic 1357363.
2 Systolic   768210.

data.frame(HF_Drug_Histories %>% 
             inner_join(CKD_Pats) %>%
  select(patient, weight, generic_name, group) %>% distinct() %>%
  group_by(group, generic_name) %>% summarise(n=sum(as.numeric(weight)))) 

1 Diastolic Canagliflozin  5660.99
2 Diastolic Dapagliflozin 10690.33
3 Diastolic Empagliflozin 23212.89
4 Diastolic Ertugliflozin   595.18
5  Systolic Canagliflozin  3232.12
6  Systolic Dapagliflozin 13282.61
7  Systolic Empagliflozin 22726.91
8  Systolic Ertugliflozin   294.76

# -----------------------------------------------
# Waterfalls segmentation - Beta Blockers | RAAS| Advanced -----------------------------
# Where are patients now?
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"
Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()
Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, month60)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")


data.frame(
  HF_Drug_Histories %>% group_by(patient) %>% 
  mutate(ON_BetaBlocker=ifelse(grepl(string_BetaBlocker, month60), 1, 0)) %>%
    mutate(ON_RAAS=ifelse(grepl(string_ACE, month60)|grepl(string_ARB, month60), 1, 0)) %>%
      mutate(ON_ARNI=ifelse(grepl(string_ARNI, month60), 1, 0)) %>%
                     mutate(ON_MRA=ifelse(grepl(string_MRA, month60), 1, 0)) %>%
  ungroup() %>%
  group_by(group, ON_BetaBlocker, ON_RAAS, ON_MRA, ON_ARNI) %>%
  summarise(n=sum(weight))
)

Current_m60 <-  HF_Drug_Histories %>% group_by(patient) %>% 
  mutate(ON_BetaBlocker=ifelse(grepl(string_BetaBlocker, month60), 1, 0)) %>%
    mutate(ON_RAAS=ifelse(grepl(string_ACE, month60)|grepl(string_ARB, month60), 1, 0)) %>%
      mutate(ON_ARNI=ifelse(grepl(string_ARNI, month60), 1, 0)) %>%
                     mutate(ON_MRA=ifelse(grepl(string_MRA, month60), 1, 0)) %>%
  ungroup() %>% select(-month60)


# Drug Experience
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"
Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()
Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)





HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, Exact_Month, month1:month60)

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)
HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, Drugs) %>% distinct()

HF_Drug_Histories <- HF_Drug_Histories %>% arrange(patient, group, weight, Drugs) %>%
  group_by(patient, weight, group) %>% mutate(ever_tried = paste(Drugs, collapse=",")) 
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, ever_tried) %>% distinct()


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")

HF_Drug_Histories

data.frame(
  HF_Drug_Histories %>% group_by(patient) %>% 
  mutate(Exp_BetaBlocker=ifelse(grepl(string_BetaBlocker, ever_tried), 1, 0)) %>%
    mutate(Exp_RAAS=ifelse(grepl(string_ACE, ever_tried)|grepl(string_ARB, ever_tried), 1, 0)) %>%
      mutate(Exp_ARNI=ifelse(grepl(string_ARNI, ever_tried), 1, 0)) %>%
                     mutate(Exp_MRA=ifelse(grepl(string_MRA, ever_tried), 1, 0)) %>%
  ungroup() %>%
  group_by(group, Exp_BetaBlocker, Exp_RAAS, Exp_MRA, Exp_ARNI) %>%
  summarise(n=sum(weight))string_BetaBlocker <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_ACE <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ACE"], collapse = "|"),")\\b")
string_ARB <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARB"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
)


Ever_Tried <- HF_Drug_Histories %>% group_by(patient) %>% 
  mutate(Exp_BetaBlocker=ifelse(grepl(string_BetaBlocker, ever_tried), 1, 0)) %>%
    mutate(Exp_RAAS=ifelse(grepl(string_ACE, ever_tried)|grepl(string_ARB, ever_tried), 1, 0)) %>%
      mutate(Exp_ARNI=ifelse(grepl(string_ARNI, ever_tried), 1, 0)) %>%
                     mutate(Exp_MRA=ifelse(grepl(string_MRA, ever_tried), 1, 0)) %>%
  ungroup() %>%  select(-ever_tried)


temp <- Current_m60 %>% left_join(Ever_Tried)

temp %>% ungroup() %>%
  group_by(group, Exp_BetaBlocker, Exp_RAAS, Exp_MRA, Exp_ARNI) %>%
  summarise(n=sum(weight)) %>%
  filter(group=="Diastolic")

fwrite(temp, "Current_m60_vs_EverTried.csv", sep=",")

names(temp)

temp_summary <- data.frame(temp %>% 
  group_by(group, ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA) %>%
  summarise(n=sum(weight)))

fwrite(temp_summary, "Current_m60_vs_EverTried_Summary.csv", sep=",")




# Time to class
HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, Exact_Month, month1:month60)
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)

FirstAdvanced <- HF_Drug_Histories %>% 
  filter(grepl(string_ARNI,Drugs)|grepl(string_MRA,Drugs)) %>% select(patient) %>% distinct() %>%
  left_join(HF_Drug_Histories) %>%
    filter(grepl(string_ARNI,Drugs)|grepl(string_MRA,Drugs)) %>% 
  group_by(patient, weight, group) %>% filter(Month==min(Month)) %>%
  select(-Drugs) %>% rename("FirstAdvanced"="Month")

FirstBetablocker <- HF_Drug_Histories %>% 
  filter(grepl(string_BetaBlocker,Drugs)) %>% select(patient) %>% distinct() %>%
  left_join(HF_Drug_Histories) %>%
    filter(grepl(string_BetaBlocker,Drugs)) %>% 
  group_by(patient, weight, group) %>% filter(Month==min(Month)) %>%
  select(-Drugs) %>% rename("FirstBetablocker"="Month")

FirstRAAS <- HF_Drug_Histories %>% 
  filter(grepl(string_ACE,Drugs)|grepl(string_ARB,Drugs)) %>% select(patient) %>% distinct() %>%
  left_join(HF_Drug_Histories) %>%
    filter(grepl(string_ACE,Drugs)|grepl(string_ARB,Drugs)) %>% 
  group_by(patient, weight, group) %>% filter(Month==min(Month)) %>%
  select(-Drugs) %>% rename("FirstRAAS"="Month")



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
  filter(!is.na(FirstBetablocker) & is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  summarise(BB_to_Advanced=weighted.mean(FirstAdvanced-FirstBetablocker, weight))
  

temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
  filter(is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>% 
    group_by(group) %>%
  summarise(RAAS_to_Advanced=weighted.mean(FirstAdvanced-FirstRAAS, weight))
  



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  summarise(BB_to_Advanced=weighted.mean(FirstAdvanced-FirstBetablocker, weight))
  


temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  summarise(RAAS_to_Advanced=weighted.mean(FirstAdvanced-FirstRAAS, weight))
  



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  summarise(BB_to_RAAS=weighted.mean(FirstRAAS-FirstBetablocker, weight))
  


temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  mutate(BB_to_RAAS=FirstRAAS-FirstBetablocker) %>%
  filter(BB_to_RAAS <0) %>% summarise(n=sum(weight))



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  mutate(BB_to_RAAS=FirstRAAS-FirstBetablocker) %>%
  filter(BB_to_RAAS == 0) %>% summarise(n=sum(weight))

temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  mutate(BB_to_RAAS=FirstRAAS-FirstBetablocker) %>%
  filter(BB_to_RAAS >0) %>% summarise(n=sum(weight))
# ----------------------------------
# HbA1c relative to SGLT2 initiation --------------------------------

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")

SGLT2_Pats <- HF_Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patient) %>% distinct()
 
HF_Drug_Histories <- SGLT2_Pats %>% left_join(HF_Drug_Histories) %>% select(-c(disease, Exact_Month)) %>%
  group_by(patient)  %>% filter(grepl(string_SGLT2, Drugs)) %>% 
  filter(Month==min(Month)) %>% select(patient, group, weight, Month) %>%
  rename("FirstSGLT2"="Month")


DANU_Measures_Full <- fread("DANU Measures Full.txt")
DANU_Measures_Full <- DANU_Measures_Full %>% filter(test=="HbA1c Level") %>% select(patid, weight, claimed, value) 
names(DANU_Measures_Full)[1] <- "patient"

DANU_Measures_Full <- SGLT2_Pats %>% inner_join(DANU_Measures_Full)

DANU_Measures_Full <- DANU_Measures_Full %>% mutate(claimed=as.character(claimed))
DANU_Measures_Full <- DANU_Measures_Full %>% mutate(claimed=str_sub(claimed, 1L, 7L))
DANU_Measures_Full <- DANU_Measures_Full %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(-claimed)

DANU_Measures_Full <- HF_Drug_Histories %>% full_join(DANU_Measures_Full) %>%
  arrange(patient, group, weight, FirstSGLT2, Exact_Month, value)

DANU_Measures_Full %>% drop_na() %>% mutate(Elapsed=Exact_Month-FirstSGLT2) %>%
  #filter(Elapsed>(-12) & Elapsed<12) %>%
  ggplot(aes(Elapsed, value)) +
  geom_smooth()


DANU_Measures_Full %>% drop_na() %>% mutate(Elapsed=Exact_Month-FirstSGLT2) %>%
  group_by(patient) %>% mutate(MIN=min(value)) %>% 
  mutate(Change=100*(value-MIN)/MIN ) %>%
 #filter(Elapsed>(-30) & Elapsed<30) %>%
  ggplot(aes(Elapsed+10, Change)) +
  geom_smooth(fill="darkslategray4", colour="darkslategray", size=2) +
  theme_minimal() +
  xlab("\n No. Lapsed Months Relative to SGLT2 1st Initiation") + 
  ylab("% Change over individual patient baseline \n(smoothed estimate) \n") 
  
# ---------------------------------
# Waterfalls segmentation - Beta Blockers | RAAS| Advanced Classes II-IV and III-IV -------------------------------------
Stages <- fread("Predicted_Stages_gbm_All.txt")

Stages %>% filter(Predicted.Stage>2) %>% select(patient)

temp <- fread("Current_m60_vs_EverTried.csv")

temp %>% ungroup() %>% 
  inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  group_by(group, Exp_BetaBlocker, Exp_RAAS, Exp_MRA, Exp_ARNI) %>%
  summarise(n=sum(weight)) %>%
  filter(group=="Diastolic")

temp_summary <- data.frame(temp %>% 
  group_by(group, ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA) %>%
  summarise(n=sum(weight)))



# Time to class
HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, group, weight, Exact_Month, month1:month60)
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)

FirstAdvanced <- HF_Drug_Histories %>% 
  filter(grepl(string_ARNI,Drugs)|grepl(string_MRA,Drugs)) %>% select(patient) %>% distinct() %>%
  left_join(HF_Drug_Histories) %>%
    filter(grepl(string_ARNI,Drugs)|grepl(string_MRA,Drugs)) %>% 
  group_by(patient, weight, group) %>% filter(Month==min(Month)) %>%
  select(-Drugs) %>% rename("FirstAdvanced"="Month")

FirstBetablocker <- HF_Drug_Histories %>% 
  filter(grepl(string_BetaBlocker,Drugs)) %>% select(patient) %>% distinct() %>%
  left_join(HF_Drug_Histories) %>%
    filter(grepl(string_BetaBlocker,Drugs)) %>% 
  group_by(patient, weight, group) %>% filter(Month==min(Month)) %>%
  select(-Drugs) %>% rename("FirstBetablocker"="Month")

FirstRAAS <- HF_Drug_Histories %>% 
  filter(grepl(string_ACE,Drugs)|grepl(string_ARB,Drugs)) %>% select(patient) %>% distinct() %>%
  left_join(HF_Drug_Histories) %>%
    filter(grepl(string_ACE,Drugs)|grepl(string_ARB,Drugs)) %>% 
  group_by(patient, weight, group) %>% filter(Month==min(Month)) %>%
  select(-Drugs) %>% rename("FirstRAAS"="Month")



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  filter(!is.na(FirstBetablocker) & is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  summarise(BB_to_Advanced=weighted.mean(FirstAdvanced-FirstBetablocker, weight))
  

temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  filter(is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>% 
    group_by(group) %>%
  summarise(RAAS_to_Advanced=weighted.mean(FirstAdvanced-FirstRAAS, weight))
  



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  summarise(BB_to_Advanced=weighted.mean(FirstAdvanced-FirstBetablocker, weight))
  


temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  summarise(RAAS_to_Advanced=weighted.mean(FirstAdvanced-FirstRAAS, weight))
  



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  summarise(BB_to_RAAS=weighted.mean(FirstRAAS-FirstBetablocker, weight))
  


temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  mutate(BB_to_RAAS=FirstRAAS-FirstBetablocker) %>%
  filter(BB_to_RAAS <0) %>% summarise(n=sum(weight))



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  mutate(BB_to_RAAS=FirstRAAS-FirstBetablocker) %>%
  filter(BB_to_RAAS == 0) %>% summarise(n=sum(weight))

temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  filter(!is.na(FirstBetablocker) & !is.na(FirstRAAS) & !is.na(FirstAdvanced)) %>%
  group_by(group) %>%
  mutate(BB_to_RAAS=FirstRAAS-FirstBetablocker) %>%
  filter(BB_to_RAAS >0) %>% summarise(n=sum(weight))



# Diastolic


temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient)) %>%
  filter(group=="Systolic") %>%
  #summarise(n=sum(weight)) %>%      # 1457267
  #filter(FirstBetablocker>FirstRAAS) %>% summarise(n=sum(weight)) # 187352
  #filter(FirstBetablocker<FirstRAAS) %>% summarise(n=sum(weight)) # 181253.2
  #filter(FirstBetablocker==FirstRAAS) %>% summarise(n=sum(weight)) # 610559

  
temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA, Exp_BetaBlocker, Exp_RAAS, Exp_ARNI, Exp_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient)) %>%
  filter(group=="Diastolic") %>%
  filter(is.na(FirstBetablocker)&is.na(FirstRAAS)) %>% 
  filter(!is.na(FirstAdvanced)) %>%
  summarise(n=sum(weight))


temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient)) %>%
    filter(group=="Diastolic") %>%
  filter(Exp_RAAS==0&Exp_BetaBlocker==0&(Exp_ARNI==1|Exp_MRA==1)) %>%
  summarise(n=sum(weight))



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient)) %>%
    filter(group=="Diastolic") %>%
  filter(Exp_RAAS==1&Exp_BetaBlocker==1) %>%
 # filter((Exp_ARNI==1|Exp_MRA==1)) %>%
  summarise(n=sum(weight))



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient)) %>%
    filter(group=="Systolic")  %>%
  filter(Exp_RAAS==1|Exp_BetaBlocker==1) %>%
 filter( (FirstBetablocker<FirstRAAS) | (is.na(FirstRAAS)&!is.na(FirstBetablocker)) ) %>% 
  filter((Exp_ARNI==1|Exp_MRA==1)) %>%
  summarise(n=sum(weight))

temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient)) %>%
    filter(group=="Diastolic")  %>%
  filter(Exp_RAAS==1|Exp_BetaBlocker==1) %>%
 filter( (FirstBetablocker<FirstRAAS) | (is.na(FirstRAAS)&!is.na(FirstBetablocker)) ) %>% 
     filter(Exp_RAAS==1&Exp_BetaBlocker==1) %>%
  summarise(n=sum(weight))



temp %>% left_join(FirstAdvanced) %>% left_join(FirstBetablocker) %>% left_join(FirstRAAS) %>%
  select(-c(ON_BetaBlocker, ON_RAAS, ON_ARNI, ON_MRA)) %>%
    inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient)) %>%
    filter(group=="Diastolic")  %>%
  filter( (Exp_RAAS==1&Exp_BetaBlocker==1) ) %>%
  filter(Exp_ARNI==1|Exp_MRA==1) %>%
  summarise(n=sum(weight))
# ------------------------
# Drug Usage last 12 months for classes II-IV and III-IV -------------------------------------
Stages <- fread("Predicted_Stages_gbm_All.txt")

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)


HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")

HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-c(disease, Exact_Month))

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))

HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)

HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs, group) %>% distinct() %>% left_join(HF_Ingredients) 


HF_Demographics %>% inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient)) %>%
  inner_join(Groups_Diastolic_vs_Systolic_L5Y) %>%
  select(patient, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))
 
# 1 Diastolic 2305855.
# 2 Systolic  1457267.

# 1 Diastolic 1067714.
# 2 Systolic   621920.

data.frame(HF_Drug_Histories %>% 
             inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient))  %>%
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

data.frame(HF_Drug_Histories %>% 
                          inner_join(Stages %>% filter(Predicted.Stage>2) %>% select(patient))  %>%
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
             mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB"|drug_class=="ARNI", "RAAS", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

# string_Surgery_Inpatient <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Surgery Inpatient"], collapse = "|"),")\\b")
# string_Heart_Transplant <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Heart Transplant"], collapse = "|"),")\\b")
# string_Injectables <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")
# 
# 
# HF_Drug_Histories_LONG <- fread("HF Drug Histories.txt")
# HF_Drug_Histories_LONG <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories_LONG)
# 
# HF_Drug_Histories_LONG <- gather(HF_Drug_Histories_LONG, Month, Drugs, month1:month60, factor_key=TRUE)
# HF_Drug_Histories_LONG <- HF_Drug_Histories_LONG %>% select(patient, Drugs) %>% distinct()
# AdvPats <- HF_Drug_Histories_LONG %>% filter(grepl(string_Heart_Transplant,Drugs)|grepl(string_Surgery_Inpatient,Drugs)|grepl(string_Injectables,Drugs)) %>%
#   select(patient) %>% distinct()



HF_Demographics %>% inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient)) %>%
  inner_join(Groups_Diastolic_vs_Systolic_L5Y) %>%
  inner_join(AdvPats) %>%
  select(patient, weight, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))
# 
# 1 Diastolic 576469.
# 2 Systolic  383585.
# 
# 1 Diastolic 393173.
# 2 Systolic  252366.


data.frame(HF_Drug_Histories %>% 
               inner_join(AdvPats) %>%
             inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient))  %>%
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

data.frame(HF_Drug_Histories %>% 
               inner_join(AdvPats) %>%
                          inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient))  %>%
             mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) %>%
             mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB"|drug_class=="ARNI", "RAAS", drug_class)) %>%
  select(patient, weight, drug_class, group) %>% distinct() %>%
  group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

# -----------------------------
#  % Above 65 ----------------
Stages <- fread("Predicted_Stages_gbm_All.txt")

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Demographics <- fread("HF Demographics.txt")
HF_Demographics <- HF_Demographics %>% select(patid, age)
names(HF_Demographics)[1] <- "patient"

Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient))  %>%
  left_join(HF_Demographics) %>%
  group_by(group) %>% summarise(n=mean(age))

1 Diastolic  74.9
2 Systolic   70.8


Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient))  %>%
  left_join(HF_Demographics) %>% mutate(age=ifelse(age>=65,1,0)) %>%
  group_by(group, age) %>% summarise(n=sum(weight))

# 1 Diastolic     0  396823.
# 2 Diastolic     1 1909032.
# 3 Systolic      0  417018.
# 4 Systolic      1 1040248.


Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient))  %>%
  left_join(HF_Demographics) %>%
  ggplot(aes(age, colour=group, fill=group)) +
  geom_density(alpha=0.6) +
  theme_minimal() +
  scale_color_manual(values = c("deepskyblue4", "firebrick")) +
  scale_fill_manual(values = c("deepskyblue4", "firebrick")) +
  xlab("\n Age distribution (y)") +
  ylab("Patient density \n")

# -----------------------
# Molecule Stock over time ARNi , MRA, SGLT2 ----------------
Stages <- fread("Predicted_Stages_gbm_All.txt")

First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(Stages %>% filter(Predicted.Stage>1) %>% select(patient))  


HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, generic_name)
unique(HF_Ingredients$drug_class)
HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="MRA"|drug_class=="SGLT2"|drug_class=="ARNI") %>% select(-drug_class)
names(HF_Ingredients)[1] <- "Drugs"


HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Month, Drugs, group) %>% distinct() %>% inner_join(HF_Ingredients) 

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Month, generic_name, group) %>% distinct() 


data.frame(HF_Drug_Histories %>% mutate(Month=parse_number(as.character(Month))) %>%
  group_by(group, Month, generic_name) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>%
  spread(key=Month, value=n)) %>% 
  arrange(group, -X60) %>%
  mutate(CAGR= 100*( ((X60/X1)^(1/5))-1) ) %>%
  select(group, generic_name, CAGR)

      group         generic_name      CAGR
1  Diastolic       Spironolactone  21.76262
2  Diastolic        Empagliflozin  54.52111
3  Diastolic        Dapagliflozin  26.66474
4  Diastolic Sacubitril/Valsartan        NA
5  Diastolic           Eplerenone  21.91369
6  Diastolic        Canagliflozin -17.98138
7  Diastolic        Ertugliflozin        NA
8   Systolic       Spironolactone  34.72278
9   Systolic Sacubitril/Valsartan 161.42897
10  Systolic        Empagliflozin  81.21861
11  Systolic        Dapagliflozin  37.64818
12  Systolic           Eplerenone  22.57127
13  Systolic        Canagliflozin -24.96236
14  Systolic        Ertugliflozin        NA




HF_Drug_Histories %>% mutate(Month=parse_number(as.character(Month))) %>%
  group_by(group, Month, generic_name) %>% summarise(n=sum(as.numeric(weight))) %>%
  ungroup() %>%
  mutate(generic_name=factor(generic_name, levels=c("Spironolactone", "Eplerenone", "Sacubitril/Valsartan", "Empagliflozin", "Dapagliflozin", "Canagliflozin", "Ertugliflozin"))) %>%
  ggplot(aes(Month, n, colour=generic_name)) +
  geom_line(size=2, alpha=0.6) +
  facet_wrap(~group) +
  theme_minimal() +
  scale_colour_manual(values=c("#0693e3", "#8ed1fc","#009688","#ff0000", "#ff4800","#ffa500", "#f7ff00")) +
  xlab("\n Month") + ylab("Population ON each molecule \n")

# ---------------------------------------
# All PAts inc death  - Class Penetrance 12 months before and after 1st Dx - Break up diuretics --------------------------------------------------

HF_Demographics <- fread("HF Demographics.txt", colClasses = "character")
HF_Demographics <- HF_Demographics %>% select(patid, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))
names(HF_Demographics)[1] <- "patient"

Groups <- fread("Groups_Diastolic_vs_Systolic_ALL.txt", sep=",")
HF_Demographics <- Groups %>% left_join(HF_Demographics)


Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, group, Exact_Month) %>% distinct()


HF_Drug_Histories <- fread("HF Drug Histories.txt", colClasses = "character")
HF_Drug_Histories <- HF_Demographics %>% select(patient) %>% inner_join(HF_Drug_Histories)

HF_Demographics %>%
  ggplot(aes(Exact_Month)) +
  geom_density(fill="darkslategray4", colour="darkslategray", size=1, alpha=0.5) +
  theme_minimal() +
  xlab("\n Month of 1st Heart Failure Dx") + 
  ylab("Patient Density \n(Gaussian kernel) \n") 

names(HF_Demographics)[3] <- "First_HF_Dx"

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs!="104")
HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(generic_name=="Bendroflumethiazide"|
                                              generic_name=="Benzthiazide"|
                                              generic_name=="Chlorothiazide"|
                                              generic_name=="Chlorthalidone"|
                                              generic_name=="Cyclothiazide"|
                                              generic_name=="Hydrochlorothiazide"|
                                              generic_name=="Hydroflumethiazide"|
                                              generic_name=="Indapamide"|
                                              generic_name=="Methyclothiazide"|
                                              generic_name=="Metolazone"|
                                              generic_name=="Polythiazide"|
                                              generic_name=="Quinethazone"|
                                              generic_name=="Polythiazide"|
                                              generic_name=="Trichlormethiazide", "Thiazide Diuretic",
                                            ifelse(generic_name=="Bumetanide"|
                                                     generic_name=="Ethacrynate"|
                                                     generic_name=="Torsemide"|
                                                     generic_name=="Furosemide", "Loop Diuretic", drug_class)))

Acetazolamide         CA Inhibitor
Amiloride             K-sparing diuretic
Bendroflumethiazide   Thiazide diuretic
Benzthiazide          Thiazide diuretic
Bumetanide             Loop diuretic
Chlorothiazide        Thiazide diuretic
Chlorthalidone         Thiazide diuretic
Cyclothiazide         Thiazide diuretic
Ethacrynate            Loop diuretic
Furosemide            Loop diuretic
Hydrochlorothiazide   Thiazide diuretic
Hydroflumethiazide    Thiazide diuretic
Indapamide            Thiazide diuretic
Mersalyl              Mercurial diuretic.
Methyclothiazide       Thiazide diuretic
Metolazone            Thiazide diuretic
Polythiazide          Thiazide diuretic
Quinethazone           Thiazide diuretic
Torsemide             Loop diuretic
Triamterene           K-sparing diuretic
Trichlormethiazide     Thiazide diuretic



HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) 

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients %>% select(-drug_group)) 

HF_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 

HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Demographics) %>% 
  mutate(Lapsed=Month-First_HF_Dx) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(HF_Drug_Histories)

HF_Drug_Histories%>% left_join(Groups) %>% filter(group=="Systolic") %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 1004756

HF_Drug_Histories %>% left_join(Groups) %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)


HF_Drug_Histories  %>% left_join(Groups) %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/338792) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Loop_Diuretic", "Thiazide_Diuretic", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=.8) +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#001E60", "#5F69B1",  "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))


# ------------------------------
# Class Penetrance 12 months before and after SGLT2/MRA/ARNI Dx Last 5y -  Break up diuretics --------------------------------------------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.character(heart_failure_onset))
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=str_sub(heart_failure_onset, 1L, 7L))

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HF_Demographics <- HF_Demographics %>% left_join(Months_lookup, by=c("heart_failure_onset"="Month")) %>% select(patient, weight, Exact_Month) %>% distinct()

Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Demographics) %>% select(patient, group, weight, Exact_Month)

HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 

HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-disease)

HF_Drug_Histories <- HF_Drug_Histories %>% filter(Month>=Exact_Month) %>% select(-Exact_Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-" & Drugs!="104")


HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_MRA <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_SGLT2 <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="SGLT2"], collapse = "|"),")\\b")




First_MRA <- HF_Drug_Histories %>% filter(grepl(string_MRA, Drugs)) %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, Month) %>% rename("First_MRA"="Month")

First_MRA <- First_MRA %>% left_join(HF_Drug_Histories)

First_MRA <- separate_rows(First_MRA, Drugs, sep = ",", convert=T)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(generic_name=="Bendroflumethiazide"|
                                              generic_name=="Benzthiazide"|
                                              generic_name=="Chlorothiazide"|
                                              generic_name=="Chlorthalidone"|
                                              generic_name=="Cyclothiazide"|
                                              generic_name=="Hydrochlorothiazide"|
                                              generic_name=="Hydroflumethiazide"|
                                              generic_name=="Indapamide"|
                                              generic_name=="Methyclothiazide"|
                                              generic_name=="Metolazone"|
                                              generic_name=="Polythiazide"|
                                              generic_name=="Quinethazone"|
                                              generic_name=="Polythiazide"|
                                              generic_name=="Trichlormethiazide", "Thiazide Diuretic",
                                            ifelse(generic_name=="Bumetanide"|
                                                     generic_name=="Ethacrynate"|
                                                     generic_name=="Torsemide"|
                                                     generic_name=="Furosemide", "Loop Diuretic", drug_class)))
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients <- HF_Ingredients %>% mutate(drug_class=ifelse(drug_group=="Injectable Therapy", "Injectables", drug_class)) 

First_MRA <- First_MRA %>% left_join(HF_Ingredients %>% select(-drug_group)) 

First_MRA %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight)))  # 219703

First_MRA <- First_MRA %>% mutate(Lapsed=Month-First_MRA) %>% filter((Lapsed>=(-6)) & (Lapsed<=(6)))

First_MRA <- First_MRA %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=13) %>%
  select(patient) %>% left_join(First_MRA)

First_MRA %>% filter(group=="Systolic") %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 30362

First_MRA %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  spread(key=Lapsed, value=n)

First_MRA %>% filter(group=="Systolic") %>% 
  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/78097) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Loop_Diuretic", "Thiazide_Diuretic", "Diuretic", "ACE", "ARB", "MRA", "ARNI", "Vasodilator"  ,
                                                "SGLT2", "Inotropic","Other", "Injectables", "Hospital_Inpatient", "Cardiac_Device", "Heart_Transplant", "Surgery_Inpatient"))) %>%
   ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=.8) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st MRA)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF", "#001E60", "#5F69B1",  "#0099BD","#69E2FF","#09D0FF", "#AE1641","#FFBFBF", "#FFF9EB",
                                        "#F1AC02", "#FFEFCA","#FFE4A7", "#B482DA", "#6BCF6B", "#96DE96", "#E8F8E8","#D1F0D1"))

# ------------------------------------
# Specialty ----------------
First_Diastolic <- fread("First_Diastolic_All_L5y.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"
HF_Demographics <- HF_Demographics %>% mutate(heart_failure_onset=as.Date(heart_failure_onset)) %>% filter(heart_failure_onset >= "2016-05-01")
HF_Demographics <- HF_Demographics %>% select(patient, weight, heart_failure_onset)


HF_Doses <- fread("HF Doses.txt", colClasses = "character")
HF_Doses <- HF_Doses %>% select(pat_id, drug_class, from_dt, specialty) %>% distinct() %>% mutate(from_dt=as.Date(from_dt))
names(HF_Doses)[1] <- "patient"

HF_Doses <- HF_Doses %>% inner_join(HF_Demographics) %>% filter(from_dt>=heart_failure_onset)

HF_Doses <- HF_Doses %>% inner_join(Groups_Diastolic_vs_Systolic_L5Y %>% select(-earliest))

First_ARNI <- HF_Doses %>% filter(drug_class=="ARNI") %>% group_by(patient) %>% filter(from_dt==min(from_dt))
First_MRA <- HF_Doses %>% filter(drug_class=="MRA") %>% group_by(patient) %>% filter(from_dt==min(from_dt))
First_SGLT2 <- HF_Doses %>% filter(drug_class=="SGLT2") %>% group_by(patient) %>% filter(from_dt==min(from_dt))

names(First_ARNI)[4] <- "code"
names(First_MRA)[4] <- "code"
names(First_SGLT2)[4] <- "code"

DANU_Specialty_Codes <- fread("DANU Specialty Codes.txt")
DANU_Specialty_Codes <- DANU_Specialty_Codes %>% select(code, specialty)

data.frame(First_ARNI %>% ungroup()  %>% left_join(DANU_Specialty_Codes)) %>%
  group_by(group, specialty) %>% count() %>% arrange(group, -n)


data.frame(data.frame(First_MRA %>% ungroup()  %>% left_join(DANU_Specialty_Codes)) %>%
  group_by(group, specialty) %>% count() %>% arrange(group, -n))


data.frame(data.frame(First_SGLT2 %>% ungroup()  %>% left_join(DANU_Specialty_Codes)) %>%
  group_by(group, specialty) %>% count() %>% arrange(group, -n))

# --------------------

# Dx rate and Rx rate per NYHA class Jay --------------------------

First_Diastolic <- fread("First_Diastolic_All.txt")
First_Diastolic$group <- "Diastolic"
First_Systolic <- fread("First_Systolic_All.txt")
First_Systolic$group <- "Systolic"

Stages <- fread("Predicted_Stages_gbm_All.txt")

Groups_Diastolic_vs_Systolic_L5Y <- First_Diastolic %>% full_join(First_Systolic) 

DANU_Demographics <- fread("DANU Demographics Full.txt")

Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(Stages) %>% 
  left_join(DANU_Demographics %>% select(patid, weight), by=c("patient"="patid")) %>%
  group_by(group) %>% summarise(n=sum(weight))

1 Diastolic 3366024.
2 Systolic  2360626.

Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(Stages) %>% 
  left_join(DANU_Demographics %>% select(patid, weight), by=c("patient"="patid")) %>%
  group_by(group, `Predicted.Stage`) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=`Predicted.Stage`, value=n)

  group         `1`      `2`      `3`     `4`
  <chr>       <dbl>    <dbl>    <dbl>   <dbl>
1 Diastolic 400859. 1502497. 1173935. 288733.
2 Systolic  343361. 1102638.  731234. 183393.


Groups_Diastolic_vs_Systolic_L5Y <- Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(Stages) %>% 
  left_join(DANU_Demographics %>% select(patid, weight), by=c("patient"="patid")) %>%
  select(-earliest)



HF_Drug_Histories <- fread("HF Drug Histories.txt")
HF_Drug_Histories <- Groups_Diastolic_vs_Systolic_L5Y %>% left_join(HF_Drug_Histories)
sum(as.numeric(HF_Drug_Histories$weight)) 
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, month49:month60)
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month49:month60, factor_key=TRUE)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="-")
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs!="104")
HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct()

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
#HF_Ingredients <- HF_Ingredients %>% filter(drug_class=="ACE"|drug_class=="ARB"|drug_class=="ARNI"|drug_class=="MRA"|drug_class=="Beta Blocker"|drug_class=="SGLT2")
HF_Drug_Histories <- HF_Drug_Histories %>% select(patient, Drugs) %>% distinct() %>% inner_join(HF_Ingredients) 

Groups_Diastolic_vs_Systolic_L5Y %>% inner_join(HF_Drug_Histories %>% select(patient) %>% distinct()) %>%
  group_by(group, `Predicted.Stage`) %>% summarise(n=sum(weight)) %>%
  ungroup() %>% spread(key=`Predicted.Stage`, value=n)


# ------------------

# Heart Failure Demand Study Patient Attributes --------------
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"

First_Systolic <- First_Systolic %>% left_join(HF_Demographics %>% select(patient, age, gender, weight)) %>% select(-c(earliest, group))
sum(First_Systolic$weight)

First_Systolic %>% group_by(gender) %>% summarise(n=sum(weight)/1706563)

First_Systolic %>%
  mutate(age=ifelse(age<=40, "<40", ifelse(age<=50, "<50", ifelse(age<=65, "<65", ifelse(age<=75, "<75", ">76"))))) %>%
  group_by(age) %>% summarise(n=sum(weight)/1706563)

Stages <- fread("Predicted_Stages_gbm_All.txt", colClasses = "character", sep="\t")
unique(Stages$Predicted.Stage)

First_Systolic %>% left_join(Stages) %>%
  group_by(Predicted.Stage) %>% summarise(n=sum(weight)/1706563)


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"
HF_Comorbidity_Inventories <- First_Systolic %>% select(patient) %>% inner_join(HF_Comorbidity_Inventories)

HF_Comorbidity_Inventories %>% filter(grepl("I10", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1706563)
HF_Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1706563)
HF_Comorbidity_Inventories %>% filter(grepl("E66", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1706563)
HF_Comorbidity_Inventories %>% filter(grepl("E78", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1706563)
HF_Comorbidity_Inventories %>% filter(grepl("I48", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1706563)

HF_Comorbidity_Inventories %>% filter(grepl("I20", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1706563)
HF_Comorbidity_Inventories %>% filter(grepl("I95", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1706563)
HF_Comorbidity_Inventories %>% filter(grepl("E875", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1706563)


DANU_Demographics <- fread("DANU Demographics Full.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"

First_Systolic %>% left_join(DANU_Demographics) %>% group_by(diagnosis) %>% summarise(n=sum(weight)/1706563)




DANU_Utilizations_Full <- fread("DANU Utilizations Full.txt",  colClasses = "character", stringsAsFactors = F)
DANU_Utilizations_Full <- DANU_Utilizations_Full %>% select(patid, hospital_stays) %>% rename("patient"="patid")

First_Systolic  %>% left_join(DANU_Utilizations_Full)   %>% 
  mutate(hospital_stays=as.numeric(hospital_stays)) %>% 
  mutate(hospital_stays=ifelse(hospital_stays>=5,5,hospital_stays)) %>% 
  group_by(hospital_stays) %>% summarise(n=sum(weight)/ (5*1706563))



HF_Drug_Histories <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-c(disease))
HF_Drug_Histories <- First_Systolic %>% left_join(HF_Drug_Histories)
HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_IV_Diuretics <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"&HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")
string_Hospital <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
# 95 Ivabradine
# 92 Vericiguat


HF_Drug_Histories %>% filter(Month>=49) %>% filter(grepl(string_IV_Diuretics, Drugs)) %>% group_by(patient, weight) %>% count() %>%
  mutate(n=ifelse(n>=5,5,n)) %>% group_by(n) %>% summarise(n2=sum(weight))

HF_Drug_Histories %>% filter(Month>=49) %>% filter(grepl(string_Hospital, Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n2=sum(weight))


HF_Drug_Histories %>% filter(Month>=49) %>% arrange(patient, Month) %>%filter( (grepl(string_Hospital, Drugs)&grepl("95", Drugs))|
                                                      (grepl(string_Hospital, Drugs)&grepl("95", lag(Drugs)))|
                                                      (grepl(string_Hospital, Drugs)&grepl("95", lead(Drugs)))|
                                                      (grepl("95", Drugs)&grepl(string_Hospital, Drugs))|
                                                      (grepl("95", Drugs)&grepl(string_Hospital, lag(Drugs)))|
                                                      (grepl("95", Drugs)&grepl(string_Hospital, lead(Drugs)))) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n2=sum(weight))


HF_Drug_Histories %>% filter(Month>=49) %>% filter(grepl(string_IV_Diuretics, Drugs)) %>%  filter(grepl(string_Hospital, Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n2=sum(weight))


DANU_Measures <- fread("DANU Measures Full.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="LVEF Fraction")
DANU_Measures <- DANU_Measures %>% select(-c(source, description, vague_value, vague_date, metric))
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- First_Systolic %>% inner_join(DANU_Measures)


length(unique(DANU_Measures$patient))
DANU_Measures %>% group_by(patient) %>% filter(value<50) %>%
  filter(value==min(value)) %>% slice(1) %>%
  mutate(value=ifelse(value<=29,"<=29", ifelse(value<=39, "<=39", "40+"))) %>%
  group_by(value) %>% summarise(n=sum(weight))


# NEW DATA


MktClarity_HF_cepts_Age_Gender <- fread("MktClarity_HF_cepts_Age&Gender.txt")
names(MktClarity_HF_cepts_Age_Gender)[2] <- "age"
names(MktClarity_HF_cepts_Age_Gender)[3] <- "gender"
MktClarity_HF_cepts_Age_Gender <- MktClarity_HF_cepts_Age_Gender %>% mutate(gender=ifelse(gender=="Male", "M", "F"))

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(age, gender, weight) %>% distinct()
data.frame(DANU_Demographics %>% arrange(gender, age))
range(DANU_Demographics$age)

unique(MktClarity_HF_cepts_Age_Gender$age)
MktClarity_HF_cepts_Age_Gender %>% mutate(age=as.numeric(age)) %>% left_join(DANU_Demographics) %>% summarise(n=sum(weight, na.rm = T))
MktClarity_HF_cepts_Age_Gender <- MktClarity_HF_cepts_Age_Gender %>% mutate(age=as.numeric(age)) %>% filter(age>=18)


MktClarity_HF_cepts_HFdxs <- fread("MktClarity_HF_cepts_HFdxs.txt")
MktClarity_HF_cepts_HFdxs <- MktClarity_HF_cepts_HFdxs %>% mutate(Systolic=ifelse(grepl("I502", DIAG)|grepl("I504", DIAG), 1,0)) %>%
  mutate(Diastolic=ifelse(grepl("I503", DIAG)|grepl("I504", DIAG), 1,0)) 


Systolic <- MktClarity_HF_cepts_HFdxs %>% filter(Systolic==1) %>% group_by(PTID) %>% count() %>% filter(n>1) %>% select(PTID) %>% distinct() %>% mutate(Systolic="Systolic")
Diastolic <- MktClarity_HF_cepts_HFdxs %>% filter(Diastolic==1) %>% group_by(PTID) %>% count() %>% filter(n>1) %>% select(PTID) %>% distinct() %>% mutate(Diastolic="Diastolic")

Groups <- Systolic %>% full_join(Diastolic)

Groups %>% group_by(Systolic, Diastolic) %>% count()

Systolic <- Groups %>% filter(Systolic=="Systolic"&is.na(Diastolic)) %>% select(PTID)


fwrite(Systolic, "HFrEF_Systolic_DemandStudy.txt", sep="\t")





MktClarity_HFrEF_rxs <- fread("MktClarity_HFrEF_rxs.txt", colClasses = "character")
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% filter(TRANSACTION_TYPE=="PAID") %>% select(-c(TRANSACTION_TYPE, chunk_id, STRENGTH, QUANTITY,  BRND_NM, PROV_UNIQUE, PROV_SPECIALTY))
length(unique(MktClarity_HFrEF_rxs$PTID)) # 98169

Heart_Failure_Demand_Study_Attributes <- read_xlsx(path="Heart_Failure_Demand_Study_Attributes.xlsx",sheet = "HF Rxs All", skip=0, col_types = "text", trim_ws = TRUE)
Heart_Failure_Demand_Study_Attributes <- Heart_Failure_Demand_Study_Attributes %>% select(med_code, drug_class)
Heart_Failure_Demand_Study_Attributes <- Heart_Failure_Demand_Study_Attributes %>% mutate(med_code=str_sub(med_code, 3L, 13L))
names(Heart_Failure_Demand_Study_Attributes)[1] <- "NDC"

MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% left_join(Heart_Failure_Demand_Study_Attributes)
range(MktClarity_HFrEF_rxs$FILL_DT) # "2016-07-30" "2022-09-30"

MktClarity_HFrEF_rxs$FILL_DT <- as.Date(MktClarity_HFrEF_rxs$FILL_DT) 
MktClarity_HFrEF_rxs$DAYS_SUP <- as.numeric(MktClarity_HFrEF_rxs$DAYS_SUP) 
MktClarity_HFrEF_rxs$FILL_DT2 <- MktClarity_HFrEF_rxs$FILL_DT + MktClarity_HFrEF_rxs$DAYS_SUP
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% select(-NDC)
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% select(-DAYS_SUP)
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% distinct() %>% select(1,4,3,2,5) %>% arrange(PTID, drug_class,GNRC_NM, FILL_DT, FILL_DT2)
MktClarity_HFrEF_rxs$FROM_n <- as.numeric(MktClarity_HFrEF_rxs$FILL_DT)
MktClarity_HFrEF_rxs$TO_n <- as.numeric(MktClarity_HFrEF_rxs$FILL_DT2)
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% group_by(PTID, drug_class, GNRC_NM) %>% mutate(TO_n = ifelse(TO_n-FROM_n<62, TO_n+62-(TO_n-FROM_n), TO_n  ))
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% group_by(PTID, drug_class, GNRC_NM) %>% mutate(FROM_n =ifelse(lag(TO_n)>FROM_n,lag(TO_n), FROM_n))
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% group_by(PTID, drug_class, GNRC_NM) %>% mutate(Elapsed=FROM_n-lag(TO_n))
MktClarity_HFrEF_rxs$Elapsed[is.na(MktClarity_HFrEF_rxs$Elapsed)] <- 0
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% mutate(FROM_n=ifelse(is.na(FROM_n), as.numeric(FILL_DT), FROM_n))
MktClarity_HFrEF_rxs$FILL_DT  <- as.Date(MktClarity_HFrEF_rxs$FROM_n, origin = "1970-01-01")
MktClarity_HFrEF_rxs$FILL_DT2 <- as.Date(MktClarity_HFrEF_rxs$TO_n, origin = "1970-01-01")
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% mutate(FILL_DT2=ifelse(FILL_DT2<FILL_DT, FILL_DT, FILL_DT2))
MktClarity_HFrEF_rxs$FILL_DT2 <- as.Date(MktClarity_HFrEF_rxs$FILL_DT2, origin = "1970-01-01")

MktClarity_HFrEF_rxs_12m <- MktClarity_HFrEF_rxs %>% filter(FILL_DT2>="2021-08-01" & FILL_DT<="2022-07-31")
MktClarity_HFrEF_rxs_12m <- MktClarity_HFrEF_rxs_12m %>% ungroup() %>% select(PTID, drug_class) %>% distinct()
length(unique(MktClarity_HFrEF_rxs_12m$PTID))

MktClarity_HFrEF_rxs_12m %>% mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB", "RAAS", drug_class)) %>%
  distinct() %>% group_by(drug_class) %>% count() %>% mutate(n=n/79288)

MktClarity_HFrEF_rxs_12m %>% filter(drug_class=="ARNI"|drug_class=="SGLT2"|drug_class=="MRA"|drug_class=="Beta Blocker") %>%
  group_by(PTID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/79288)



Systolic <- fread("HFrEF_Systolic_DemandStudy.txt")
length(unique(Systolic$PTID)) # 112580
MktClarity_HF_cepts_labs <- fread("MktClarity_HF_cepts_labs.txt", colClasses = "character")
MktClarity_HF_cepts_labs <- MktClarity_HF_cepts_labs %>% select(-c(SOURCE, chunck_id))

Heart_Failure_Demand_Study_Attributes <- read_xlsx(path="Heart_Failure_Demand_Study_Attributes.xlsx",sheet = "HF LOINC", skip=0, col_types = "text", trim_ws = TRUE)
names(Heart_Failure_Demand_Study_Attributes)[1] <- "LOINC_CD"

MktClarity_HF_cepts_labs <- MktClarity_HF_cepts_labs %>% left_join(Heart_Failure_Demand_Study_Attributes %>% select(-LONG_COMMON_NAME)) %>%
  select(-LOINC_CD)

MktClarity_HF_cepts_labs$RSLT_NBR <- as.numeric(MktClarity_HF_cepts_labs$RSLT_NBR)

unique(MktClarity_HF_cepts_labs$TEST)

SBP <- MktClarity_HF_cepts_labs %>% filter(TEST=="SBP") %>%
  filter(RSLT_NBR>=40&RSLT_NBR<=300)

SBP <- SBP %>% group_by(PTID) %>% mutate(FST_DT=as.Date(FST_DT)) %>% arrange(FST_DT, RSLT_NBR) %>%
                                    filter(FST_DT==max(FST_DT)) %>% slice(1) %>%
  select(PTID, RSLT_NBR) %>% ungroup()

SBP %>% mutate(RSLT_NBR=ifelse(RSLT_NBR<105, "<105", 
                               ifelse(RSLT_NBR<140, "105-140", ">140"))) %>%
  group_by(RSLT_NBR) %>% count()



Potassium <- MktClarity_HF_cepts_labs %>% filter(TEST=="Potassium") %>%
  filter(RSLT_NBR>=1&RSLT_NBR<=10)

Potassium <- Potassium %>% group_by(PTID) %>% mutate(FST_DT=as.Date(FST_DT)) %>% arrange(FST_DT, desc(RSLT_NBR)) %>%
                                    filter(FST_DT==max(FST_DT)) %>% slice(1) %>%
  select(PTID, RSLT_NBR) %>% ungroup()

Potassium %>% mutate(RSLT_NBR=ifelse(RSLT_NBR<3.5, "<3.5", 
                               ifelse(RSLT_NBR<5, "3.5-5", ">5"))) %>%
  group_by(RSLT_NBR) %>% count()




BNP <- MktClarity_HF_cepts_labs %>% filter(TEST=="BNP") 
mean(BNP$RSLT_NBR)

BNP <- BNP %>% group_by(PTID) %>% mutate(FST_DT=as.Date(FST_DT)) %>% arrange(FST_DT, desc(RSLT_NBR)) %>%
                                    filter(FST_DT==max(FST_DT)) %>% slice(1) %>%
  select(PTID, RSLT_NBR) %>% ungroup()

BNP %>% mutate(RSLT_NBR=ifelse(RSLT_NBR<100, "<100", 
                               ifelse(RSLT_NBR<400, "100-400", ">400"))) %>%
  group_by(RSLT_NBR) %>% count()





GFR <- MktClarity_HF_cepts_labs %>% filter(TEST=="GFR") 
mean(GFR$RSLT_NBR)

GFR <- GFR %>% group_by(PTID) %>% mutate(FST_DT=as.Date(FST_DT)) %>% arrange(FST_DT, desc(RSLT_NBR)) %>%
                                    filter(FST_DT==max(FST_DT)) %>% slice(1) %>%
  select(PTID, RSLT_NBR) %>% ungroup()



GFR %>% mutate(RSLT_NBR=ifelse(RSLT_NBR<20, "<20", 
                               ifelse(RSLT_NBR<30, "20-30", 
                                      ifelse(RSLT_NBR<60, "30-60", ">60")))) %>%
  group_by(RSLT_NBR) %>% count()


# -----------------------------------------------------
# No. months ON SGLT2/Entresto from possible treatment-months last year  -------------------------

HF_Ingredients <- fread("HF Ingredients.txt", integer64 = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients$Drugs <- as.numeric(HF_Ingredients$Drugs)

HF_Drug_Histories <- read.table("HF Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)
HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-Drugs) %>% distinct()
HF_Drug_Histories <- HF_Drug_Histories %>% filter(drug_class=="SGLT2") %>% select(-drug_class)

HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% filter(Month>=49) %>%
  rename("First"="Month") %>%
  inner_join(HF_Drug_Histories)

HF_Drug_Histories$Total_Duration <- 60 - HF_Drug_Histories$First + 1

HF_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 
Total_Duration <- HF_Drug_Histories %>% select(patient, weight, Total_Duration) %>% distinct()
SGLT2_Duration <- HF_Drug_Histories %>% group_by(patient, weight) %>% count()

Total_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*Total_Duration))  # 1202439
SGLT2_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*n))  # 921737 # 0.7665561


HF_Ingredients <- fread("HF Ingredients.txt", integer64 = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, drug_class)
names(HF_Ingredients)[1] <- "Drugs"
HF_Ingredients$Drugs <- as.numeric(HF_Ingredients$Drugs)

HF_Drug_Histories <- read.table("HF Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
HF_Drug_Histories <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)
HF_Drug_Histories <- HF_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
HF_Drug_Histories <- separate_rows(HF_Drug_Histories, Drugs, sep = ",", convert=T)
HF_Drug_Histories <- HF_Drug_Histories %>% left_join(HF_Ingredients)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-Drugs) %>% distinct()
HF_Drug_Histories <- HF_Drug_Histories %>% filter(drug_class=="ARNI") %>% select(-drug_class)

HF_Drug_Histories <- HF_Drug_Histories %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% filter(Month>=49) %>%
  rename("First"="Month") %>%
  inner_join(HF_Drug_Histories)

HF_Drug_Histories$Total_Duration <- 60 - HF_Drug_Histories$First + 1

HF_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 

Total_Duration <- HF_Drug_Histories %>% select(patient, weight, Total_Duration) %>% distinct()
ARNI_Duration <- HF_Drug_Histories %>% group_by(patient, weight) %>% count()

Total_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*Total_Duration))  # 994485
ARNI_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*n))  # 754775 # 0.7589607






CKD_Ingredients <- fread("CKD Ingredients.txt", integer64 = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, drug_class)
names(CKD_Ingredients)[1] <- "Drugs"
CKD_Ingredients$Drugs <- as.numeric(CKD_Ingredients$Drugs)

CKD_Drug_Histories <- read.table("CKD Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
CKD_Drug_Histories <- separate_rows(CKD_Drug_Histories, Drugs, sep = ",", convert=T)
CKD_Drug_Histories <- CKD_Drug_Histories %>% left_join(CKD_Ingredients)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(-Drugs) %>% distinct()
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(drug_class=="SGLT2") %>% select(-drug_class)

CKD_Drug_Histories <- CKD_Drug_Histories %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% filter(Month>=49) %>%
  rename("First"="Month") %>%
  inner_join(CKD_Drug_Histories)

CKD_Drug_Histories$Total_Duration <- 60 - CKD_Drug_Histories$First + 1

CKD_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 
Total_Duration <- CKD_Drug_Histories %>% select(patient, weight, Total_Duration) %>% distinct()
SGLT2_Duration <- CKD_Drug_Histories %>% group_by(patient, weight) %>% count()

Total_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*Total_Duration))  # 1473825
SGLT2_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*n))  # 1119145 0.7593473





CKD_Ingredients <- fread("CKD Ingredients.txt", integer64 = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, drug_class)
names(CKD_Ingredients)[1] <- "Drugs"
CKD_Ingredients$Drugs <- as.numeric(CKD_Ingredients$Drugs)

CKD_Drug_Histories <- read.table("CKD Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs != "-") %>% select(-c(disease))
CKD_Drug_Histories <- separate_rows(CKD_Drug_Histories, Drugs, sep = ",", convert=T)
CKD_Drug_Histories <- CKD_Drug_Histories %>% left_join(CKD_Ingredients)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(-Drugs) %>% distinct()
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(drug_class=="ARNI") %>% select(-drug_class)

CKD_Drug_Histories <- CKD_Drug_Histories %>% group_by(patient, weight) %>% filter(Month==min(Month)) %>% filter(Month>=49) %>%
  rename("First"="Month") %>%
  inner_join(CKD_Drug_Histories)

CKD_Drug_Histories$Total_Duration <- 60 - CKD_Drug_Histories$First + 1

CKD_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 
Total_Duration <- CKD_Drug_Histories %>% select(patient, weight, Total_Duration) %>% distinct()
ARNI_Duration <- CKD_Drug_Histories %>% group_by(patient, weight) %>% count()

Total_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*Total_Duration))  # 470081
ARNI_Duration %>% ungroup() %>% summarise(n=sum(as.numeric(weight)*n))  # 333866



# -----------------------------------------


# Heart Failure Demand Study Patient Attributes - NEW REQUEST --------------
First_Systolic <- fread("First_Systolic_All_L5y.txt")
First_Systolic$group <- "Systolic"

HF_Demographics <- fread("HF Demographics.txt")
names(HF_Demographics)[1] <- "patient"

First_Systolic <- First_Systolic %>% left_join(HF_Demographics %>% select(patient, age, gender, weight)) %>% select(-c(earliest, group))

Stages <- fread("Predicted_Stages_gbm_All.txt")
Stages <- Stages %>% filter(Predicted.Stage!=1)

First_Systolic <- First_Systolic %>% inner_join(Stages %>% select(patient))

sum(First_Systolic$weight)

First_Systolic %>% group_by(gender) %>% summarise(n=sum(weight)/1457267)


First_Systolic %>%
  mutate(age=ifelse(age<=40, "<40", ifelse(age<=50, "<50", ifelse(age<=65, "<65", ifelse(age<=75, "<75", ">76"))))) %>%
  group_by(age) %>% summarise(n=sum(weight)/1457267)

Stages <- fread("Predicted_Stages_gbm_All.txt", colClasses = "character", sep="\t")
unique(Stages$Predicted.Stage)

First_Systolic %>% left_join(Stages) %>%
  group_by(Predicted.Stage) %>% summarise(n=sum(weight)/1457267)


HF_Comorbidity_Inventories <- fread("HF Comorbidity Inventories.txt", colClasses = "character")
names(HF_Comorbidity_Inventories)[1] <- "patient"
HF_Comorbidity_Inventories <- First_Systolic %>% select(patient) %>% inner_join(HF_Comorbidity_Inventories)

HF_Comorbidity_Inventories %>% filter(grepl("I10", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)
HF_Comorbidity_Inventories %>% filter(grepl("N18", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)
HF_Comorbidity_Inventories %>% filter(grepl("E66", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)
HF_Comorbidity_Inventories %>% filter(grepl("E78", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)
HF_Comorbidity_Inventories %>% filter(grepl("I48", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)
HF_Comorbidity_Inventories %>% filter(grepl("R64", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)
HF_Comorbidity_Inventories %>% filter(grepl("R63", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)

HF_Comorbidity_Inventories %>% filter(grepl("I20", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)
HF_Comorbidity_Inventories %>% filter(grepl("I95", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)
HF_Comorbidity_Inventories %>% filter(grepl("E875", diagnosis)) %>% summarise(n=sum(as.numeric(weight))/1457267)


DANU_Demographics <- fread("DANU Demographics Full.txt")
DANU_Demographics <- DANU_Demographics %>% select(patid, weight, diagnosis) %>% filter(grepl("Diabetes", diagnosis)|grepl("Obesity", diagnosis))
names(DANU_Demographics)[1] <- "patient"

First_Systolic %>% left_join(DANU_Demographics) %>% group_by(diagnosis) %>% summarise(n=sum(weight)/1457267)




DANU_Utilizations_Full <- fread("DANU Utilizations Full.txt",  colClasses = "character", stringsAsFactors = F)
DANU_Utilizations_Full <- DANU_Utilizations_Full %>% select(patid, hospital_stays) %>% rename("patient"="patid")

First_Systolic  %>% left_join(DANU_Utilizations_Full)   %>% 
  mutate(hospital_stays=as.numeric(hospital_stays)) %>% 
  mutate(hospital_stays=ifelse(hospital_stays>=5,5,hospital_stays)) %>% 
  group_by(hospital_stays) %>% summarise(n=sum(weight)/ (5*1457267))



HF_Drug_Histories <- fread("HF Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
HF_Drug_Histories <- HF_Drug_Histories %>% select(-c(disease))
HF_Drug_Histories <- First_Systolic %>% left_join(HF_Drug_Histories)
HF_Drug_Histories     <- gather(HF_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HF_Drug_Histories$Month <- as.character(HF_Drug_Histories$Month)
HF_Drug_Histories$Month <- parse_number(HF_Drug_Histories$Month)

HF_Ingredients <- fread("HF Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
HF_Ingredients <- HF_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
HF_Ingredients <- HF_Ingredients %>% select(molecule, generic_name, drug_class, drug_group)
unique(HF_Ingredients$drug_class)
HF_Ingredients$molecule <- as.numeric(HF_Ingredients$molecule)
names(HF_Ingredients)[1] <- "Drugs"
unique(HF_Ingredients$drug_class)

string_IV_Diuretics <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Diuretic"&HF_Ingredients$drug_group=="Injectable Therapy"], collapse = "|"),")\\b")
string_Hospital <- paste0("\\b(",paste0(HF_Ingredients$Drugs[HF_Ingredients$drug_class=="Hospital Inpatient"], collapse = "|"),")\\b")
# 95 Ivabradine
# 92 Vericiguat


HF_Drug_Histories %>% filter(Month>=49) %>% filter(grepl(string_IV_Diuretics, Drugs)) %>% group_by(patient, weight) %>% count() %>%
  mutate(n=ifelse(n>=5,5,n)) %>% group_by(n) %>% summarise(n2=sum(weight)/1457267)

HF_Drug_Histories %>% filter(Month>=49) %>% filter(grepl(string_Hospital, Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n2=sum(weight))


HF_Drug_Histories %>% filter(Month>=49) %>% arrange(patient, Month) %>%filter( (grepl(string_Hospital, Drugs)&grepl("95", Drugs))|
                                                      (grepl(string_Hospital, Drugs)&grepl("95", lag(Drugs)))|
                                                      (grepl(string_Hospital, Drugs)&grepl("95", lead(Drugs)))|
                                                      (grepl("95", Drugs)&grepl(string_Hospital, Drugs))|
                                                      (grepl("95", Drugs)&grepl(string_Hospital, lag(Drugs)))|
                                                      (grepl("95", Drugs)&grepl(string_Hospital, lead(Drugs)))) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n2=sum(weight))


HF_Drug_Histories %>% filter(Month>=49) %>% filter(grepl(string_IV_Diuretics, Drugs)) %>%  filter(grepl(string_Hospital, Drugs)) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n2=sum(weight))


DANU_Measures <- fread("DANU Measures Full.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="LVEF Fraction")
DANU_Measures <- DANU_Measures %>% select(-c(source, description, vague_value, vague_date, metric))
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- First_Systolic %>% inner_join(DANU_Measures)


length(unique(DANU_Measures$patient))
DANU_Measures %>% group_by(patient) %>% filter(value<50) %>%
  filter(value==min(value)) %>% slice(1) %>%
  mutate(value=ifelse(value<=29,"<=29", ifelse(value<=39, "<=39", "40+"))) %>%
  group_by(value) %>% summarise(n=sum(weight))



DANU_Measures <- fread("DANU Measures Full.txt")
DANU_Measures <- DANU_Measures %>% filter(test=="BMI")
DANU_Measures <- DANU_Measures %>% select(-c(source, description, vague_value, vague_date, metric))
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- First_Systolic %>% inner_join(DANU_Measures)

DANU_Measures %>% mutate(claimed=as.Date(claimed)) %>% group_by(patient) %>% 
  filter(claimed==max(claimed)) %>% filter(value==max(value)) %>% slice(1) %>%
  mutate(value=ifelse(value<20, "<20", 
                      ifelse(value<25, "<25",
                             ifelse(value<27, "<27", 
                                    ifelse(value<30, "<30",
                                           ifelse(value<35, "<35", 
                                                  ifelse(value<40, "<40", ">40" ))))))) %>% group_by(value) %>% count() %>% mutate(n=n/6710)


DANU_Measures <- DANU_Measures %>% mutate(claimed=as.Date(claimed))
range(DANU_Measures$claimed)

DANU_Measures <- DANU_Measures %>% filter(claimed>="2020-05-01")

DANU_Measures <- DANU_Measures %>% select(patient, weight, claimed, value)

DANU_Measures <- DANU_Measures %>% left_join(DANU_Measures %>% rename("claimed2"="claimed", "value2"="value"))
DANU_Measures <- DANU_Measures %>% filter(claimed2>claimed)

DANU_Measures  %>% filter(claimed>="2020-11-01") %>%
  mutate(Diff=100*(value2-value)/value) %>% group_by(patient) %>% filter(Diff==min(Diff)) %>% slice(1) %>%
  mutate(Diff=ifelse(Diff<(-10), "<10",
                     ifelse(Diff<(-5), "<5", 
                            ifelse(Diff<0, "5", "no")))) %>% group_by(Diff) %>% count() %>% mutate(n=n/1555)

#   Diff      n
# 1 5     0.398
# 2 <10   0.191
# 3 <5    0.188
# 4 no    0.223


DANU_Measures  %>% filter(claimed>="2020-11-01") %>%
  mutate(Diff=100*(value2-value)/value) %>% group_by(patient) %>% filter(Diff==min(Diff)) %>% slice(1) %>%
  mutate(Diff=ifelse(Diff<(-10), "<10",
                     ifelse(Diff<(-5), "<5", 
                            ifelse(Diff<0, "5", "no")))) %>% group_by(Diff) %>% summarise(n=weighted.mean(value2, weight))


# NEW DATA


MktClarity_HF_cepts_Age_Gender <- fread("MktClarity_HF_cepts_Age&Gender.txt")
names(MktClarity_HF_cepts_Age_Gender)[2] <- "age"
names(MktClarity_HF_cepts_Age_Gender)[3] <- "gender"
MktClarity_HF_cepts_Age_Gender <- MktClarity_HF_cepts_Age_Gender %>% mutate(gender=ifelse(gender=="Male", "M", "F"))

DANU_Demographics <- fread("DANU Demographics.txt")
DANU_Demographics <- DANU_Demographics %>% select(age, gender, weight) %>% distinct()
data.frame(DANU_Demographics %>% arrange(gender, age))
range(DANU_Demographics$age)

unique(MktClarity_HF_cepts_Age_Gender$age)
MktClarity_HF_cepts_Age_Gender %>% mutate(age=as.numeric(age)) %>% left_join(DANU_Demographics) %>% summarise(n=sum(weight, na.rm = T))
MktClarity_HF_cepts_Age_Gender <- MktClarity_HF_cepts_Age_Gender %>% mutate(age=as.numeric(age)) %>% filter(age>=18)


MktClarity_HF_cepts_HFdxs <- fread("MktClarity_HF_cepts_HFdxs.txt")
MktClarity_HF_cepts_HFdxs <- MktClarity_HF_cepts_HFdxs %>% mutate(Systolic=ifelse(grepl("I502", DIAG)|grepl("I504", DIAG), 1,0)) %>%
  mutate(Diastolic=ifelse(grepl("I503", DIAG)|grepl("I504", DIAG), 1,0)) 


Systolic <- MktClarity_HF_cepts_HFdxs %>% filter(Systolic==1) %>% group_by(PTID) %>% count() %>% filter(n>1) %>% select(PTID) %>% distinct() %>% mutate(Systolic="Systolic")
Diastolic <- MktClarity_HF_cepts_HFdxs %>% filter(Diastolic==1) %>% group_by(PTID) %>% count() %>% filter(n>1) %>% select(PTID) %>% distinct() %>% mutate(Diastolic="Diastolic")

Groups <- Systolic %>% full_join(Diastolic)

Groups %>% group_by(Systolic, Diastolic) %>% count()

Systolic <- Groups %>% filter(Systolic=="Systolic"&is.na(Diastolic)) %>% select(PTID)


fwrite(Systolic, "HFrEF_Systolic_DemandStudy.txt", sep="\t")

Systolic <- fread("HFrEF_Systolic_DemandStudy.txt", sep="\t")




MktClarity_HFrEF_rxs <- fread("MktClarity_HFrEF_rxs.txt", colClasses = "character")
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% filter(TRANSACTION_TYPE=="PAID") %>% select(-c(TRANSACTION_TYPE, chunk_id, STRENGTH, QUANTITY,  BRND_NM, PROV_UNIQUE, PROV_SPECIALTY))
length(unique(MktClarity_HFrEF_rxs$PTID)) # 98169

Heart_Failure_Demand_Study_Attributes <- read_xlsx(path="Heart_Failure_Demand_Study_Attributes.xlsx",sheet = "HF Rxs All", skip=0, col_types = "text", trim_ws = TRUE)
Heart_Failure_Demand_Study_Attributes <- Heart_Failure_Demand_Study_Attributes %>% select(med_code, drug_class)
Heart_Failure_Demand_Study_Attributes <- Heart_Failure_Demand_Study_Attributes %>% mutate(med_code=str_sub(med_code, 3L, 13L))
names(Heart_Failure_Demand_Study_Attributes)[1] <- "NDC"

MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% left_join(Heart_Failure_Demand_Study_Attributes)
range(MktClarity_HFrEF_rxs$FILL_DT) # "2016-07-30" "2022-09-30"

MktClarity_HFrEF_rxs$FILL_DT <- as.Date(MktClarity_HFrEF_rxs$FILL_DT) 
MktClarity_HFrEF_rxs$DAYS_SUP <- as.numeric(MktClarity_HFrEF_rxs$DAYS_SUP) 
MktClarity_HFrEF_rxs$FILL_DT2 <- MktClarity_HFrEF_rxs$FILL_DT + MktClarity_HFrEF_rxs$DAYS_SUP
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% select(-NDC)
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% select(-DAYS_SUP)
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% distinct() %>% select(1,4,3,2,5) %>% arrange(PTID, drug_class,GNRC_NM, FILL_DT, FILL_DT2)
MktClarity_HFrEF_rxs$FROM_n <- as.numeric(MktClarity_HFrEF_rxs$FILL_DT)
MktClarity_HFrEF_rxs$TO_n <- as.numeric(MktClarity_HFrEF_rxs$FILL_DT2)
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% group_by(PTID, drug_class, GNRC_NM) %>% mutate(TO_n = ifelse(TO_n-FROM_n<62, TO_n+62-(TO_n-FROM_n), TO_n  ))
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% group_by(PTID, drug_class, GNRC_NM) %>% mutate(FROM_n =ifelse(lag(TO_n)>FROM_n,lag(TO_n), FROM_n))
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% group_by(PTID, drug_class, GNRC_NM) %>% mutate(Elapsed=FROM_n-lag(TO_n))
MktClarity_HFrEF_rxs$Elapsed[is.na(MktClarity_HFrEF_rxs$Elapsed)] <- 0
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% mutate(FROM_n=ifelse(is.na(FROM_n), as.numeric(FILL_DT), FROM_n))
MktClarity_HFrEF_rxs$FILL_DT  <- as.Date(MktClarity_HFrEF_rxs$FROM_n, origin = "1970-01-01")
MktClarity_HFrEF_rxs$FILL_DT2 <- as.Date(MktClarity_HFrEF_rxs$TO_n, origin = "1970-01-01")
MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% mutate(FILL_DT2=ifelse(FILL_DT2<FILL_DT, FILL_DT, FILL_DT2))
MktClarity_HFrEF_rxs$FILL_DT2 <- as.Date(MktClarity_HFrEF_rxs$FILL_DT2, origin = "1970-01-01")

copy <- MktClarity_HFrEF_rxs
MktClarity_HFrEF_rxs <- copy

# MktClarity_HFrEF_rxs
# MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% select(PTID, drug_class, FILL_DT, FILL_DT2) %>% distinct()
# MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% ungroup() %>% select(PTID, drug_class, FILL_DT, FILL_DT2) %>% distinct()
# MktClarity_HFrEF_rxs
# MktClarity_HFrEF_rxs$FILL_DT <- as.character(MktClarity_HFrEF_rxs$FILL_DT)
# MktClarity_HFrEF_rxs$FILL_DT2 <- as.character(MktClarity_HFrEF_rxs$FILL_DT2)
# MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% mutate(FILL_DT=str_sub(FILL_DT, 1L, 7L))
# MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% mutate(FILL_DT2=str_sub(FILL_DT2, 1L, 7L))
# MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% select(-drug_class) %>% distinct() # filter(drug_class=="SGLT2")
# MktClarity_HFrEF_rxs <- gather(MktClarity_HFrEF_rxs, name, date, FILL_DT:FILL_DT2)
# MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% select(-name) %>% distinct()
# MktClarity_HFrEF_rxs
# MktClarity_HFrEF_rxs <- MktClarity_HFrEF_rxs %>% group_by(date) %>% count()
# MktClarity_HFrEF_rxs
# data.frame(MktClarity_HFrEF_rxs)


# MktClarity_HFrEF_rxs_12m <- MktClarity_HFrEF_rxs %>% filter(FILL_DT2>="2021-08-01" & FILL_DT<="2022-07-31")
# MktClarity_HFrEF_rxs_12m <- MktClarity_HFrEF_rxs_12m %>% ungroup() %>% select(PTID, drug_class) %>% distinct()
# length(unique(MktClarity_HFrEF_rxs_12m$PTID))
# 
# MktClarity_HFrEF_rxs_12m <- MktClarity_HFrEF_rxs_12m %>% filter(drug_class=="ARNI"|drug_class=="SGLT2"|drug_class=="MRA"|drug_class=="Beta Blocker") %>%
#   select(PTID) %>% distinct() %>% left_join(MktClarity_HFrEF_rxs_12m)
# 
# length(unique(MktClarity_HFrEF_rxs_12m$PTID))
# 
# MktClarity_HFrEF_rxs_12m %>% filter(drug_class=="ARNI"|drug_class=="SGLT2"|drug_class=="MRA"|drug_class=="Beta Blocker") %>%
#   group_by(PTID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/66942)
# 
# 
# MktClarity_HFrEF_rxs_12m %>% filter(drug_class=="ARNI"|drug_class=="SGLT2"|drug_class=="MRA"|drug_class=="Beta Blocker") %>%
#   select(PTID) %>% distinct() %>% left_join(MktClarity_HFrEF_rxs) %>%
#    select(PTID, drug_class) %>% distinct() %>%
#  mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB", "RAAS", drug_class)) %>%
#   distinct() %>% group_by(drug_class) %>% count() %>% mutate(n=n/66942)
# 
# 
# unique(MktClarity_HFrEF_rxs_12m$drug_class)
# 
# MktClarity_HFrEF_rxs_12m %>% filter(drug_class=="Inotropic") %>%
#   group_by(PTID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/66942)


MktClarity_HFrEF_rxs_current <- MktClarity_HFrEF_rxs %>% mutate(FILL_DT2=as.character(FILL_DT2)) %>% filter(grepl("2022-06",FILL_DT2)|grepl("2022-04",FILL_DT2)|grepl("2022-05",FILL_DT2))
MktClarity_HFrEF_rxs_current <- MktClarity_HFrEF_rxs_current %>% ungroup() %>% select(PTID, drug_class) %>% distinct()

MktClarity_HFrEF_rxs_current <- MktClarity_HFrEF_rxs_current %>% filter(drug_class=="ARNI"|drug_class=="SGLT2"|drug_class=="MRA"|drug_class=="Beta Blocker"|drug_class=="ACE"|drug_class=="ARB") %>%
  select(PTID) %>% distinct() %>% left_join(MktClarity_HFrEF_rxs_current)

length(unique(MktClarity_HFrEF_rxs_current$PTID)) # 36635
 
MktClarity_HFrEF_rxs_current %>%  mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB", "RAAS", drug_class)) %>% 
  filter(drug_class=="ARNI"|drug_class=="SGLT2"|drug_class=="MRA"|drug_class=="Beta Blocker"|drug_class=="RAAS") %>%
  group_by(PTID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/59028)


MktClarity_HFrEF_rxs_current %>%  mutate(drug_class=ifelse(drug_class=="ACE"|drug_class=="ARB", "RAAS", drug_class)) %>% 
  filter(drug_class=="Beta Blocker") %>% select(PTID) %>% distinct() %>%
  group_by(PTID) %>% count() %>% ungroup() %>% group_by(n) %>% count() %>% mutate(nn=nn/59028)



Systolic <- fread("HFrEF_Systolic_DemandStudy.txt")
length(unique(Systolic$PTID)) # 112580


MktClarity_HF_cepts_labs <- fread("MktClarity_HF_cepts_labs.txt", colClasses = "character")
MktClarity_HF_cepts_labs <- MktClarity_HF_cepts_labs %>% select(-c(SOURCE, chunck_id))


MktClarity_HF_cepts_labs <- MktClarity_HFrEF_rxs_12m %>% filter(drug_class=="ARNI"|drug_class=="SGLT2"|drug_class=="MRA"|drug_class=="Beta Blocker") %>%
  select(PTID) %>% distinct() %>% inner_join(MktClarity_HF_cepts_labs)


Heart_Failure_Demand_Study_Attributes <- read_xlsx(path="Heart_Failure_Demand_Study_Attributes.xlsx",sheet = "HF LOINC", skip=0, col_types = "text", trim_ws = TRUE)
names(Heart_Failure_Demand_Study_Attributes)[1] <- "LOINC_CD"

MktClarity_HF_cepts_labs <- MktClarity_HF_cepts_labs %>% left_join(Heart_Failure_Demand_Study_Attributes %>% select(-LONG_COMMON_NAME)) %>%
  select(-LOINC_CD)

MktClarity_HF_cepts_labs$RSLT_NBR <- as.numeric(MktClarity_HF_cepts_labs$RSLT_NBR)


length(unique(MktClarity_HF_cepts_labs$PTID))

unique(MktClarity_HF_cepts_labs$TEST)

SBP <- MktClarity_HF_cepts_labs %>% filter(TEST=="SBP") %>%
  filter(RSLT_NBR>=40&RSLT_NBR<=300)

SBP <- SBP %>% group_by(PTID) %>% mutate(FST_DT=as.Date(FST_DT)) %>% arrange(FST_DT, RSLT_NBR) %>%
                                    filter(FST_DT==max(FST_DT)) %>% slice(1) %>%
  select(PTID, RSLT_NBR) %>% ungroup()

SBP %>% mutate(RSLT_NBR=ifelse(RSLT_NBR<105, "<105", 
                               ifelse(RSLT_NBR<140, "105-140", ">140"))) %>%
  group_by(RSLT_NBR) %>% count() %>% mutate(n=n/14201)



Potassium <- MktClarity_HF_cepts_labs %>% filter(TEST=="Potassium") %>%
  filter(RSLT_NBR>=1&RSLT_NBR<=10)

Potassium <- Potassium %>% group_by(PTID) %>% mutate(FST_DT=as.Date(FST_DT)) %>% arrange(FST_DT, desc(RSLT_NBR)) %>%
                                    filter(FST_DT==max(FST_DT)) %>% slice(1) %>%
  select(PTID, RSLT_NBR) %>% ungroup()

Potassium %>% mutate(RSLT_NBR=ifelse(RSLT_NBR<3.5, "<3.5", 
                               ifelse(RSLT_NBR<5, "3.5-5", ">5"))) %>%
  group_by(RSLT_NBR) %>% count() %>% mutate(n=n/14201)




BNP <- MktClarity_HF_cepts_labs %>% filter(TEST=="BNP") 
mean(BNP$RSLT_NBR)

BNP <- BNP %>% group_by(PTID) %>% mutate(FST_DT=as.Date(FST_DT)) %>% arrange(FST_DT, desc(RSLT_NBR)) %>%
                                    filter(FST_DT==max(FST_DT)) %>% slice(1) %>%
  select(PTID, RSLT_NBR) %>% ungroup()

BNP %>% mutate(RSLT_NBR=ifelse(RSLT_NBR<100, "<100", 
                               ifelse(RSLT_NBR<400, "100-400", ">400"))) %>%
  group_by(RSLT_NBR) %>% count() %>% mutate(n=n/14201)






GFR <- MktClarity_HF_cepts_labs %>% filter(TEST=="GFR") 
mean(GFR$RSLT_NBR)

GFR <- GFR %>% group_by(PTID) %>% mutate(FST_DT=as.Date(FST_DT)) %>% arrange(FST_DT, desc(RSLT_NBR)) %>%
                                    filter(FST_DT==max(FST_DT)) %>% slice(1) %>%
  select(PTID, RSLT_NBR) %>% ungroup()



GFR %>% mutate(RSLT_NBR=ifelse(RSLT_NBR<15, "<15", 
                               ifelse(RSLT_NBR<30, "15-30", 
                                      ifelse(RSLT_NBR<60, "30-60",
                                             ifelse(RSLT_NBR<90, "60-90",">90"))))) %>%
  group_by(RSLT_NBR) %>% count() %>% mutate(n=n/14201)


# ----------------------------
