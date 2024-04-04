
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library("readxl")

options(scipen = 999)

# % With Oral/Inj CGRP or Nasal/triptan Spray -----------------------

ZAVUS24_Migraine_Dxs <- fread("Source/ZAVUS24 Migraine Dxs.txt")
ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")
RIME_MEDICATIONS <- fread("Source/RIME Medications.txt")

ZAVUS24_Doses <- ZAVUS24_Doses %>% select(code, generic, drug_class, patid, from_dt, days_sup) %>% distinct()
length(unique(ZAVUS24_Doses$patid)) # 260

ZAVUS24_Doses <- ZAVUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(generic=="Zavegepant") %>% group_by(patid) %>% filter(from_dt==max(from_dt)) %>%
  rename("last_zav"="from_dt") %>% select(patid, last_zav) %>% distinct() %>%
  left_join(ZAVUS24_Doses  %>% mutate(from_dt=as.Date(from_dt))) %>%
  filter( (from_dt<=last_zav & from_dt+days_sup>=last_zav) | (from_dt>=last_zav & last_zav+days_sup>=from_dt) ) %>%
  select(patid, drug_class, code) %>% distinct() 

length(unique(ZAVUS24_Doses$patid[ZAVUS24_Doses$drug_class=="CGRP Oral"])) # 107
length(unique(ZAVUS24_Doses$patid[ZAVUS24_Doses$drug_class=="CGRP Injectable"|ZAVUS24_Doses$drug_class=="CGRP Oral"])) # 178

RIME_MEDICATIONS <- RIME_MEDICATIONS %>% filter(med_route=="NASAL") %>% select(drug_class, med_code, drug_id, generic_name)
RIME_MEDICATIONS$med_code <- substr(RIME_MEDICATIONS$med_code, start = 3, stop = nchar(RIME_MEDICATIONS$med_code))
RIME_MEDICATIONS <- RIME_MEDICATIONS %>% select(-drug_id)
ZAVUS24_Doses <- RIME_MEDICATIONS %>% select(med_code) %>% inner_join(ZAVUS24_Doses, by=c("med_code"="code"))
names(ZAVUS24_Doses)
unique(ZAVUS24_Doses$drug_class)

length(unique(ZAVUS24_Doses$patid))
length(unique(ZAVUS24_Doses$patid)) # Any 3 nasal spray

length(unique(ZAVUS24_Doses$patid[ZAVUS24_Doses$drug_class=="Triptan"])) # 1 triptan nasal spray



# -------------------------------

# % Classes ever tried (2 years visibility) --------------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")

ZAVUS24_Demographics <- ZAVUS24_Demographics %>% mutate(visib_to_zav=as.numeric(first_Zav_Rx-fst_enr_dd)) %>% 
  filter(visib_to_zav>730.5) %>% select(patid, CV, psychiatric, epileptic) 

length(unique(ZAVUS24_Demographics$patid)) # 191

ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, generic, drug_class)  %>% distinct()
ZAVUS24_Doses <- ZAVUS24_Doses %>% inner_join(ZAVUS24_Demographics)
unique(ZAVUS24_Doses$drug_class)

ZAVUS24_Doses <- ZAVUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

unique(ZAVUS24_Doses$flag)
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(flag==0)

data.frame(ZAVUS24_Doses %>% select(patid, drug_class) %>% distinct() %>% 
  group_by(drug_class) %>% count() %>% mutate(n=n/191) %>% arrange(-n))



# -----------





# Visibility ----------
ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")

ZAVUS24_Demographics %>% select(patid, fst_enr_dd) %>%
  mutate(fst_enr_dd = as.Date(fst_enr_dd)) %>%
  mutate(VIZ = as.numeric(as.Date("2024-01-15") - fst_enr_dd)/30.5) %>%
  summarise(mean=mean(VIZ), median=median(VIZ)) %>% # 75.68235 52.88525
  ggplot(aes(VIZ)) + 
  geom_density()


ZAVUS24_Demographics %>% select(patid, fst_enr_dd) %>%
  mutate(fst_enr_dd = as.Date(fst_enr_dd)) %>%
  mutate(VIZ = as.numeric(as.Date("2024-01-15") - fst_enr_dd)/30.5)  %>%
  arrange(VIZ) %>% group_by(VIZ) %>% count() %>% ungroup() %>%
  mutate(cumn=cumsum(n)) %>% mutate(cumn=cumn/260) %>%
  ggplot(aes(VIZ, 100*cumn)) +
  geom_line(size=2, colour="firebrick", alhpa=0.6) +
  theme_bw() +
  xlab("\n Number of Visibility Months \n From Enrolment to January 2024") +
  ylab("Cumulative patient share \n")

# -----------
# Scripts per specialty last 12 months -----------------------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16")
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(generic=="Zavegepant") %>% select(patid, weight, provcat)
PROVCAT <- read_excel("Source/CDM_Data_Reference_Lookup_Tables_V81.xlsx", sheet="PROVCAT", skip = 5)
PROVCAT <- PROVCAT %>% select(PROVCAT, DESCRIPTION)  %>% mutate(PROVCAT=as.numeric(PROVCAT))

ZAVUS24_Doses %>% select(provcat) %>% distinct() %>%
  inner_join(PROVCAT %>% select(PROVCAT) %>% distinct(), by=c("provcat"="PROVCAT"))

ZAVUS24_Doses <- PROVCAT %>% inner_join(ZAVUS24_Doses, by=c("PROVCAT"="provcat"))

data.frame(ZAVUS24_Doses %>% select(DESCRIPTION) %>% distinct())

ZAVUS24_Doses %>% mutate(DESCRIPTION=ifelse(DESCRIPTION %in% c("UNKNOWN PROVIDER TYPE", "URGENT CARE CENTER","HOSPICE AND PALLIATIVE MEDICINE"), "Remove",
                                            ifelse(DESCRIPTION %in% c("AUDIOLOGIST","PEDIATRIC SPECIALIST","ANESTHESIOLOGIST","PAIN MANAGEMENT SPECIALIST", "CARDIOLOGIST",
                                                                      "EMERGENCY MEDICINE (PHYSICIANS)", "GENERAL SURGEON", "NEURO-SURGEON", "CLINICAL NEUROPHYSIOLOGIST",
                                                                      "OTOLARYNGOLOGIST", "REHABILITATION MEDICINE SPECIALIST", "PSYCHIATRIST" ), "Other Physician",
                                                   ifelse(DESCRIPTION %in% c("FAMILY NURSE PRACTITIONER", "NURSE PRACTITIONER", "PHYSICIANS ASSISTANT", "REGISTERED NURSE", "OTHER NON-PHYSICIAN PROVIDER"), "Other HCP",
                                                          ifelse( DESCRIPTION %in% c("FAMILY PRACTITIONER","FAMILY PRACTICE SPECIALIST","FAMILY PRACTICE/GENERAL PRACTICE"), "PCP",
                                                                  ifelse(DESCRIPTION %in% c("INTERNAL MEDICINE SPECIALIST","INTERNIST/GENERAL INTERNIST", "HOSPITALIST") , "IM",
                                                                         ifelse(DESCRIPTION %in% c("NEUROLOGIST" ), "Neuro", NA ))))))) %>%
  filter(DESCRIPTION!="Remove") %>% group_by(DESCRIPTION) %>% summarise(n=sum(weight)/(21+293+105+56+19))


# ------------
# Age group breakdown ------------
ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")
ZAVUS24_Demographics <- ZAVUS24_Demographics %>% select(patid,age)
ZAVUS24_Demographics <- ZAVUS24_Demographics %>% mutate(age=ifelse(age<=29, 29,
                                                                   ifelse(age<=39,39,
                                                                          ifelse(age<=49,49,
                                                                                 ifelse(age<=59,59,
                                                                                        ifelse(age<=69,69,
                                                                                               ifelse(age<=79,79,80))))))) %>%
  group_by(age) %>% count() %>% mutate(n=n/260)

 
# RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
# RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid,AGE)
# #ModSev_Pats_V3 <- fread("ModSev_Pats_V3.txt")
# #RIMUS23_Demographics <- ModSev_Pats_V3 %>% filter(group=="ModSev") %>% select(patient) %>%   mutate(patient=as.numeric(patient)) %>%
# #  inner_join(RIMUS23_Demographics %>% mutate(patid=as.numeric(patid)), by=c("patient"="patid"))
# RIMUS23_Demographics %>% mutate(AGE=as.numeric(AGE)) %>% mutate(AGE=ifelse(AGE<=29, 29,
#                                                                    ifelse(AGE<=39,39,
#                                                                           ifelse(AGE<=49,49,
#                                                                                  ifelse(AGE<=59,59,
#                                                                                         ifelse(AGE<=69,69,
#                                                                                                ifelse(AGE<=79,79,80))))))) %>%
#   group_by(AGE) %>% count() %>% mutate(n=n/263848)


# ------
# Individual Molecule penetrantion ever ------------
ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, generic, drug_class, drug_group) %>% distinct()
ZAVUS24_Doses <- ZAVUS24_Doses %>% group_by(generic,drug_class, drug_group) %>% count() %>% mutate(n=n/260)
data.frame(ZAVUS24_Doses) %>% arrange(drug_group, desc(n))

# --------------
# Number of classes ever tried ----------



ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")
ZAVUS24_Demographics <- ZAVUS24_Demographics %>% mutate(visib_to_zav=as.numeric(first_Zav_Rx-fst_enr_dd)) %>% 
  filter(visib_to_zav>730.5) %>% select(patid) 

ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, drug_class, drug_group) %>% distinct() %>% inner_join(ZAVUS24_Demographics) 
length(unique(ZAVUS24_Doses$patid))

data.frame(ZAVUS24_Doses %>% group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n") %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/191,2)))

ZAVUS24_Doses %>% group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n") %>% summarise(mean=mean(classes))


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, drug_class, generic) %>% distinct() %>% inner_join(ZAVUS24_Demographics)

data.frame(ZAVUS24_Doses %>% filter(grepl("CGRP", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n") %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/191,2)))

#   classes    n
# 1       1 0.13
# 2       2 0.23
# 3       3 0.27
# 4       4 0.17
# 5       5 0.10
# 6       6 0.07
# 7       7 0.02
# 8       8 0.01


ZAVUS24_Doses %>% filter(grepl("CGRP", drug_class)) %>% 
  group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n") %>% summarise(mean=mean(classes)) # 3.2



# TRIPTAN

unique(ZAVUS24_Doses$drug_class)

data.frame(
ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("Triptan", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/191,2)))

#   classes    n
# 1       0 0.32
# 2       1 0.37
# 3       2 0.20
# 4       3 0.07
# 5       4 0.03

ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("Triptan", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>% summarise(mean=mean(classes)) # 1.1





# NSAID

unique(ZAVUS24_Doses$drug_class)

data.frame(
ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("NSAID", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/191,2)))

# 1       0 0.19
# 2       1 0.28
# 3       2 0.21
# 4       3 0.18
# 5       4 0.08
# 6       5 0.04
# 7       6 0.01
# 8       8 0.01

ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("NSAID", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>% summarise(mean=mean(classes)) # 1.9





# OPIOID

unique(ZAVUS24_Doses$drug_class)

data.frame(
ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("Opioid", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/191,2)))

# 1        0 0.26
# 2        1 0.16
# 3        2 0.18
# 4        3 0.12
# 5        4 0.10
# 6        5 0.08
# 7        6 0.03
# 8        7 0.04
# 9        8 0.03
# 10       9 0.01

ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("Opioid", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>% summarise(mean=mean(classes)) # 2.4

# ---------------

# # Visibility Months vs # Zavegepant refills ----------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(generic=="Zavegepant")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, from_dt)

ZAVUS24_Doses %>% group_by(patid) %>% count()


ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")

names(ZAVUS24_Demographics)
sum(is.na(ZAVUS24_Demographics$migraine_onset))

ZAVUS24_Demographics <-  ZAVUS24_Demographics %>% select(patid, migraine_onset) %>%
  mutate(migraine_onset = as.Date(migraine_onset)) %>%
  mutate(VIZ = as.numeric(as.Date("2024-01-15") - migraine_onset)/30.5)  %>% select(patid, VIZ)

ZAVUS24_Demographics <- ZAVUS24_Demographics %>% drop_na()

ZAVUS24_Demographics <- ZAVUS24_Demographics %>% inner_join(ZAVUS24_Doses %>% group_by(patid) %>% count())  %>%
  mutate(VIZ=round(VIZ)) %>% rename("SCRIPTS"="n")

max(ZAVUS24_Demographics$VIZ) # 74

ZAVUS24_Demographics$group <- as.numeric(cut(ZAVUS24_Demographics$VIZ,11))

ZAVUS24_Demographics %>% group_by(group) %>% count() 


ZAVUS24_Demographics %>%
  group_by(group, SCRIPTS) %>% count() %>%
  spread(key=group, value=n)


# From First ZAV

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(generic=="Zavegepant")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, from_dt)

ZAVUS24_Doses %>% group_by(patid) %>% count()
ZAVUS24_Doses <- ZAVUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% group_by(patid) %>% mutate(first=min(from_dt))

ZAVUS24_Doses <- ZAVUS24_Doses %>% mutate(VIZ = as.numeric(as.Date("2024-01-15") - first)/30.5)  

ZAVUS24_Doses <- ZAVUS24_Doses %>% ungroup() %>% mutate(patid=as.character(patid)) %>% filter(from_dt>=first)

ZAVUS24_Doses <- ZAVUS24_Doses %>% group_by(patid, VIZ) %>% count() %>% 
  mutate(VIZ=round(VIZ)) %>% rename("SCRIPTS"="n")

max(ZAVUS24_Doses$VIZ) # 6

ZAVUS24_Doses %>% group_by(VIZ) %>% count() 

ZAVUS24_Doses %>%
  group_by(VIZ, SCRIPTS) %>% count() %>%
  spread(key=VIZ, value=n)



# ----------
# Compare Zavegepant with other Migraine patients ----------------

ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")
mean(ZAVUS24_Demographics$age) # 52
ZAVUS24_Demographics %>% group_by(gender) %>% count() #87% F
ZAVUS24_Demographics %>% group_by(CV) %>% count() #52%
ZAVUS24_Demographics %>% group_by(epileptic) %>% count() #9%
ZAVUS24_Demographics %>% group_by(psychiatric) %>% count() #66%


RIMUS23_Demographics <- fread("RIMUS23 Demographics.txt")
RIMUS23_Demographics <- RIMUS23_Demographics %>% anti_join(ZAVUS24_Demographics %>% select(patid))
mean(RIMUS23_Demographics$AGE) # 60
RIMUS23_Demographics %>% group_by(GENDER) %>% count() #76% F
RIMUS23_Demographics %>% group_by(CV) %>% count() # 68%
RIMUS23_Demographics %>% group_by(epileptic) %>% count() # 6%
RIMUS23_Demographics %>% group_by(psychiatric) %>% count() # 63%

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% anti_join(ZAVUS24_Demographics %>% select(patid) %>% mutate(patid=as.character(patid)), by=c("patient"="patid"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-") %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, drug_class) %>% distinct()
All_drug_classes<- data.frame(RIMUS23_Drug_Histories %>% group_by(drug_class) %>% count() %>% mutate(n=n/length(unique(RIMUS23_Drug_Histories$patient))))

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, drug_class) %>% distinct()
ZAV_drug_classes <-ZAVUS24_Doses %>% group_by(drug_class) %>% count() %>% mutate(n=n/260) %>% rename("ZAV"="n")

data.frame(ZAV_drug_classes %>% full_join(All_drug_classes))

ZAVUS24_Migraine_Dxs <- fread("Source/ZAVUS24 Migraine Dxs.txt")
ZAVUS24_Migraine_Dxs <- ZAVUS24_Migraine_Dxs %>% select(patid, mig_diag) %>% distinct()
ZAV_Dxs <- ZAVUS24_Migraine_Dxs %>% group_by(mig_diag) %>% count() %>% mutate(n=n/260) %>% rename("ZAV"="n") %>% rename("ICD10_diag"="mig_diag")

RIMUS23_Comorbidities_Extended_Dxs <- fread("RIMUS23 Comorbidities Extended Dxs.txt")
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% anti_join(ZAVUS24_Demographics %>% select(patid))
length(unique(RIMUS23_Comorbidities_Extended_Dxs$patid)) # 262157
ALL_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% group_by(ICD10_diag) %>% count() %>% mutate(n=n/262157) %>% rename("ALL"="n")


data.frame(ZAV_Dxs %>% inner_join(ALL_Dxs))


# ----------~
# --------------
# Zavegepant  patients waterfall ON January 2024 --------------



ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)

string_CGRPs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"|Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")

ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, month60) 

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(grepl("134", month60))

dim(ZAVUS24_Drug_Histories)[1] # 138

ZAVUS24_Drug_Histories$Zavegepant <- 1

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(Combo=ifelse(grepl(",", month60),1,0))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(CGRPs=ifelse(grepl(string_CGRPs, month60),1,0))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(Acute=ifelse(grepl(string_Acute, month60),1,0))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(Symptomatic=ifelse(grepl(string_Symptomatic, month60),1,0))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(Preventive=ifelse(grepl(string_Preventive, month60),1,0))

ZAVUS24_Drug_Histories %>% 
  mutate(AcuteSympt=ifelse(Acute==1|Symptomatic==1,1,0)) %>%
  group_by(Zavegepant, Combo, CGRPs,  Preventive, AcuteSympt) %>%
  count()

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(CGRPs==1) %>% select(patient, month60) 
length(unique(ZAVUS24_Drug_Histories$patient)) # 95
ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, month60, sep = ",", convert=T )
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(grepl(string_CGRPs, month60))
 ZAVUS24_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic), by=c("month60"="drug_id")) %>%
   group_by(generic) %>% count() %>% mutate(n=n) %>% arrange(-n)


# -------------
# Zavegepant  patients LINES OF THERAPY --------------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)

string_CGRPs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"|Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")


ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

length(unique(ZAVUS24_Drug_Histories$patient))

All_Pats <- ZAVUS24_Drug_Histories %>% select(patient) %>% distinct()  %>% mutate(ZAV="ZAV")

CGRPs_Pats <- ZAVUS24_Drug_Histories %>% filter(grepl(string_CGRPs, Treat)) %>% select(patient) %>% distinct() %>% mutate(CGRP="CGRP")

All_Pats <- All_Pats %>% left_join(CGRPs_Pats)

ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(Treat!="-")
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, Treat) %>% distinct() %>% group_by(patient) %>% count()
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% ungroup() %>% rename("Lines"="n")

All_Pats <- All_Pats %>% left_join(ZAVUS24_Drug_Histories)
mean(All_Pats$Lines)
unique(All_Pats$Lines)
All_Pats %>% group_by(CGRP) %>% summarise(mean=mean(Lines))
All_Pats %>% mutate(Lines=ifelse(Lines<=3, "1to3",
                                 ifelse(Lines<=6, "4to6",
                                        ifelse(Lines<=9,"7to9","10+")))) %>% group_by(CGRP,Lines) %>% count()
# -------------
# Concomitants classes --------------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(Month=="month60")
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(grepl("134", Treat))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, Treat)
ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, Treat, sep = ",", convert=T )

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, drug_class) %>% distinct()
ZAVUS24_Drug_Histories$Exp <- 1

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% arrange(patient, drug_class)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(drug_class!="CGRP Nasal")
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% group_by(patient) %>% mutate(Exp=paste0(drug_class, collapse = " + "))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, Exp) %>% distinct() 

data.frame(ZAVUS24_Drug_Histories %>% ungroup() %>% group_by(Exp) %>% count() %>% arrange(-n))




# -------------
# Generate long flows table --------------
MIG_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- read.table("Source/ZAVUS24 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

setDT(MIG_Drug_Histories)
setDT(MIG_Box_Histories)

# Flows table in long format
flMIG <- MIG_Drug_Histories
flMIG <- flMIG[,disease := NULL]

flMIG <- melt(flMIG, id = c("patient","weight"))
names(flMIG)[c(3,4)] <- c("p1","v1")
flMIG <- flMIG[, p1 := str_extract(p1,"[:digit:]+")]
flMIG$p1 <- as.numeric(flMIG$p1)
flMIG <- data.frame(cbind(flMIG[p1 < 60], flMIG[p1 > 1,.(p2 = p1, v2 = v1)]), stringsAsFactors = F)
flMIG <- flMIG[,c(1:3,5,4,6)]

# Any flow flag and stops flag**************************************************
flMIG <- setDT(flMIG)[, flow := (v1 != v2)*1]
flMIG <- flMIG[, stops := (flow == 1 & v2 == "-")*1]


# Treatment experience**********************************************************
RxExp <- data.frame(MIG_Drug_Histories, stringsAsFactors = F)
RxExp$month1 <- (RxExp$month1 != "-")*1

for(i in 2:60){
  cat(i)
  RxExp[,i+2] <- (((RxExp[,i+2] != "-")*1 + RxExp[,i+2-1]) > 0)*1
}

RxExp <- setDT(RxExp)
RxExp <- melt(RxExp, id = c("patient","weight"))
RxExp <- RxExp[, month := str_extract(variable,"[:digit:]+")]
RxExp$month <- as.numeric(RxExp$month)
names(RxExp)[4] <- "MIG_RxExp"

flMIG <- RxExp[,.(patient,month,MIG_RxExp)][flMIG, on = .(patient, month = p1)]
flMIG <- flMIG[,.(patient, weight, p1 = month, p2, v1, v2, p1_RxExp = MIG_RxExp, flow, stops)]

# Starts and re-starts flag*****************************************************
flMIG <- flMIG[, starts := (flow == 1 & v1 == "-" & p1_RxExp == 0)*1]
flMIG <- flMIG[, re_starts := (flow == 1 & v1 == "-" & p1_RxExp == 1)*1]
flMIG <- flMIG[, disease := "ZAVUS24"]
flMIG <- flMIG[,c(12,1:11)]

# Bring Therapy classes (Stocks) to the table***********************************
MIG_Box_Histories <- MIG_Box_Histories[,disease := NULL]
MIG_Box_Histories <- data.frame(MIG_Box_Histories, stringsAsFactors = F)

for(i in 1:60){
  cat(i)
  MIG_Box_Histories[,i+2] <- unlist(lapply(MIG_Box_Histories[,i+2],function(x) str_sub(x, 1L, 1L)))
}

setDT(MIG_Box_Histories) 
MIG_Box_Histories <- melt(MIG_Box_Histories, id = c("patient","weight"))
names(MIG_Box_Histories)[c(3,4)] <- c("p","s")
MIG_Box_Histories <- MIG_Box_Histories[, p := str_extract(p,"[:digit:]+")]
MIG_Box_Histories$p <- as.numeric(MIG_Box_Histories$p)

flMIG <- MIG_Box_Histories[,.(patient,p,s)][flMIG, on = .(patient, p = p1)]
names(flMIG)[c(2,3)] <- c("p1","s1")
flMIG <- MIG_Box_Histories[,.(patient,p,s)][flMIG, on = .(patient, p = p2)]
names(flMIG)[c(2,3)] <- c("p2","s2")

flMIG <- flMIG[,.(disease, patient, weight, p1, p2, v1, v2, s1, s2, p1_RxExp, flow, stops, starts, re_starts)]
names(flMIG)[c(6,7)] <- c("d1","d2")

fwrite(flMIG,"Source/MIG_Flows_Aux_Long.txt")

# --------
# Classes Before vs After First Zavegepant ------------------

flMIG <- fread("Source/MIG_Flows_Aux_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_ZAV <- flMIG %>% 
  filter(grepl("134", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_ZAV"="p2")

flMIG <- First_ZAV %>% left_join(flMIG)

flMIG <- flMIG %>% filter(First_ZAV>1&First_ZAV<60)  %>% mutate(before=First_ZAV-1) %>% mutate(after=First_ZAV+1) 

before <- flMIG %>% filter(p2==before) %>% select(patient, d2)
current <- flMIG %>% filter(p2==First_ZAV) %>% select(patient, d2)
after <- flMIG %>% filter(p2==after) %>% select(patient, d2)

before <- separate_rows(before, d2, sep = ",", convert=T )
current <- separate_rows(current, d2, sep = ",", convert=T )
after <- separate_rows(after, d2, sep = ",", convert=T )


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


data.frame(before %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, drug_class) %>% distinct() %>% group_by(drug_class) %>% count() %>% mutate(n=n/length(unique(before$patient))) %>%
  rename("before"="n") %>%
  full_join(
    current %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, drug_class) %>% distinct() %>% group_by(drug_class) %>% count() %>% mutate(n=n/length(unique(current$patient))) %>%
  rename("current"="n")) %>%
  full_join(
    after %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, drug_class) %>% distinct() %>% group_by(drug_class) %>% count() %>% mutate(n=n/length(unique(after$patient))) %>%
  rename("after"="n")
  ))

flMIG <- flMIG %>% select(patient, First_ZAV, p2, d2)
length(unique(flMIG$patient)) # 133

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG %>% mutate(p2=p2-First_ZAV) %>%
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>%
  select(patient, p2, generic) %>% distinct() %>%
  group_by(patient, p2) %>% count() %>% ungroup() %>%
  group_by(p2) %>% summarise(mean=mean(n)) %>%
  ggplot(aes(p2, mean)) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_line(size=2, alpha=0.5, colour="deepskyblue4") +
  theme_bw() +
  xlab("\n Month Relative to Zavogepant Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  

# -----------
# Stocks Flows to Zavegepant ------------

flMIG <- fread("Source/MIG_Flows_Aux_Long.txt")
flMIG <- flMIG %>% filter(p2>=49)

flMIG %>% filter(!grepl("Z",s1) & grepl("Z",s2)) %>%
  group_by(s1) %>% count() %>% mutate(n=round(n/192,2))

flMIG %>% filter(grepl("Z",s1) & !grepl("Z",s2)) %>%
  group_by(s2) %>% count() %>% mutate(n=round(n/54,2))

flMIG <- fread("Source/MIG_Flows_Aux_Long.txt")
flMIG <- flMIG %>% filter(!grepl("Z",s1) & grepl("Z",s2)) 

data.frame(flMIG %>% group_by(p2, s1) %>% count() %>% spread(key=s1, value=n))


# -----------------
# Pathways to Zavegepant ------------------------------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(grepl("134", Treat)) %>% select(patient) %>% distinct() %>% left_join(ZAVUS24_Drug_Histories)
ZAVUS24_Drug_Histories$Month <- parse_number(as.character(ZAVUS24_Drug_Histories$Month))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(Treat != "-" &  Treat != "*"  )

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl("134",Treat))) 1:which.max(grepl("134",Treat)) else row_number())   

ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, Treat, sep = ",", convert=T )

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, Month, drug_group) %>% distinct()

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% anti_join(
  ZAVUS24_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct()
  )

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% group_by(patient, drug_group) %>% count()

ZAVUS24_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(mean=mean(n))


#   drug_group       mean
# 1 CGRP Injectable  19.8
# 2 CGRP Oral        10.0
# 3 Preventive       27.4
# 4 Symptomatic      19.1
# 5 Triptans         14.7

ZAVUS24_Drug_Histories %>% ungroup() %>%
  mutate(drug_group=factor(drug_group, levels=c( "CGRP Oral", "Triptans",  "Symptomatic",  "CGRP Injectable","Preventive" ))) %>%
  ggplot(aes(drug_group,n,colour=drug_group, fill=drug_group)) +
  geom_jitter(alpha=0.3, width=0.2, shape = 1, size = 2, stroke = 2) +
  geom_boxplot(alpha=0.5, notch = TRUE) +
  geom_violin(alpha=0.5) +
  theme_minimal() +
  ylab("Number of Months Until Rimegepant Initiation \n") + xlab("\n Drug Group") +
  scale_fill_manual(values=c( "#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  scale_colour_manual(values=c("#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) 


length(unique(ZAVUS24_Drug_Histories$patient))

data.frame(
  ZAVUS24_Drug_Histories %>% ungroup() %>% select(-n) %>% distinct() %>%
    arrange(patient, drug_group) %>%
  group_by(patient) %>% mutate(drug_group=paste0(drug_group, collapse="+")) %>% distinct() %>%
  ungroup() %>% group_by(drug_group) %>% count() %>% arrange(-n) %>% mutate(n=n/185)
  )
  



ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(grepl("134", Treat)) %>% select(patient) %>% distinct() %>% left_join(ZAVUS24_Drug_Histories)
ZAVUS24_Drug_Histories$Month <- parse_number(as.character(ZAVUS24_Drug_Histories$Month))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(Treat != "-" &  Treat != "*"  )

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl("134",Treat))) 1:which.max(grepl("134",Treat)) else row_number())   

ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, Treat, sep = ",", convert=T )

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, Month, drug_group) %>% distinct()

length(unique(ZAVUS24_Drug_Histories$patient))

ZAVUS24_Drug_Histories %>% 
  left_join(
    ZAVUS24_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct() %>% rename("Max"="Month")
    ) %>%
  mutate(Month=Month-Max) %>% filter(Month!=0) %>%
    mutate(drug_group=factor(drug_group, levels=c( "CGRP Oral", "Triptans",  "Symptomatic",  "CGRP Injectable","Preventive" ))) %>%
  ungroup() %>% group_by(Month, drug_group) %>% count() %>% rename("pop"="n") %>%
  ggplot(aes(Month, 100*pop/189, colour=drug_group, fill=drug_group)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Number of Months Until Zavegepant Initiation") + ylab("Population (%) \n") +
  scale_fill_manual(values=c( "#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  scale_colour_manual(values=c("#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) 


# --------------
# ICD10 Zavegepant vs Other MIG --------------

RIMUS23_Comorbidities_Extended_Dxs <- fread("RIMUS23 Comorbidities Extended Dxs.txt")
length(unique(RIMUS23_Comorbidities_Extended_Dxs$patid)) # 262194
RIMUS23_Comorbidities_Extended_Dxs$ICD10_diag <- str_sub(RIMUS23_Comorbidities_Extended_Dxs$ICD10_diag, 1L, 3L)
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()

ZAVUS24_Comorbidities_Extended_Dxs <- fread("Source/ZAVUS24 Comorbidities Extended Dxs.txt")
length(unique(ZAVUS24_Comorbidities_Extended_Dxs$patid)) # 251
ZAVUS24_Comorbidities_Extended_Dxs$ICD10_diag <- str_sub(ZAVUS24_Comorbidities_Extended_Dxs$ICD10_diag, 1L, 3L)
ZAVUS24_Comorbidities_Extended_Dxs <- ZAVUS24_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()

data.frame(
  ZAVUS24_Comorbidities_Extended_Dxs %>% group_by(ICD10_diag) %>% count() %>% mutate(n=n/251) %>% rename("ZAV"="n") %>%
  inner_join(RIMUS23_Comorbidities_Extended_Dxs %>% group_by(ICD10_diag) %>% count() %>% mutate(n=n/262194) %>% rename("ALL"="n"))
  ) %>% mutate(Diff=ZAV-ALL) %>% arrange(-abs(Diff))

# ------------
# Profile patients 1 refill vs 2+ refills  --------------


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(generic=="Zavegepant")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, from_dt)

Refills <- ZAVUS24_Doses %>% group_by(patid) %>% count()
Refills <- Refills %>% mutate(n=ifelse(n==1,n,2)) %>% rename("refills"="n")
Refills %>% group_by(refills) %>% count() # 1  166   2  94
 
ZAVUS24_Comorbidities_Extended_Dxs <- fread("Source/ZAVUS24 Comorbidities Extended Dxs.txt")
ZAVUS24_Comorbidities_Extended_Dxs$ICD10_diag <- str_sub(ZAVUS24_Comorbidities_Extended_Dxs$ICD10_diag, 1L, 3L)
ZAVUS24_Comorbidities_Extended_Dxs <- ZAVUS24_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()

data.frame(
  ZAVUS24_Comorbidities_Extended_Dxs %>% inner_join(Refills) %>%
    group_by(refills, ICD10_diag) %>% count() %>% 
    mutate(n=ifelse(refills==1,n/166, n/94)) %>%
    spread(key=refills, value=n) %>%
    mutate(`2`=ifelse(is.na(`2`),0,`2`)) %>% mutate(`1`=ifelse(is.na(`1`),0,`1`)) %>%
    mutate(Diff=`2`-`1`) %>% arrange(-abs(Diff))
  )


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(generic=="Zavegepant")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, from_dt)

Refills <- ZAVUS24_Doses %>% group_by(patid) %>% count()
Refills <- Refills %>% mutate(n=ifelse(n==1,n,2)) %>% rename("refills"="n")
Refills %>% group_by(refills) %>% count() # 1  166   2  94

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, drug_class) %>% distinct()

data.frame(ZAVUS24_Doses %>% inner_join(Refills) %>% group_by(refills, drug_class) %>% count() %>%
    mutate(n=ifelse(refills==1,n/166, n/94)) %>% spread(key=refills, value=n) %>%
       mutate(Diff=`2`-`1`) %>% arrange(-abs(Diff)))



ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(generic=="Zavegepant")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, from_dt) %>% group_by(patid) %>% filter(from_dt==min(from_dt)) %>% distinct()
ZAVUS24_Doses$from_dt <- as.Date(ZAVUS24_Doses$from_dt)

ZAVUS24_Doses %>% inner_join(Refills) %>% group_by(refills) %>% summarise(mean=mean(from_dt))



# ------------
# Stocks Flows to Zavegepant by molecule of CGRP ------------


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


flMIG <- fread("Source/MIG_Flows_Aux_Long.txt")
flMIG <- flMIG %>% filter(p2>=49)

S1 <- flMIG %>% filter(!grepl("Z",s1) & grepl("Z",s2)) %>% filter(s1=="O"|s1=="I") %>% select(patient,p2, d1)
# 122

S1 <- separate_rows(S1, d1, sep = ",", convert=T )

S1 %>% left_join(Drugs_lookup %>% select(drug_id, generic, drug_class), by=c("d1"="drug_id")) %>%
  filter(grepl("CGRP", drug_class)) %>%
   group_by(generic) %>% count() %>% mutate(n=n/122) %>% arrange(-n)

# ------------------------

# Compare ICD10 in Zavegepant with vs without Riemgepant ----------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAV_pats <- ZAVUS24_Doses %>% select(patid) %>% distinct()
RIME_pats <- ZAVUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid) %>% distinct() %>% mutate(RIME="RIME")
ZAV_pats <- ZAV_pats %>% left_join(RIME_pats) %>% mutate(RIME=ifelse(is.na(RIME), "no", RIME))

ZAVUS24_Comorbidities_Extended_Dxs <- fread("Source/ZAVUS24 Comorbidities Extended Dxs.txt")
ZAVUS24_Comorbidities_Extended_Dxs <- ZAVUS24_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()

ZAV_pats %>% group_by(RIME) %>% count()

to_save <- ZAV_pats %>% left_join(ZAVUS24_Comorbidities_Extended_Dxs) %>%
  group_by(RIME, ICD10_diag) %>% count() %>%
  mutate(n=ifelse(RIME=="RIME", n/114, n/146)) %>%
  spread(key=RIME, value=n) %>% drop_na()

ZAVUS24_Comorbidities_Extended_Dxs %>% group_by(patid) %>% count() %>%
  inner_join(ZAV_pats) %>% ungroup() %>% group_by(RIME) %>% summarise(mean=mean(n))

fwrite(to_save, "Comorbidities_Pen_ZAV_Rime.csv")

ZAVUS24_Migraine_Dxs <- fread("Source/ZAVUS24 Migraine Dxs.txt")
ZAVUS24_Migraine_Dxs <- ZAVUS24_Migraine_Dxs %>% select(patid, mig_diag) %>% distinct()

to_save <- ZAV_pats %>% left_join(ZAVUS24_Migraine_Dxs) %>%
  group_by(RIME, mig_diag) %>% count() %>%
  mutate(n=ifelse(RIME=="RIME", n/114, n/146)) %>%
  spread(key=RIME, value=n) %>% drop_na()


ZAVUS24_Migraine_Dxs %>% group_by(patid) %>% count() %>%
  inner_join(ZAV_pats) %>% ungroup() %>% group_by(RIME) %>% summarise(mean=mean(n))

data.frame(to_save)


ZAVUS24_Comorbidities_Extended_Dxs <- fread("Source/ZAVUS24 Comorbidities Extended Dxs.txt")
ZAVUS24_Comorbidities_Extended_Dxs <- ZAVUS24_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()
Prevalence_ZAV <- ZAVUS24_Comorbidities_Extended_Dxs %>% group_by(ICD10_diag) %>% count() %>% mutate(n=n/195)

ZAVUS24_Comorbidities_Extended_Dxs %>% group_by(patid) %>% count() %>% ungroup() %>% summarise(mean=mean(n))


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-") %>% distinct() %>% filter(grepl("136", Treat)) %>% select(patient) %>% distinct()
Rimegepant_pats <- RIMUS23_Drug_Histories

RIMUS23_Comorbidities_Extended_Dxs <- fread("RIMUS23 Comorbidities Extended Dxs.txt")
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()
Prevalence_RIME <- RIMUS23_Comorbidities_Extended_Dxs %>% mutate(patid=as.character(patid)) %>% inner_join(RIMUS23_Drug_Histories, by=c("patid"="patient")) %>% 
  group_by(ICD10_diag) %>% count() %>% mutate(n=n/4235)

All_comorbs <- Prevalence_ZAV %>% rename("ZAV"="n") %>% inner_join(Prevalence_RIME %>% rename("RIME"="n"))

fwrite(All_comorbs, "All_comorbs.csv")

RIMUS23_Comorbidities_Extended_Dxs %>% mutate(patid=as.character(patid)) %>% inner_join(RIMUS23_Drug_Histories, by=c("patid"="patient")) %>%
  group_by(patid) %>% count() %>% ungroup() %>% summarise(mean=mean(n))


Drugs_lookup <- fread("Drugs_lookup.csv")


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease)) %>% inner_join(Rimegepant_pats)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-") %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, Month, drug_class) %>% distinct()

Persis_classes_Rime <- RIMUS23_Drug_Histories %>% group_by(patient, drug_class) %>% count() %>% ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n))


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)

ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(-c(disease)) 
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(Treat != "-") %>% distinct()
ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, Treat, sep = ",", convert=T )
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(Treat != "*") %>% distinct()
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(Treat=as.numeric(Treat)) %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, Month, drug_class) %>% distinct()

Persis_classes_Zav <- ZAVUS24_Drug_Histories %>% group_by(patient, drug_class) %>% count() %>% ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n))

data.frame(Persis_classes_Zav %>% rename("ZAV"="mean") %>% full_join(Persis_classes_Rime %>% rename("RIME"="mean")))


# -----------
# ON ZAV vs Ever tried last 6 months ------------
ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
ZAVUS24_Drug_Histories$Month <- parse_number(as.character(ZAVUS24_Drug_Histories$Month))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(ON=ifelse(grepl("134", Treat),1,0))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, Month, ON)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% arrange(patient, Month) 
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% group_by(patient) %>% mutate(CUM_ON=cumsum(ON))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(CUM_ON=ifelse(CUM_ON==0,0,1))

ZAVUS24_Drug_Histories %>% filter(ON==1) %>% group_by(Month) %>% count()

ZAVUS24_Drug_Histories %>% filter(CUM_ON==1) %>% group_by(Month) %>% count()

# --------