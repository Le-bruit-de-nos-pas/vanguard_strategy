
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




ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(Month=="month60")
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(grepl("134", Treat))
length(unique(ZAVUS24_Drug_Histories$patient))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, Treat)
ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, Treat, sep = ",", convert=T )

ZAVUS24_Drug_Histories %>% filter(Treat!="134") %>% group_by(patient) %>% count() %>% ungroup() %>% summarise(mean=mean(n))

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, drug_class) %>% distinct()

data.frame(ZAVUS24_Drug_Histories %>% group_by(drug_class) %>% count() %>% mutate(n=n/138) %>% arrange(-n))




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
  filter(grepl("CGP", drug_class)) %>%
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


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses$from_dt  <- str_sub(as.character(ZAVUS24_Doses$from_dt), 1L, 7L)
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, from_dt, generic) %>% distinct()

length(unique(ZAVUS24_Doses$patid))

ZAVUS24_Doses %>% filter(generic=="Zavegepant") %>% group_by(patid) %>% 
  filter(from_dt==min(from_dt)) %>% ungroup() %>% group_by(from_dt) %>% count()


# --------
# Overall Migraine 2024 Waterfall stocks monhth 60 INGORE !!! -------------

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)
Drugs_lookup <- Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% distinct()


MIGUS24_Drug_Histories <- fread("Source/MIGUS24 Drug Histories.txt")
length(unique(MIGUS24_Drug_Histories$patient)) # 240747

MIGUS24_Drug_Histories <- MIGUS24_Demographics %>% inner_join(MIGUS24_Drug_Histories, by=c("patid"="patient"))
sum(MIGUS24_Drug_Histories$weight) # 21292150

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(patid, CV, psychiatric, epileptic, weight, month60) %>% 
  filter(month60 != "-") %>% distinct()
MIGUS24_Drug_Histories <- separate_rows(MIGUS24_Drug_Histories, month60, sep = ",", convert=T )

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% left_join(Drugs_lookup, by=c("month60"="drug_id")) %>% select(-month60) %>% distinct()



# MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
#                                                                  ifelse(CV==1&drug_class=="Cardiovascular",1,
#                                                                         ifelse(CV==1&drug_class=="Calcium Blocker",1,
#                                                                                ifelse(epileptic==1&drug_class=="Antiepileptic",1,
#                                                                                       ifelse(psychiatric==1&drug_class=="SSRI",1,
#                                                                                              ifelse(psychiatric==1&drug_class=="SNRI",1,
#                                                                                                     ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
#                                                                                                            ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))
# 
# MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(flag==0) %>% select(-flag)

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(-c(CV, psychiatric, epileptic))
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(-drug_class) %>% distinct()
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)
MIGUS24_Drug_Histories[is.na(MIGUS24_Drug_Histories)] <- 0

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(Box=ifelse(`CGRP Nasal`==1, "Nasal",
                                                                       ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                                              ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                                                     ifelse(Preventive==1&Triptans==1, "Prev_Acute",
                                                                                            ifelse(Preventive==1&Ditans==1, "Prev_Acute",
                                                                                                   ifelse(Preventive==1&Symptomatic==1, "Prev_Sympt",
                                                                                                          ifelse(Preventive==1, "Prev",
                                                                                                                 ifelse(Triptans==1, "Acute",
                                                                                                                        ifelse(Ditans==1, "Acute",
                                                                                                                               ifelse(Symptomatic==1, "Sympt", "Lapsed")))))))))))


MIGUS24_Drug_Histories %>% group_by(Box) %>% summarise(tot=sum(weight))

# ---------

# Overall Migraine 2024 Stocks monht over month INGORE !!!-------------

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)
Drugs_lookup <- Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% distinct()


MIGUS24_Drug_Histories <- fread("Source/MIGUS24 Drug Histories.txt")
length(unique(MIGUS24_Drug_Histories$patient)) # 240747

MIGUS24_Drug_Histories <- MIGUS24_Demographics %>% inner_join(MIGUS24_Drug_Histories, by=c("patid"="patient"))
sum(MIGUS24_Drug_Histories$weight) # 21292150

MIGUS24_Drug_Histories <- gather(MIGUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIGUS24_Drug_Histories <- separate_rows(MIGUS24_Drug_Histories, Treat, sep = ",", convert=T )


MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% 
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id")) %>% select(-Treat) %>% distinct()

MIGUS24_Drug_Histories$Month <- parse_number(as.character(MIGUS24_Drug_Histories$Month))


MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(flag==0) %>% select(-flag)

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(-c(CV, psychiatric, epileptic))
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(-drug_class) %>% distinct()
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)
MIGUS24_Drug_Histories[is.na(MIGUS24_Drug_Histories)] <- 0

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(Box=ifelse(`CGRP Nasal`==1, "Nasal",
                                                                       ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                                              ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                                                     ifelse(Preventive==1&Triptans==1, "Prev_Acute",
                                                                                            ifelse(Preventive==1&Ditans==1, "Prev_Acute",
                                                                                                   ifelse(Preventive==1&Symptomatic==1, "Prev_Sympt",
                                                                                                          ifelse(Preventive==1, "Prev",
                                                                                                                 ifelse(Triptans==1, "Acute",
                                                                                                                        ifelse(Ditans==1, "Acute",
                                                                                                                               ifelse(Symptomatic==1, "Sympt", "Lapsed")))))))))))


data.frame(MIGUS24_Drug_Histories %>% group_by(Month, Box) %>% filter(Month>=25) %>%
  summarise(tot=sum(weight)) %>% spread(key=Box, value=tot))


# ---------

# Share of Zavegepant among non-orals all migraine INGORE !!! ----------------


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% select(code, generic, drug_class, patid, weight) %>% distinct()

RIME_MEDICATIONS <- fread("Source/RIME Medications.txt")
RIME_MEDICATIONS$med_code <- substr(RIME_MEDICATIONS$med_code, start = 3, stop = nchar(RIME_MEDICATIONS$med_code))

unique(RIME_MEDICATIONS$med_route)

MIGUS24_Doses %>% filter(generic=="Zavegepant")



Non_Oral_pats <- RIME_MEDICATIONS %>% filter(med_route!="ORAL"&med_route!="MULTIPLE ROUTES"&
                              med_route!="ROUTE NOT APPLICABLE"&med_route!="UNKNOWN") %>%
  select(drug_class, med_code, drug_id, generic_name) %>% 
  inner_join(MIGUS24_Doses, by=c("med_code"="code")) %>%
  select(patid, weight) %>% distinct()

sum(Non_Oral_pats$weight)




unique(RIME_MEDICATIONS$drug_class)



Triptan_spray_pats <- RIME_MEDICATIONS %>% filter(med_route=="NASAL"&drug_class=="Triptan") %>%
  select(med_code) %>% 
  inner_join(MIGUS24_Doses, by=c("med_code"="code")) %>%
  select(patid, weight) %>% distinct()

sum(Triptan_spray_pats$weight)


Other_spray_pats <- RIME_MEDICATIONS %>% filter(med_route=="NASAL"&drug_class!="Triptan") %>%
  select(med_code) %>% 
  inner_join(MIGUS24_Doses, by=c("med_code"="code")) %>%
  select(patid, weight) %>% distinct()

sum(Other_spray_pats$weight)



Triptan_inj_pats <- RIME_MEDICATIONS %>% filter(med_route=="INTRAVENOUS"|med_route=="INJECTION"|
                                                  med_route=="INTRADERMAL"|med_route=="SUBCUTANEOUS"|
                                                  med_route=="INTRAMUSCULAR") %>%
                                                  filter(drug_class=="Triptan") %>%
  select(med_code) %>% 
  inner_join(MIGUS24_Doses, by=c("med_code"="code")) %>%
  select(patid, weight) %>% distinct()

sum(Triptan_inj_pats$weight)


Other_inj_pats <- RIME_MEDICATIONS %>% filter(med_route=="INTRAVENOUS"|med_route=="INJECTION"|
                                                  med_route=="INTRADERMAL"|med_route=="SUBCUTANEOUS"|
                                                  med_route=="INTRAMUSCULAR") %>%
                                                  filter(drug_class!="Triptan") %>%
  select(med_code) %>% 
  inner_join(MIGUS24_Doses, by=c("med_code"="code")) %>%
  select(patid, weight) %>% distinct()

sum(Other_inj_pats$weight)


Zav_pats <- MIGUS24_Doses %>% filter(generic=="Zavegepant") %>% select(patid, weight) %>% distinct()



Non_Oral_pats %>%
 inner_join(Zav_pats) %>%
  summarise(n=sum(weight))


Triptan_spray_pats %>%
#  inner_join(Zav_pats) %>%
  summarise(n=sum(weight))

Other_spray_pats %>%
#  inner_join(Zav_pats) %>%
  summarise(n=sum(weight))

Triptan_inj_pats %>%
  inner_join(Zav_pats) %>%
  summarise(n=sum(weight))

Other_inj_pats %>%
  #inner_join(Zav_pats) %>%
  summarise(n=sum(weight))



Zav_pats %>%
 inner_join(Other_spray_pats) %>%
  summarise(n=sum(weight))



# ---------
# NEW Extended files excluding comorbidities -------------

# COMORBIDITIES
MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")
ZAVUS24_Demographics <- ZAVUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
MIGUS24_Demographics <- ZAVUS24_Demographics %>% bind_rows(MIGUS24_Demographics) %>% distinct()

# DRUG LOOKUP
ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
Drugs_lookup <- Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% bind_rows(Drugs_lookup_1) %>% distinct()
fwrite(Drugs_lookup, "Source/Drugs_lookup.txt")
Drugs_lookup <- fread( "Source/Drugs_lookup.txt")


MIGUS24_Drug_Histories <- fread("Source/MIGUS24 Drug Histories.txt")
length(unique(MIGUS24_Drug_Histories$patient)) # 240747
ZAVUS24_Drug_Histories <- fread("Source/ZAVUS24 Drug Histories.txt")

MIGUS24_Drug_Histories %>% select(patient) %>%
  anti_join(ZAVUS24_Drug_Histories %>% select(patient))

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(-disease) %>% mutate(version="NEW_ZAV") %>%
  bind_rows(
    ZAVUS24_Drug_Histories %>% select(-disease) %>% anti_join(
      MIGUS24_Drug_Histories %>% select(patient) %>% distinct()
      ) %>% mutate(version="OLD_ZAV")
    )

length(unique(MIGUS24_Drug_Histories$patient)) # 240965

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(patient, weight, version, month1:month60)

MIGUS24_Drug_Histories <- MIGUS24_Demographics %>% inner_join(MIGUS24_Drug_Histories, by=c("patid"="patient"))

fwrite(MIGUS24_Drug_Histories, "Source/MIGUS24_Drug_Histories_Extended.txt")

All_pats <- MIGUS24_Drug_Histories %>%  select(patid, version, weight) %>% distinct()
All_pats %>% group_by(version) %>% summarise(n=sum(weight))
# 1 NEW_ZAV 21292150.
# 2 OLD_ZAV      218 

MIGUS24_Drug_Histories <- gather(MIGUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(Treat!="-")
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(Treat!="*")

MIGUS24_Drug_Histories <- separate_rows(MIGUS24_Drug_Histories, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% left_join(Drugs_lookup , by=c("Treat"="drug_id")) 



MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(flag==0) %>% select(-flag)

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(-c(CV, psychiatric, epileptic))

Mod_Sev <- MIGUS24_Drug_Histories %>% filter(Month %in% c("month49","month50","month51","month52",
                                                          "month53","month54","month55","month56",
                                                          "month57","month58","month59","month60")) %>% filter(grepl("CGRP",drug_group) | drug_group=="Triptans" | drug_group=="Preventive") %>% select(patid) %>% distinct()

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_last_dx) %>% 
  mutate(migraine_last_dx=as.Date(migraine_last_dx)) %>% filter(migraine_last_dx>="2022-11-16") %>% select(patid)  %>% distinct()

ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")
ZAVUS24_Demographics <- ZAVUS24_Demographics %>%  select(patid, migraine_last_dx) %>% 
  mutate(migraine_last_dx=as.Date(migraine_last_dx)) %>% filter(migraine_last_dx>="2022-11-16") %>% select(patid)  %>% distinct()

Mod_Sev <- ZAVUS24_Demographics %>% bind_rows(MIGUS24_Demographics) %>% bind_rows(Mod_Sev) %>% distinct()
Mod_Sev$group <- "ModSev"

fwrite(Mod_Sev, "Source/Mod_Sev.txt")

All_pats %>% filter(version =="NEW_ZAV") %>% summarise(n=sum(weight)) # 21292150
All_pats %>% inner_join(Mod_Sev) %>% filter(version =="NEW_ZAV") %>% summarise(n=sum(weight)) # 12768773

df_month60 <- MIGUS24_Drug_Histories %>% filter(version=="NEW_ZAV" & Month=="month60") %>% 
  select(patid, weight, drug_group) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=drug_group, value=exp)

df_month60[is.na(df_month60)] <- 0

df_month60 <- df_month60 %>% mutate(Box=ifelse(`CGRP Nasal`==1, "Nasal",
                                                                       ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                                              ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                                                     ifelse(Preventive==1&Triptans==1, "Prev_Acute",
                                                                                                   ifelse(Preventive==1&Symptomatic==1, "Prev_Sympt",
                                                                                                          ifelse(Preventive==1, "Prev",
                                                                                                                 ifelse(Triptans==1, "Acute",
                                                                                                                               ifelse(Symptomatic==1, "Sympt", "Lapsed")))))))))

df_month60 %>% group_by(Box) %>% summarise(tot=sum(weight))

df <- All_pats %>% left_join(MIGUS24_Drug_Histories %>% select(-c(drug_class, drug_group))) %>%
  arrange(patid, Month, Treat) %>%
  group_by(patid, Month) %>% mutate(Treat=paste0(Treat, collapse=",")) %>% ungroup() %>% distinct() %>%
  spread(key=Month, value=Treat)

df[is.na(df)] <- "-"

df %>% group_by(version) %>% summarise(n=sum(weight))
# 1 NEW_ZAV 21292150.
# 2 OLD_ZAV      218 
names(df)
df <- df %>% select(-c(`<NA>`)) %>% distinct()

fwrite(df, "Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")


Drugs_lookup <- fread("Source/Drugs_lookup.txt")
df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- df %>% filter(version=="NEW_ZAV") %>% select(-version)
sum(df$weight) # 21292150
df <- gather(df, Month, Treat, month1:month60, factor_key=TRUE)
df <- df %>% filter(Treat!="-")
df <- separate_rows(df, Treat, sep = ",", convert=T )

df <- df %>% left_join(Drugs_lookup , by=c("Treat"="drug_id")) %>% select(-Treat) %>% distinct()
df$Month <- parse_number(as.character(df$Month))
df <- df %>% select(-drug_class) %>% distinct()
df <- df %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)
df[is.na(df)] <- 0

df <- df %>% mutate(Box=ifelse(`CGRP Nasal`==1, "Nasal",
                               ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                      ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                             ifelse(Preventive==1&Triptans==1, "Prev_Acute",
                                                           ifelse(Preventive==1&Symptomatic==1, "Prev_Sympt",
                                                                  ifelse(Preventive==1, "Prev",
                                                                         ifelse(Triptans==1, "Acute",
                                                                                       ifelse(Symptomatic==1, "Sympt", "Lapsed")))))))))


data.frame(df %>% group_by(Month, Box) %>% summarise(tot=sum(weight)) %>% spread(key=Box, value=tot))


# -------------
# Oral only vs non-oral waterall------------

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% select(code, generic, drug_class, patid, weight) %>% distinct()

RIME_MEDICATIONS <- fread("Source/RIME Medications.txt")
RIME_MEDICATIONS$med_code <- substr(RIME_MEDICATIONS$med_code, start = 3, stop = nchar(RIME_MEDICATIONS$med_code))

unique(RIME_MEDICATIONS$med_route)

Zav_pats <- MIGUS24_Doses %>% filter(generic=="Zavegepant") %>% select(patid, weight) %>% distinct()



MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 


MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, code) %>% distinct() %>%
  inner_join(RIME_MEDICATIONS %>% select(med_code, med_route, drug_class) %>% distinct() , by=c("code"="med_code"))

MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Demographics) %>%
  mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                     ifelse(CV==1&drug_class=="Cardiovascular",1,
                            ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                   ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                          ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                 ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                        ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                               ifelse(psychiatric==1&drug_class=="Tricyclic",1,0))))))))) %>%
   filter(flag==0) %>% select(-flag)



Non_Oral_pats <- MIGUS24_Doses %>% filter(med_route!="ORAL"&med_route!="MULTIPLE ROUTES"&
                              med_route!="ROUTE NOT APPLICABLE"&med_route!="UNKNOWN") %>%
  select(patid) %>% distinct() %>% mutate(Non_Oral="Non_Oral")

Triptan_spray_pats <- MIGUS24_Doses %>% filter(med_route=="NASAL"&drug_class=="Triptan") %>%
  select(patid) %>% distinct()  %>% mutate(Triptan_spray="Triptan_spray")

Other_spray_pats <- MIGUS24_Doses %>% filter(med_route=="NASAL"&drug_class!="Triptan") %>%
  select(patid) %>% distinct() %>% mutate(Other_spray="Other_spray")

Triptan_inj_pats <- MIGUS24_Doses %>% filter(med_route=="INTRAVENOUS"|med_route=="INJECTION"|
                                                  med_route=="INTRADERMAL"|med_route=="SUBCUTANEOUS"|
                                                  med_route=="INTRAMUSCULAR") %>%
                                                  filter(drug_class=="Triptan") %>%
  select(patid) %>% distinct() %>% mutate(Triptan_inj="Triptan_inj")

Other_inj_pats <- MIGUS24_Doses %>% filter(med_route=="INTRAVENOUS"|med_route=="INJECTION"|
                                                  med_route=="INTRADERMAL"|med_route=="SUBCUTANEOUS"|
                                                  med_route=="INTRAMUSCULAR") %>%
                                                  filter(drug_class!="Triptan") %>%
  select(patid) %>% distinct() %>% mutate(Other_inj_pats="Other_inj_pats")


df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- df %>% filter(version=="NEW_ZAV") %>% select(patid, weight)

df <-  df %>% left_join(Non_Oral_pats) %>% left_join(Triptan_spray_pats) %>% left_join(Triptan_inj_pats) %>% 
  left_join(Other_spray_pats) %>% left_join(Other_inj_pats) 

df[is.na(df)] <- "0"
sum(df$weight)
df %>% mutate(Box=ifelse(Triptan_spray=="Triptan_spray", "Triptan_spray",
                         ifelse(Triptan_inj=="Triptan_inj","Triptan_inj",
                                ifelse(Other_spray=="Other_spray",Other_spray,
                                       ifelse(Other_inj_pats=="Other_inj_pats", "Other_inj_pats", "Oral_only"))))) %>%
                group_by(Box) %>% summarise(n=sum(weight))

# -------

# Pills per month of Rimegepant ------------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")

RIME_pats <- ZAVUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid) %>% distinct()
ZAVUS24_Doses <- RIME_pats %>% left_join(ZAVUS24_Doses) %>% filter(generic=="Rimegepant")
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
range(ZAVUS24_Doses$from_dt)
ZAVUS24_Doses <- ZAVUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2023-01-16") %>% filter(days_sup  != "")
ZAVUS24_Doses <- ZAVUS24_Doses %>% arrange(patid, generic, from_dt) 
ZAVUS24_Doses <- ZAVUS24_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
ZAVUS24_Doses <- ZAVUS24_Doses %>% drop_na()
ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(elapsed <= 92)
ZAVUS24_Doses <- ZAVUS24_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

ZAVUS24_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(generic, mean) %>% summarise(n=sum(as.numeric(weight)))


ZAVUS24_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  ggplot(aes(mean)) +
  geom_density(fill="deepskyblue4", alpha=0.8) +
  theme_minimal() +
  ylab("Patient density \n") + xlab("\n Number of Rimegepant Pills per month")

# --------
# NEW Patient Classification Ever vs 12 months ------------
Drug_formulary <- fread("Source/Drug_formulary.txt")

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
sum(MIGUS24_Drug_Histories_Extended_NoComorbs$weight) # 21292150

MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month>=49)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-Month) %>% distinct() %>% filter(Treat!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()
names(MIGUS24_Drug_Histories_Extended_NoComorbs)[3] <- "drug_id"

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% select(code, drug_id, generic, drug_class, drug_group, patid, weight) %>% distinct()
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)



RIME_MEDICATIONS <- fread("Source/RIME Medications.txt")
RIME_MEDICATIONS$med_code <- substr(RIME_MEDICATIONS$med_code, start = 3, stop = nchar(RIME_MEDICATIONS$med_code))
unique(RIME_MEDICATIONS$drug_group)
RIME_MEDICATIONS <- RIME_MEDICATIONS %>% filter(drug_group=="Acute") %>% 
  filter(med_route!="ORAL"&med_route!="MULTIPLE ROUTES"&med_route!="ROUTE NOT APPLICABLE"&med_route!="UNKNOWN") %>%
  select(med_code) %>% distinct() %>% mutate(NonOralTriptans="NonOralTriptans") %>% rename("code"="med_code")

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(RIME_MEDICATIONS) %>% mutate(NonOralTriptans=ifelse(is.na(NonOralTriptans),"no",NonOralTriptans))

NonOralTriptans <- MIGUS24_Doses %>% select(patid, NonOralTriptans) %>% distinct() %>% filter(NonOralTriptans=="NonOralTriptans") 

MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, drug_group) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)
MIGUS24_Doses[is.na(MIGUS24_Doses)] <- 0
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(Box=ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                     ifelse(`CGRP Injectable`==1,"CGRP_Inj",
                                                            ifelse(Preventive==1, "Prev", "Acute_Only"))))

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(NonOralTriptans)
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(NonOralTriptans=ifelse(is.na(NonOralTriptans),"no",NonOralTriptans))

length(unique(MIGUS24_Doses$patid))

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(Box2=ifelse(NonOralTriptans=="NonOralTriptans","NonOralTriptans",
                                     ifelse(Triptans==1, "Triptans",
                                            ifelse(Symptomatic==1,"Symptomatic", "No_Sympt"))))


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <-MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Doses <- Mod_Sev %>% inner_join(MIGUS24_Doses)

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) 


Nasal <- MIGUS24_Doses %>% filter(`CGRP Nasal`==1) %>% select(patid) %>% mutate(Nasal="Nasal")

MIGUS24_Doses %>%  group_by(Box, Box2) %>% summarise(n=sum(weight)) %>% rename("den"="n") %>%
  left_join(MIGUS24_Doses %>%  inner_join(Nasal) %>% group_by(Box, Box2) %>% summarise(n=sum(weight)) %>% rename("num"="n")) %>%
  mutate(num=ifelse(is.na(num),0,num)) %>% mutate(share=num/den)

# -------
# NEW Patient Classification Ever vs 12 months: WITH OLD ZAV Pats  ------------
Drug_formulary <- fread("Source/Drug_formulary.txt")

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")

sum(MIGUS24_Drug_Histories_Extended_NoComorbs$weight) # 21292150

MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month>=49)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-Month) %>% distinct() %>% filter(Treat!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()
names(MIGUS24_Drug_Histories_Extended_NoComorbs)[3] <- "drug_id"

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% select(code, drug_id, generic, drug_class, drug_group, patid, weight) %>% distinct()
ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(code, drug_id, generic, drug_class, drug_group, patid, weight) %>% distinct()

MIGUS24_Doses <- MIGUS24_Doses %>% bind_rows(ZAVUS24_Doses)

MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)


RIME_MEDICATIONS <- fread("Source/RIME Medications.txt")
RIME_MEDICATIONS$med_code <- substr(RIME_MEDICATIONS$med_code, start = 3, stop = nchar(RIME_MEDICATIONS$med_code))
unique(RIME_MEDICATIONS$drug_group)
RIME_MEDICATIONS <- RIME_MEDICATIONS %>% filter(drug_group=="Acute") %>% 
  filter(med_route!="ORAL"&med_route!="MULTIPLE ROUTES"&med_route!="ROUTE NOT APPLICABLE"&med_route!="UNKNOWN") %>%
  select(med_code) %>% distinct() %>% mutate(NonOralTriptans="NonOralTriptans") %>% rename("code"="med_code")

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(RIME_MEDICATIONS) %>% mutate(NonOralTriptans=ifelse(is.na(NonOralTriptans),"no",NonOralTriptans))

NonOralTriptans <- MIGUS24_Doses %>% select(patid, NonOralTriptans) %>% distinct() %>% filter(NonOralTriptans=="NonOralTriptans") 

MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, drug_group) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)
MIGUS24_Doses[is.na(MIGUS24_Doses)] <- 0
names(MIGUS24_Doses)
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(Box=ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                     ifelse(`CGRP Injectable`==1,"CGRP_Inj",
                                                            ifelse(Preventive==1, "Prev", "Acute_Only"))))

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(NonOralTriptans)
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(NonOralTriptans=ifelse(is.na(NonOralTriptans),"no",NonOralTriptans))

length(unique(MIGUS24_Doses$patid))

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(Box2=ifelse(NonOralTriptans=="NonOralTriptans","NonOralTriptans",
                                     ifelse(Triptans==1, "Triptans",
                                            ifelse(Symptomatic==1,"Symptomatic", "No_Sympt"))))


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight)


MIGUS24_Doses <- MIGUS24_Doses %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) 

Nasal <- MIGUS24_Doses %>% filter(`CGRP Nasal`==1) %>% select(patid) %>% mutate(Nasal="Nasal")


MIGUS24_Doses


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(weight, AGE, GENDER)  %>% distinct()
ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")
ZAVUS24_Demographics <- ZAVUS24_Demographics %>%  select(patid, age, gender) 

ZAV_weights <- ZAVUS24_Demographics %>% left_join(MIGUS24_Demographics %>% rename("age"="AGE", "gender"="GENDER")) %>%
  select(patid, weight) %>% rename("ZAV_weight"="weight")


MIGUS24_Doses <- MIGUS24_Doses %>% left_join(ZAV_weights) %>% mutate(ZAV_weight=ifelse(is.na(ZAV_weight), weight, ZAV_weight))

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Doses %>% inner_join(Mod_Sev) %>%  group_by(Box, Box2) %>% summarise(n=sum(ZAV_weight)) %>% rename("den"="n") %>%
  left_join(MIGUS24_Doses %>% inner_join(Mod_Sev) %>%   inner_join(Nasal) %>% group_by(Box, Box2) %>% summarise(n=sum(ZAV_weight)) %>% rename("num"="n")) %>%
  mutate(num=ifelse(is.na(num),0,num)) %>% mutate(share=num/den)

# -------

# Pills per month of rimegepant relative to Zavegepant / Nasal Triptan initiation ------------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")

RIME_pats <- ZAVUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid) %>% distinct()
ZAVUS24_Doses <- RIME_pats %>% left_join(ZAVUS24_Doses) 

ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(generic=="Zavegepant") %>% group_by(patid) %>% filter(from_dt==min(from_dt)) %>%
  select(patid, from_dt) %>% rename("first_zav"="from_dt") %>%
  left_join(ZAVUS24_Doses) %>% select(patid, first_zav, drug_id, generic, from_dt, days_sup, qty)


ZAVUS24_Doses$first_zav <- as.Date(ZAVUS24_Doses$first_zav)
ZAVUS24_Doses$from_dt <- as.Date(ZAVUS24_Doses$from_dt)

ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(generic=="Rimegepant")
ZAVUS24_Doses <- ZAVUS24_Doses %>% mutate(from_dt=from_dt-first_zav) 
ZAVUS24_Doses$from_dt <- as.numeric(ZAVUS24_Doses$from_dt) 

ZAVUS24_Doses %>%
  filter(from_dt<0) %>% select(patid) %>% distinct() %>%
  inner_join(
    ZAVUS24_Doses %>%
  filter(from_dt>0) %>% select(patid) %>% distinct()
  ) %>%
  left_join(ZAVUS24_Doses) %>%
  group_by(patid) %>% arrange(patid, abs(from_dt)) %>%
  mutate(patid=as.character(patid)) %>%
  filter(from_dt!=0) %>%
  slice(1:2) %>%
  #filter(from_dt>=(-100) & from_dt<=100) %>%
  ggplot(aes(from_dt, days_sup)) +
  geom_smooth()



ZAVUS24_Doses %>%
  filter(from_dt<0) %>% select(patid) %>% distinct() %>%
  inner_join(
    ZAVUS24_Doses %>%
  filter(from_dt>0) %>% select(patid) %>% distinct()
  ) %>%
  left_join(ZAVUS24_Doses) %>%
  group_by(patid) %>% arrange(patid, abs(from_dt)) %>%
  mutate(patid=as.character(patid)) %>%
  #filter(from_dt!=0) %>%
  # slice(1:2) %>%
  filter(from_dt>=(-100) & from_dt<=100) %>%
  ggplot(aes(from_dt, qty)) +
  geom_smooth(fill="deepskyblue4", colour="firebrick") +
  ylim(0,20) +
  theme_minimal() + 
  xlab("\n Number of days relative to FIRST ZAVEGEPANT") +
  ylab("Number of Rimegepant Pills per script \n")




# ZAVUS24_Doses %>%
#   filter(from_dt<0) %>% select(patid) %>% distinct() %>%
#   inner_join(
#     ZAVUS24_Doses %>%
#   filter(from_dt>0) %>% select(patid) %>% distinct()
#   ) %>%
#   left_join(ZAVUS24_Doses) %>%
#   group_by(patid) %>% arrange(patid, abs(from_dt)) %>%
#   mutate(patid=as.character(patid)) %>%
#   filter(from_dt!=0) %>%
#   filter(from_dt>=(-100) & from_dt<=100) %>%
#   slice(1:2) %>% mutate(from_dt=ifelse(from_dt<0,0,1)) %>%
#   distinct() %>% ungroup() %>%
#   spread(key=from_dt, value=qty) %>%
#   mutate(diff=`1`-`0`) %>%
#   ggplot(aes(diff)) +
#   geom_density()


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
RIME_pats <- MIGUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid) %>% distinct()
MIGUS24_Doses <- RIME_pats %>% left_join(MIGUS24_Doses) 
dim(MIGUS24_Doses)


RIME_MEDICATIONS <- fread("Source/RIME Medications.txt")
RIME_MEDICATIONS$med_code <- substr(RIME_MEDICATIONS$med_code, start = 3, stop = nchar(RIME_MEDICATIONS$med_code))
unique(RIME_MEDICATIONS$drug_group)
unique(RIME_MEDICATIONS$med_route)

RIME_MEDICATIONS <- RIME_MEDICATIONS %>% filter(drug_group=="Acute" & med_route=="NASAL") %>% 
  select(med_code) %>% distinct() %>% mutate(NasalTriptan="NasalTriptan") %>% rename("code"="med_code")

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(RIME_MEDICATIONS)  %>% filter(NasalTriptan=="NasalTriptan") %>% 
  group_by(patid) %>% filter(from_dt==min(from_dt)) %>%
 select(patid, from_dt) %>% rename("first_nasal_triptan"="from_dt") %>%
  left_join(MIGUS24_Doses) %>% select(patid, first_nasal_triptan, drug_id, generic, from_dt, days_sup, qty)


MIGUS24_Doses$first_nasal_triptan <- as.Date(MIGUS24_Doses$first_nasal_triptan)
MIGUS24_Doses$from_dt <- as.Date(MIGUS24_Doses$from_dt)

MIGUS24_Doses <- MIGUS24_Doses %>% filter(generic=="Rimegepant")
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt=from_dt-first_nasal_triptan) 
MIGUS24_Doses$from_dt <- as.numeric(MIGUS24_Doses$from_dt) 



range(MIGUS24_Doses$from_dt)

MIGUS24_Doses %>%
  filter(from_dt<0) %>% select(patid) %>% distinct() %>%
  inner_join(
    ZAVUS24_Doses %>%
  filter(from_dt>0) %>% select(patid) %>% distinct()
  ) %>%
  left_join(MIGUS24_Doses) %>%
  group_by(patid) %>% arrange(patid, abs(from_dt)) %>%
  mutate(patid=as.character(patid)) %>%
  filter(from_dt>=(-100) & from_dt<=100) %>%
  ggplot(aes(from_dt, qty)) +
  geom_smooth(fill="deepskyblue4", colour="firebrick") +
  #ylim(0,20) +
  theme_minimal() + 
  xlab("\n Number of days relative to FIRST ZAVEGEPANT") +
  ylab("Number of Rimegepant Pills per script \n")




MIGUS24_Doses %>%
  group_by(patid) %>% arrange(patid, abs(from_dt)) %>%
  mutate(patid=as.character(patid)) %>%
  filter(from_dt>=(-100) & from_dt<=100) %>%
  ggplot(aes(from_dt, qty)) +
  geom_smooth(fill="deepskyblue4", colour="firebrick") +
  ylim(0,20) +
  theme_minimal() + 
  xlab("\n Number of days relative to FIRST NASAL Triptan") +
  ylab("Number of Rimegepant Pills per script \n")




# --------------
# On month60 number of drugs etc ------------

Drug_formulary <- fread("Source/Drug_formulary.txt")

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, month60)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(month60!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, month60, sep = ",", convert=T )

N_drugs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid, weight) %>% count()

string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, month60)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(month60!="-")

MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Nasal, month60)) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Oral, month60)) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, month60)) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Acute, month60)) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Preventive, month60)) %>% summarise(n=sum(weight))


MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Nasal, month60)) %>% left_join(N_drugs) %>% summarise(mean=mean(n))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Oral, month60)) %>% left_join(N_drugs) %>% summarise(mean=mean(n))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, month60)) %>% left_join(N_drugs) %>% summarise(mean=mean(n))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Acute, month60)) %>% left_join(N_drugs) %>% summarise(mean=mean(n))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Preventive, month60)) %>% left_join(N_drugs) %>% summarise(mean=mean(n))



MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Nasal, month60)) %>% left_join(N_drugs) %>% 
  mutate(n=ifelse(n>=4,4,n)) %>% group_by(n) %>% summarise(pop=sum(weight)) %>%
  ungroup() %>% mutate(tot=sum(pop)) %>% mutate(pop=pop/tot)

MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Oral, month60)) %>% left_join(N_drugs) %>% 
  mutate(n=ifelse(n>=4,4,n)) %>% group_by(n) %>% summarise(pop=sum(weight)) %>%
  ungroup() %>% mutate(tot=sum(pop)) %>% mutate(pop=pop/tot)

MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, month60)) %>% left_join(N_drugs) %>% 
  mutate(n=ifelse(n>=4,4,n)) %>% group_by(n) %>% summarise(pop=sum(weight)) %>%
  ungroup() %>% mutate(tot=sum(pop)) %>% mutate(pop=pop/tot)

MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Acute, month60)) %>% left_join(N_drugs) %>% 
  mutate(n=ifelse(n>=4,4,n)) %>% group_by(n) %>% summarise(pop=sum(weight)) %>%
  ungroup() %>% mutate(tot=sum(pop)) %>% mutate(pop=pop/tot)

MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Preventive, month60)) %>% left_join(N_drugs) %>% 
  mutate(n=ifelse(n>=4,4,n)) %>% group_by(n) %>% summarise(pop=sum(weight)) %>%
  ungroup() %>% mutate(tot=sum(pop)) %>% mutate(pop=pop/tot)

# ------
# Nasal Triptan Sprays ON month 60 - Concomitant Classes and Stock before starting -------------

Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


# Concomitant Classes

MIGUS24_Flows_Long_version_NS <- fread("Source/MIGUS24 Flows_Long - version NS.txt")
length(unique(MIGUS24_Flows_Long_version_NS$patient))

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% filter(p2==60) %>% select(patient, weight, d2)

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% filter(d2!="-")
MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% filter(grepl(string_Triptan_Nasal, d2))
sum(MIGUS24_Flows_Long_version_NS$weight) # 39086.42

MIGUS24_Flows_Long_version_NS <- separate_rows(MIGUS24_Flows_Long_version_NS, d2, sep = ",", convert=T )

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% left_join(Drug_formulary, by=c("d2"="drug_id_2"))

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

MIGUS24_Flows_Long_version_NS <- MIGUS24_Demographics %>% inner_join(MIGUS24_Flows_Long_version_NS)

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% filter(flag==0) %>% select(-flag)


MIGUS24_Flows_Long_version_NS %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))

data.frame(MIGUS24_Flows_Long_version_NS %>% select(patient, weight, drug_class) %>% distinct() %>%
  group_by(drug_class) %>% summarise(n=sum(weight)/39086.42) %>% arrange(-n))


# Stock Before Start


MIGUS24_Flows_Long_version_NS <- fread("Source/MIGUS24 Flows_Long - version NS.txt")
length(unique(MIGUS24_Flows_Long_version_NS$patient))

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% filter(grepl(string_Triptan_Nasal, d2)&!grepl(string_Triptan_Nasal, d1))
MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% group_by(patient) %>% filter(p2==min(p2)) %>% ungroup()


 Mod_Sev  %>% inner_join(MIGUS24_Flows_Long_version_NS) %>% group_by(s1) %>% summarise(n=sum(weight))


MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% select(patient, weight, d1)

Mod_Sev <- fread("Source/Mod_Sev.txt")
MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
sum(MIGUS24_Demographics$weight)


Mod_Sev <- Mod_Sev %>% select(patid)  %>% rename("patient"="patid")

MIGUS24_Flows_Long_version_NS <- Mod_Sev  %>% inner_join(MIGUS24_Flows_Long_version_NS)

sum(MIGUS24_Flows_Long_version_NS$weight) # 277856.6

MIGUS24_Flows_Long_version_NS <- separate_rows(MIGUS24_Flows_Long_version_NS, d1, sep = ",", convert=T )

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% left_join(Drug_formulary, by=c("d1"="drug_id_2"))




MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

MIGUS24_Flows_Long_version_NS <- MIGUS24_Demographics %>% inner_join(MIGUS24_Flows_Long_version_NS)

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Flows_Long_version_NS <- MIGUS24_Flows_Long_version_NS %>% filter(flag==0) %>% select(-flag)


df <- MIGUS24_Flows_Long_version_NS %>% select(patient, weight, drug_group) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=drug_group, value=exp)
  
df[is.na(df)] <- 0
head(df)

sum(df$weight) # 208918.1


df %>% mutate(Box=ifelse(`CGRP Oral`==1,"CGRP_Oral",
                         ifelse(`CGRP Injectable`==1,"CGRP_Injectable",
                                ifelse(Preventive==1&Triptans==1,"PrevTriptan",
                                       ifelse(Preventive==1&Symptomatic==1,"PrevSympt",
                                              ifelse(Preventive==1, "Prev",
                                                     ifelse(Triptans==1,"Triptans",
                                                            ifelse(Symptomatic==1,"Symp",
                                                                   ifelse(`<NA>`==1,"None",NA))))))))) %>%
  group_by(Box) %>% summarise(n=sum(weight)) 
# 277856.6

# ----------
# Number of switches last 12 months --------------

Drug_formulary <- fread("Source/Drug_formulary.txt")

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, month60)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(month60!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, month60, sep = ",", convert=T )

N_drugs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid, weight) %>% count()

string_CGRP_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Nasal"], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, month60)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(month60!="-")

CGRP_Nasal <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Nasal, month60)) %>% select(patid)
CGRP_Oral <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(stringMIGUS24_Doses_CGRP_Oral, month60)) %>% select(patid)
CGRP_Inj <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, month60)) %>% select(patid)
Acute <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Acute, month60)) %>%  select(patid)
Preventive <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Preventive, month60)) %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, month48:month60)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month48:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  mutate(flag=ifelse(Treat!=lag(Treat),1,0))

MIGUS24_Drug_Histories_Extended_NoComorbs %>% ungroup() %>% filter(flag==1) %>%
  group_by(patid) %>% count() %>% ungroup() %>%
  inner_join(Preventive) %>% summarise(mean=mean(n))

# ---------
# Supply days and quantity of nasal triptan spray per year ---------
Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))

MIGUS24_Doses_version_NS %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
  group_by(patid, weight) %>%
  mutate(days_sup=sum(days_sup)) %>%
  mutate(qty=sum(qty))  %>%
  ungroup() %>%
  select(patid, days_sup, qty) %>% distinct() %>%
  summarise(days_sup=mean(days_sup), qty=mean(qty))

#   days_sup   qty
# 1     105.  30.8


MIGUS24_Doses_version_NS %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
   group_by(patid, weight) %>%
  mutate(days_sup=sum(days_sup)) %>%
  mutate(qty=sum(qty))  %>%
  ungroup() %>%
  select(patid, days_sup, qty) %>% distinct() %>%
  ggplot(aes(days_sup)) +
  geom_density(colour="navy", size=2) +
  theme_minimal() +
  xlab("\n Number of Supply Days per Year") +
  ylab("Patient density \n")



MIGUS24_Doses_version_NS %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
   group_by(patid, weight) %>%
  mutate(days_sup=sum(days_sup)) %>%
  mutate(qty=sum(qty))  %>%
  ungroup() %>%
  select(patid, days_sup, qty) %>% distinct() %>%
  ggplot(aes(qty)) +
  geom_density(colour="firebrick", size=2) +
  theme_minimal() +
  xlab("\n Number of doses per Year") +
  ylab("Patient density \n")


Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))


MIGUS24_Doses_version_NS %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
  mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  select(patid, weight, from_dt) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>%
  summarise(mean=median(n))

MIGUS24_Doses_version_NS %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
  mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  select(patid, weight, from_dt) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>%
  ggplot(aes(n)) +
  geom_density(colour="firebrick", size=2) +
  theme_minimal() +
  xlab("\n Number of Different Months Used Nasal Triptan \n (NOT necessarily contiguously)") +
  ylab("Patient density \n")





# ----------
# Average pills per month Nasal triptan spray ---------

Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())
string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS  %>% filter(status != "G") %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()
range(MIGUS24_Doses_version_NS$from_dt)
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2023-01-16") %>% filter(days_sup  != "")
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(-drug_id_2) %>% distinct()
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% arrange(patid, from_dt) 
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% drop_na()
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(elapsed <= 92)
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% mutate(rate=30*(as.numeric(qty)/elapsed))


MIGUS24_Doses_version_NS %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight) %>% summarise(rate=mean(rate)) %>%
  ungroup() %>% summarise(mean=mean(rate))


MIGUS24_Doses_version_NS %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, drug_id_2) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(drug_id_2, mean) %>% summarise(n=sum(as.numeric(weight)))



MIGUS24_Doses_version_NS %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, drug_id_2) %>% summarise(mean=mean(rate)) %>%
  ggplot(aes(mean)) +
  geom_density(fill="deepskyblue4", alpha=0.8) +
  theme_minimal() +
  ylab("Patient density \n") + xlab("\n Number of Nasal Spray Doses per month")

# ----------------
# Number of distinct months on nasal triptan vs yearly doses ----------
Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))

Months_ON_Nasal <- MIGUS24_Doses_version_NS %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
  mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  select(patid, weight, from_dt) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>% rename("Months"="n")

Months_ON_Nasal <- MIGUS24_Doses_version_NS %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
  mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  select(patid, weight, from_dt) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>% rename("Months"="n")



MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))


QTY_ON_Nasal <- MIGUS24_Doses_version_NS %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
   group_by(patid, weight) %>%
  mutate(days_sup=sum(days_sup)) %>%
  mutate(qty=sum(qty))  %>%
  ungroup() %>%
  select(patid, days_sup, qty) %>% distinct()


df <- Months_ON_Nasal %>% inner_join(QTY_ON_Nasal %>% select(-days_sup))


df %>% mutate(div=qty/Months) %>% group_by(Months) %>% summarise(mean=mean(div))


max(df$qty) # 74

df$group <- as.numeric(cut(df$qty,10))


df %>%
  group_by(group, Months) %>% summarise(n=sum(weight)) %>%
  spread(key=group, value=n)


# -----------

# In which months are patients using triptan sprays ? ------------
Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))

MIGUS24_Doses_version_NS


MIGUS24_Doses_version_NS %>% 
  mutate(from_dt=str_sub(as.character(from_dt), 6L,7L)) %>%
  group_by(from_dt) %>% summarise(tot=sum(weight))


data.frame(MIGUS24_Doses_version_NS %>% 
  mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  group_by(from_dt) %>% summarise(tot=sum(weight))) %>%
  ggplot(aes(from_dt, tot)) +
  geom_point() +
  geom_smooth()

# -------
# Are the triptan sprays concentrated or sparsed ? ------------
Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))

MIGUS24_Doses_version_NS


MIN <- MIGUS24_Doses_version_NS %>%
  group_by(patid) %>% filter(from_dt==min(from_dt)) %>% select(patid, from_dt) %>% distinct() %>%
  rename("MIN"="from_dt")

MAX <- MIGUS24_Doses_version_NS %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>% select(patid, from_dt) %>% distinct() %>%
  rename("MAX"="from_dt")


df <- MIN %>% inner_join(MAX)

df$Diff <- as.numeric(as.Date(df$MAX) - as.Date(df$MIN)) / 30.44

N_Months <- MIGUS24_Doses_version_NS %>%
  select(patid, from_dt) %>% mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  distinct() %>% group_by(patid) %>% count() 



N_Months %>% inner_join(df) %>%
  ggplot(aes(Diff, n)) +
  geom_jitter(size=1, alpha=0.5, colour="deepskyblue4") +
  theme_minimal() +
  xlab("\n Number of Elapsed Months \n From First to Last Month of Nasal Triptan Spray") +
  ylab("Number of Months \n USING Nasal Triptan Spray\n ")



N_Months %>% inner_join(df) %>% mutate(patid=as.character(patid)) %>%
  mutate(n=n-1) %>%
  mutate(perc=n/Diff) %>%
  filter(Diff>0&perc<1) %>% 
  ggplot(aes(perc)) +
  geom_density(colour="firebrick", fill="firebrick", alpha=0.5) + 
  theme_minimal() +
  xlab("\n Proportion of First-to-Last Months \n Actually Spent ON of Nasal Triptan Spray") +
  ylab("Patient density\n ")



N_Months %>% inner_join(df) %>% mutate(patid=as.character(patid)) %>%
  mutate(n=n-1) %>%
  mutate(perc=n/Diff) %>%
  filter(Diff>0&perc<1) %>% ungroup() %>% summarise(mean=mean(perc))

# -------
# Are the triptan ORALS concentrated or sparsed ? ------------

Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_group_2, drug_group) %>% distinct())

string_Triptan_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Triptans" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Oral, drug_id_2))

MIGUS24_Doses_version_NS


MIN <- MIGUS24_Doses_version_NS %>%
  group_by(patid) %>% filter(from_dt==min(from_dt)) %>% select(patid, from_dt) %>% distinct() %>%
  rename("MIN"="from_dt")

MAX <- MIGUS24_Doses_version_NS %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>% select(patid, from_dt) %>% distinct() %>%
  rename("MAX"="from_dt")


df <- MIN %>% inner_join(MAX)

df$Diff <- as.numeric(as.Date(df$MAX) - as.Date(df$MIN)) / 30.44

N_Months <- MIGUS24_Doses_version_NS %>%
  select(patid, from_dt) %>% mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  distinct() %>% group_by(patid) %>% count() 



N_Months %>% inner_join(df) %>%
  ggplot(aes(Diff, n)) +
  geom_jitter(size=0.3, alpha=0.25, colour="deepskyblue4") +
  theme_minimal() +
  xlab("\n Number of Elapsed Months \n From First to Last Month of Oral Triptan") +
  ylab("Number of Months \n USING Oral Triptan\n ")



N_Months %>% inner_join(df) %>% mutate(patid=as.character(patid)) %>%
  mutate(n=n-1) %>%
  mutate(perc=n/Diff) %>%
  filter(Diff>0&perc<1) %>% 
  ggplot(aes(perc)) +
  geom_density(colour="firebrick", fill="firebrick", alpha=0.5) + 
  theme_minimal() +
  xlab("\n Proportion of First-to-Last Months \n Actually Spent ON on Oral Triptan") +
  ylab("Patient density\n ")



N_Months %>% inner_join(df) %>% mutate(patid=as.character(patid)) %>%
  mutate(n=n-1) %>%
  mutate(perc=n/Diff) %>%
  filter(Diff>0&perc<1) %>% ungroup() %>% summarise(mean=mean(perc))

# -----------------
# Are the CGRP ORALS concentrated or sparsed ? ------------

Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_group_2, drug_group) %>% distinct())

string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="CGRP Oral" &
                                                                      Drug_formulary$drug_group=="CGRP Oral"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_CGRP_Oral, drug_id_2))

MIGUS24_Doses_version_NS


MIN <- MIGUS24_Doses_version_NS %>%
  group_by(patid) %>% filter(from_dt==min(from_dt)) %>% select(patid, from_dt) %>% distinct() %>%
  rename("MIN"="from_dt")

MAX <- MIGUS24_Doses_version_NS %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>% select(patid, from_dt) %>% distinct() %>%
  rename("MAX"="from_dt")


df <- MIN %>% inner_join(MAX)

df$Diff <- as.numeric(as.Date(df$MAX) - as.Date(df$MIN)) / 30.44

N_Months <- MIGUS24_Doses_version_NS %>%
  select(patid, from_dt) %>% mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  distinct() %>% group_by(patid) %>% count() 



N_Months %>% inner_join(df) %>%
  ggplot(aes(Diff, n)) +
  geom_jitter(size=0.5, alpha=0.5, colour="deepskyblue4") +
  theme_minimal() +
  xlab("\n Number of Elapsed Months \n From First to Last Month of Oral CGRP") +
  ylab("Number of Months \n USING Oral CGRP\n ")



N_Months %>% inner_join(df) %>% mutate(patid=as.character(patid)) %>%
  mutate(n=n-1) %>%
  mutate(perc=n/Diff) %>%
  filter(Diff>0&perc<1) %>% 
  ggplot(aes(perc)) +
  geom_density(colour="firebrick", fill="firebrick", alpha=0.5) + 
  theme_minimal() +
  xlab("\n Proportion of First-to-Last Months \n Actually Spent ON on Oral CGRP") +
  ylab("Patient density\n ")



N_Months %>% inner_join(df) %>% mutate(patid=as.character(patid)) %>%
  mutate(n=n-1) %>%
  mutate(perc=n/Diff) %>%
  filter(Diff>0&perc<1) %>% ungroup() %>% summarise(mean=mean(perc))

# -----------------
# Generics Before vs After First Zavegepant ------------------

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
  select(patient, generic) %>% distinct() %>% group_by(generic) %>% count() %>% mutate(n=n/length(unique(before$patient))) %>%
  rename("before"="n") %>%
  full_join(
    current %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, generic) %>% distinct() %>% group_by(generic) %>% count() %>% mutate(n=n/length(unique(current$patient))) %>%
  rename("current"="n")) %>%
  full_join(
    after %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, generic) %>% distinct() %>% group_by(generic) %>% count() %>% mutate(n=n/length(unique(after$patient))) %>%
  rename("after"="n")
  ))

# ---------------
# Source of riemgepant over time ------------
MIGUS24_Flows_Long <- fread("Source/MIGUS24 Flows_Long.txt")

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% filter(grepl("136", d2)&!grepl("136", d1))

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% select(patient, weight, d1, p2)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)  %>% rename("patient"="patid")

MIGUS24_Flows_Long <- Mod_Sev  %>% inner_join(MIGUS24_Flows_Long)

MIGUS24_Flows_Long <- separate_rows(MIGUS24_Flows_Long, d1, sep = ",", convert=T )

Drug_formulary <- fread("Source/Drug_formulary.txt")

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)), by=c("d1"="drug_id"))


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

MIGUS24_Flows_Long <- MIGUS24_Demographics %>% inner_join(MIGUS24_Flows_Long)

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% filter(flag==0) %>% select(-flag)


df <- MIGUS24_Flows_Long %>% select(patient, weight, p2, drug_group) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=drug_group, value=exp)
  
df[is.na(df)] <- 0
head(df)



data.frame(df %>% mutate(Box=ifelse(`CGRP Oral`==1,"CGRP_Oral",
                         ifelse(`CGRP Injectable`==1,"CGRP_Injectable",
                                ifelse(Preventive==1&Triptans==1,"PrevTriptan",
                                       ifelse(Preventive==1&Symptomatic==1,"PrevSympt",
                                              ifelse(Preventive==1, "Prev",
                                                     ifelse(Triptans==1,"Triptans",
                                                            ifelse(Symptomatic==1,"Symp",
                                                                   ifelse(`<NA>`==1,"None",NA))))))))) %>%
  group_by(p2, Box) %>% summarise(n=sum(weight)) %>% spread(key=Box, value=n))

# ------------------------

# Source of riemgepant over time Acute vs Prev ------------



MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")

RIME_pats <- MIGUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid) %>% distinct()
MIGUS24_Doses <- RIME_pats %>% left_join(MIGUS24_Doses) %>% filter(generic=="Rimegepant")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
range(MIGUS24_Doses$from_dt)
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2020-01-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% arrange(patid, generic, from_dt) 
MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
MIGUS24_Doses <- MIGUS24_Doses %>% drop_na()
MIGUS24_Doses <- MIGUS24_Doses %>% filter(elapsed <= 92)
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

groups <- MIGUS24_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  select(patid, mean) %>% distinct()



MIGUS24_Flows_Long <- fread("Source/MIGUS24 Flows_Long.txt")

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% filter(grepl("136", d2)&!grepl("136", d1))

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% select(patient, weight, d1, p2)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)  %>% rename("patient"="patid")

MIGUS24_Flows_Long <- Mod_Sev  %>% inner_join(MIGUS24_Flows_Long)

MIGUS24_Flows_Long <- separate_rows(MIGUS24_Flows_Long, d1, sep = ",", convert=T )

Drug_formulary <- fread("Source/Drug_formulary.txt")

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)), by=c("d1"="drug_id"))


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

MIGUS24_Flows_Long <- MIGUS24_Demographics %>% inner_join(MIGUS24_Flows_Long)

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% filter(flag==0) %>% select(-flag)


df <- MIGUS24_Flows_Long %>% select(patient, weight, p2, drug_group) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=drug_group, value=exp)
  
df[is.na(df)] <- 0
head(df)



data.frame(df %>% inner_join(groups %>% ungroup() %>% select(-c(weight)) %>% filter(mean=="Acute"), by=c("patient"="patid")) %>% mutate(Box=ifelse(`CGRP Oral`==1,"CGRP_Oral",
                         ifelse(`CGRP Injectable`==1,"CGRP_Injectable",
                                ifelse(Preventive==1&Triptans==1,"PrevTriptan",
                                       ifelse(Preventive==1&Symptomatic==1,"PrevSympt",
                                              ifelse(Preventive==1, "Prev",
                                                     ifelse(Triptans==1,"Triptans",
                                                            ifelse(Symptomatic==1,"Symp",
                                                                   ifelse(`<NA>`==1,"None",NA))))))))) %>%
  group_by(p2, Box) %>% summarise(n=sum(weight)) %>% spread(key=Box, value=n))

# ------------


# Supply days and quantity of Zavegepant  per year ---------
Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary %>% filter(generic_name=="Zavegepant")

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
names(ZAVUS24_Doses)
unique(ZAVUS24_Doses$status)

ZAVUS24_Doses <- ZAVUS24_Doses %>% select(drug_id, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(ZAVUS24_Doses$from_dt)

ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(grepl("134", drug_id))

ZAVUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
  group_by(patid, weight) %>%
  mutate(days_sup=sum(days_sup)) %>%
  mutate(qty=sum(qty))  %>%
  ungroup() %>%
  select(patid, days_sup, qty) %>% distinct() %>%
  summarise(days_sup=mean(days_sup), qty=mean(qty))

#  days_sup   qty
#1     52.4  12.3


ZAVUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
   group_by(patid, weight) %>%
  mutate(days_sup=sum(days_sup)) %>%
  mutate(qty=sum(qty))  %>%
  ungroup() %>%
  select(patid, days_sup, qty) %>% distinct() %>%
  ggplot(aes(days_sup)) +
  geom_density(colour="navy", size=2) +
  theme_minimal() +
  xlab("\n Number of Supply Days per Year") +
  ylab("Patient density \n")



ZAVUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
   group_by(patid, weight) %>%
  mutate(days_sup=sum(days_sup)) %>%
  mutate(qty=sum(qty))  %>%
  ungroup() %>%
  select(patid, days_sup, qty) %>% distinct() %>%
  ggplot(aes(qty)) +
  geom_density(colour="firebrick", size=2) +
  theme_minimal() +
  xlab("\n Number of doses per Year") +
  ylab("Patient density \n")


Drug_formulary <- fread("Source/Drug_formulary.txt")

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")


ZAVUS24_Doses <- ZAVUS24_Doses %>% select(drug_id, patid, weight, from_dt, days_sup, qty) %>% distinct()


ZAVUS24_Doses <- ZAVUS24_Doses %>% filter(grepl("134", drug_id))


ZAVUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
  mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  select(patid, weight, from_dt) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>%
  summarise(mean=mean(n))

ZAVUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>%
  filter(from_dt>="2023-01-16") %>%
  mutate(from_dt=str_sub(as.character(from_dt), 1L,7L)) %>%
  select(patid, weight, from_dt) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>%
  ggplot(aes(n)) +
  geom_density(colour="firebrick", size=2) +
  theme_minimal() +
  xlab("\n Number of Different Months Used Zavegepant \n (NOT necessarily contiguously)") +
  ylab("Patient density \n")

# ----------




# Forecast Oral CGRP Population -----------

Drug_formulary <- fread("Source/Drug_formulary.txt")
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Oral"], collapse = "|"),")\\b")

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-")
CGRP_Oral_Pop <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Oral, Treat)) %>% 
  select(patid, weight, Month) %>% group_by(Month) %>% summarise(n=sum(weight))



poly_model <- lm(n ~ poly(Month, 3, raw = TRUE), data = CGRP_Oral_Pop)

future_months <- data.frame(Month = 61:120)  

predicted_population <- predict(poly_model, newdata = future_months)

forecast_df <- data.frame(Month = c(CGRP_Oral_Pop$Month, future_months$Month),
                          population = c(CGRP_Oral_Pop$n, predicted_population),
                          type = c(rep("Actual", nrow(CGRP_Oral_Pop)), rep("Forecast", nrow(future_months))))


ggplot(forecast_df, aes(x = Month, y = population, color = type)) +
  geom_line(size=3, alpha=0.5) +
  labs(title = "Oral CGRP Population Forecast",
       x = " \n Exact Month",
       y = "(Expected) Population \n") +
  theme_minimal() +
  scale_color_manual(values = c("deepskyblue4", "firebrick"))


install.packages("prophet")
library(prophet)


dates <- data.frame(date_range <- seq.Date(from = as.Date("2019-08-01"), to = as.Date("2023-05-01"), by = "month"))
names(dates) <- "Months_2"

CGRP_Oral_Pop <- CGRP_Oral_Pop %>% bind_cols(dates)
CGRP_Oral_Pop <- CGRP_Oral_Pop %>% select(Months_2, n)
names(CGRP_Oral_Pop) <- c("ds", "y")


prophet_model <- prophet(CGRP_Oral_Pop)

future_months <- data.frame(ds = seq(as.Date(paste0(max(CGRP_Oral_Pop$ds), "-01")), by = "month", length.out = 60)) 

forecast <- predict(prophet_model, future_months)

plot(prophet_model, forecast)

plot(prophet_model, forecast) +
  theme_minimal() +  
  xlab(" \n Date") + ylab("Expected Population \n") +
  ylim(0,2500000) +
  geom_point(data = forecast$y, aes(x = ds, y = y), color = "firebrick", size = 3, shape = 1)










# -------
# Distance between triptan sprays scripts ? ------------
Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight, from_dt, days_sup, qty) %>% distinct()

range(MIGUS24_Doses_version_NS$from_dt)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))

MIGUS24_Doses_version_NS

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(patid, from_dt, days_sup, qty) %>%
  arrange(patid, from_dt, days_sup, qty)

MIGUS24_Doses_version_NS %>% mutate(from_dt=as.numeric(from_dt)) %>% mutate(patid=as.character(patid)) %>%
  group_by(patid) %>% mutate(elapsed=from_dt-lag(from_dt)) %>% drop_na() %>%
  mutate(mean=mean(elapsed)) %>% ungroup() %>%
  # summarise(mean=mean(elapsed), median=median(elapsed)) %>%
  ggplot(aes(elapsed)) + 
  xlim(0,365) +
  geom_density(size=2, colour="deepskyblue4") +
  theme_minimal() +
  ylab("Patient density \n") + xlab("\n Number of elapsed days between scripts")



MIGUS24_Doses_version_NS %>% mutate(from_dt=as.numeric(from_dt)) %>% mutate(patid=as.character(patid)) %>%
  group_by(patid) %>% mutate(elapsed=from_dt-lag(from_dt)) %>% drop_na() %>%
  mutate(mean=mean(elapsed)) %>% ungroup() %>%
  select(patid, mean) %>% distinct() %>%
  #summarise( median=mean(mean)) %>%
  ggplot(aes(mean)) + 
  xlim(0,365) +
  geom_density(size=2, colour="firebrick") +
  theme_minimal() +
  ylab("Patient density \n") + xlab("\n Number of elapsed days between scripts \n Mean per patient")

  
  
# -------------------------------

# Forecast Oral CGRP Population Ever tried previous 12 months -----------

Drug_formulary <- fread("Source/Drug_formulary.txt")
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Oral"], collapse = "|"),")\\b")

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-")

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  mutate(ON=ifelse(grepl(string_CGRP_Oral, Treat), 1,0)) %>% arrange(patid, Month) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  mutate(moving_sum = cumsum(ON) - lag(cumsum(ON), 12, default = 0))

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  mutate(moving_sum=ifelse(moving_sum==0,0,1))


CGRP_Oral_Pop <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% ungroup()  %>% 
  select(patid, weight, Month, moving_sum) %>% filter(moving_sum==1) %>% group_by(Month) %>% summarise(n=sum(weight))

CGRP_Oral_Pop %>% ggplot(aes(Month, n)) + geom_line()


poly_model <- lm(n ~ poly(Month, 3, raw = TRUE), data = CGRP_Oral_Pop)

future_months <- data.frame(Month = 61:120)  

predicted_population <- predict(poly_model, newdata = future_months)

forecast_df <- data.frame(Month = c(CGRP_Oral_Pop$Month, future_months$Month),
                          population = c(CGRP_Oral_Pop$n, predicted_population),
                          type = c(rep("Actual", nrow(CGRP_Oral_Pop)), rep("Forecast", nrow(future_months))))


ggplot(forecast_df, aes(x = Month, y = population, color = type)) +
  geom_line(size=3, alpha=0.5) +
  labs(title = "Oral CGRP Population Forecast",
       x = " \n Exact Month",
       y = "(Expected) Population \n") +
  theme_minimal() +
  scale_color_manual(values = c("deepskyblue4", "firebrick"))


install.packages("prophet")
library(prophet)


dates <- data.frame(date_range <- seq.Date(from = as.Date("2019-08-01"), to = as.Date("2023-05-01"), by = "month"))
names(dates) <- "Months_2"

CGRP_Oral_Pop <- CGRP_Oral_Pop %>% bind_cols(dates)
CGRP_Oral_Pop <- CGRP_Oral_Pop %>% select(Months_2, n)
names(CGRP_Oral_Pop) <- c("ds", "y")

prophet_model <- prophet(CGRP_Oral_Pop)

future_months <- data.frame(ds = seq(as.Date(paste0(max(CGRP_Oral_Pop$ds), "-01")), by = "month", length.out = 60)) 

forecast <- predict(prophet_model, future_months)

plot(prophet_model, forecast)

plot(prophet_model, forecast) +
  theme_minimal() +  
  xlab(" \n Date") + ylab("Expected Population \n") +
  geom_point(data = forecast$y, aes(x = ds, y = y), color = "firebrick", size = 3, shape = 1)




# -------------






# Number of migriane days using script dates last 12 months --------------

Drug_formulary <- fread("Source/Drug_formulary.txt")

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, month60)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(month60!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, month60, sep = ",", convert=T )

string_CGRP_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Nasal"], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, month60)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(month60!="-")

CGRP_Nasal <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Nasal, month60)) %>% select(patid)
CGRP_Oral <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Oral, month60)) %>% select(patid)
CGRP_Inj <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, month60)) %>% select(patid)
Acute <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Acute, month60)) %>%  select(patid)
Preventive <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Preventive, month60)) %>% select(patid)

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(status!="G") %>% select(-c(code, drug_group, qty, NPI, provider, provcat, status))

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 

MIGUS24_Doses <- MIGUS24_Demographics %>% inner_join(MIGUS24_Doses)


MIGUS24_Doses <- MIGUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(flag==0) %>% select(-flag)
MIGUS24_Doses <- MIGUS24_Doses %>% select(-c(CV, psychiatric, epileptic))
MIGUS24_Doses$from_dt <- as.Date(MIGUS24_Doses$from_dt)

MIGUS24_Doses$dest <- MIGUS24_Doses$from_dt + MIGUS24_Doses$days_sup
MIGUS24_Doses <- MIGUS24_Doses %>% filter(from_dt>="2023-01-16")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, weight, from_dt, dest) %>% distinct() 


MIN <- MIGUS24_Doses %>% group_by(patid) %>% filter(from_dt==min(from_dt)) %>% distinct() %>% select(patid, from_dt) %>% distinct()

MAX <- MIGUS24_Doses %>% group_by(patid) %>% filter(dest==max(dest)) %>% distinct() %>% select(patid, dest) %>% distinct()

df <- MIN %>% inner_join(MAX)

MIGUS24_Doses <- MIGUS24_Doses %>% select(-weight) %>% distinct() %>%  mutate(patid=as.character(patid))



# CGRP_Nasal (388) CGRP_Oral (359) CGRP_Inj (397) Acute (313) Preventive (359) 
# CGRP_Nasal (372) CGRP_Oral (335) CGRP_Inj (379) Acute (274) Preventive (337) 

# CGRP_Nasal (349) CGRP_Oral (312) CGRP_Inj  (353) Acute (256) Preventive (314) 


generate_dates <- function(start_date, end_date) {
  seq(start_date, end_date, by = "day")
}


df_2 <- df %>% inner_join(CGRP_Nasal)

names(df_2) <- c("patid", "MIN", "MAX")

df_2 <- df_2 %>% mutate(patid=as.character(patid )) %>% distinct() %>%
  group_by(patid) %>%
  summarise(dates = list(generate_dates(min(MIN), max(MAX)))) %>%
  unnest(dates) %>% ungroup() %>% distinct()


df_3 <- df_2 %>%
  inner_join(MIGUS24_Doses %>% inner_join(CGRP_Nasal %>% mutate(patid=as.character(patid)))) %>%
  filter(dates >= from_dt & dates <= dest) %>%
  ungroup()


df_3 %>% select(patid, dates) %>% distinct() %>% 
  group_by(patid) %>% count() %>% 
  ungroup() %>% summarise(mean=mean(n))

# -----------

# Age of Triptan nasal Spray -----

Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses_version_NS <- fread("Source/MIGUS24 Doses - version NS.txt")
names(MIGUS24_Doses_version_NS)
unique(MIGUS24_Doses_version_NS$status)

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(drug_id_2, patid, weight) %>% distinct()

MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% filter(grepl(string_Triptan_Nasal, drug_id_2))
MIGUS24_Doses_version_NS <- MIGUS24_Doses_version_NS %>% select(patid, weight) %>% distinct()

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, AGE) 

MIGUS24_Demographics %>% inner_join(MIGUS24_Doses_version_NS) %>% summarise(mean=weighted.mean(AGE, weight))

# ---------
# Visiblity vs Number scripts rimegepant 2021 ----------
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(generic=="Rimegepant")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, from_dt)
range(MIGUS24_Doses$from_dt)

MIGUS24_Doses %>% group_by(patid) %>% count()
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% group_by(patid) %>% mutate(first=min(from_dt))

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% 
  filter(from_dt<="2023-12-31") %>%
  filter(first<="2023-12-31") %>%
     filter(first>="2023-01-01") %>%
  filter(from_dt>="2023-01-01")

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(VIZ = as.numeric(as.Date("2023-12-31") - first)/30.5)  

MIGUS24_Doses <- MIGUS24_Doses %>% ungroup() %>% mutate(patid=as.character(patid)) %>% filter(from_dt>=first)

range(MIGUS24_Doses$VIZ)

MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, VIZ) %>% count() %>% 
  mutate(VIZ=round(VIZ)) %>% rename("SCRIPTS"="n")

max(MIGUS24_Doses$VIZ) # 6

MIGUS24_Doses %>% group_by(VIZ) %>% count() 

length(unique(MIGUS24_Doses$patid))


data.frame(MIGUS24_Doses %>%
  group_by(VIZ, SCRIPTS) %>% count() %>%
  spread(key=VIZ, value=n))


# ----------------
# For how many months have the been lapsed looking back ? -------

Mod_Sev <- fread("Source/Mod_Sev.txt")
unique(Mod_Sev$group)

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
sum(MIGUS24_Drug_Histories_Extended_NoComorbs$weight)
# 12768773 21292150
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs # %>% inner_join(Mod_Sev) 
# MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-group)

MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, desc(Month))
 
length(unique(MIGUS24_Drug_Histories_Extended_NoComorbs$patid))

MIGUS24_Drug_Histories_Extended_NoComorbs <- data.frame(MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% 
  slice(if(any(grepl("-",Treat))) 1:which.max(!grepl("-",Treat)) else row_number())   %>%
    ungroup() %>% filter(Treat=="-"))

data.frame(MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% summarise(tot=sum(as.numeric(weight))))


# -----------

# Comorbs ZAV vs ALL MIG 24 Ext ----------
ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")
mean(ZAVUS24_Demographics$age) # 52
ZAVUS24_Demographics %>% group_by(gender) %>% count() #87% F
ZAVUS24_Demographics %>% group_by(CV) %>% count() #52%
ZAVUS24_Demographics %>% group_by(epileptic) %>% count() #9%
ZAVUS24_Demographics %>% group_by(psychiatric) %>% count() #66%


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>% anti_join(ZAVUS24_Demographics %>% select(patid))
mean(MIGUS24_Demographics$AGE) # 60
MIGUS24_Demographics %>% group_by(GENDER) %>% count() #76% F
MIGUS24_Demographics %>% group_by(CV) %>% count() # 68%
MIGUS24_Demographics %>% group_by(epileptic) %>% count() # 6%
MIGUS24_Demographics %>% group_by(psychiatric) %>% count() # 63%

ZAVUS24_Comorbidities_Extended_Dxs <- fread("Source/ZAVUS24 Comorbidities Extended Dxs.txt")
ZAVUS24_Comorbidities_Extended_Dxs <- ZAVUS24_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()
ZAV_Dxs <- ZAVUS24_Comorbidities_Extended_Dxs %>% group_by(ICD10_diag) %>% count() %>% mutate(n=n/260) %>% rename("ZAV"="n") %>% rename("ICD10_diag"="ICD10_diag")

MIGUS24_Comorbidities_Extended_Dxs <- fread("Source/MIGUS24 Comorbidities Extended Dxs.txt")
length(unique(MIGUS24_Comorbidities_Extended_Dxs$patid))
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% select(patid, ICD10_diag) %>% distinct()
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% anti_join(ZAVUS24_Demographics %>% select(patid))
length(unique(MIGUS24_Comorbidities_Extended_Dxs$patid)) # 262157
ALL_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% group_by(ICD10_diag) %>% count() %>% mutate(n=n/252555) %>% rename("ALL"="n")


data.frame(ZAV_Dxs %>% inner_join(ALL_Dxs) %>% mutate(diff=abs(ZAV-ALL))) %>% arrange(desc(diff)) %>%
  filter(diff>0.05)

# -------
# Oral CGRP usage among Triptan patients pathways Bella All MIG --------

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
sum(MIGUS24_Drug_Histories_Extended_NoComorbs$weight)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, desc(Month))
# 21292150

Drug_formulary <- fread("Source/Drug_formulary.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class=="Triptan"], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class=="CGRP Oral"], collapse = "|"),")\\b")


Triptan_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, Treat) %>% distinct() %>%
  filter(grepl(string_Triptan, Treat)) %>% select(patid, weight)  %>% distinct() 

sum(Triptan_pats$weight) # 8788198 ever 

MIGUS24_Drug_Histories_Extended_NoComorbs <- Triptan_pats %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs)

Triptan_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%  filter(grepl(string_Triptan, Treat)) %>%
  group_by(patid, weight)  %>% filter(Month==min(Month)) %>%
  select(patid, weight, Month) %>% filter(Month<=36)

names(Triptan_pats)[3] <- "First_Triptan"
sum(Triptan_pats$weight) # 6773084 first 3 years

Oral_CGRP_pats <- Triptan_pats %>% 
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  filter(Month>=First_Triptan) %>%
  filter(grepl(string_CGRP_Oral, Treat)) %>% select(patid, weight)  %>% distinct() 

sum(Oral_CGRP_pats$weight) # 633479.2 ( 633479.2/6773084 = 0.09352892)


Oral_CGRP_pats <- Triptan_pats %>% 
  inner_join(Oral_CGRP_pats) %>%
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  filter(Month>=First_Triptan) %>%
  filter(grepl(string_CGRP_Oral, Treat)) %>% filter(Month==min(Month)) %>%
  select(patid, weight, Month) 


names(Oral_CGRP_pats)[3] <- "First_Oral_CGRP"
sum(Oral_CGRP_pats$weight) # 633479.2 

Oral_CGRP_pats %>% inner_join(Triptan_pats) %>% mutate(Elapsed=First_Oral_CGRP-First_Triptan) %>%
  ungroup() %>% 
  # summarise(mean=mean(Elapsed)) %>% # 32.4
  # summarise(median=median(Elapsed)) # 33
  ggplot(aes(Elapsed)) +
  geom_density(colour="black", size=2, fill="black", alpha=0.5) +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number Elapsed Months Between Start Triptan & Start Oral CGRP") +
  ylab("Patient density \n")


MIGUS24_Drug_Histories_Extended_NoComorbs <- Oral_CGRP_pats %>% inner_join(Triptan_pats)  %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month>=First_Triptan&Month<=First_Oral_CGRP) 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%mutate(Elapsed=First_Oral_CGRP-First_Triptan) 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-group)


MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Triptan, Treat)) %>%
  group_by(patid, weight, Elapsed) %>% count() %>%
  ggplot(aes(Elapsed, n)) +
  geom_jitter(size=0.5, alpha=0.5) +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number Elapsed Months Between Start Triptan & Start Oral CGRP") +
  ylab("Number of Months ON Triptan \n")


MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Triptan, Treat)) %>%
  group_by(patid, weight, Elapsed) %>% count() %>% ungroup() %>% #  summarise(mean=mean(n)) # 16.3
  mutate(perc=n/Elapsed) %>% filter(Elapsed!=0) %>% # summarise(mean=mean(perc)) # 53% 
  ggplot(aes(Elapsed, n)) +
  geom_jitter(size=0.5, alpha=0.5) +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number Elapsed Months Between Start Triptan & Start Oral CGRP") +
  ylab("Number of Months ON Triptan \n")


MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Triptan, Treat)) %>%
  group_by(patid, weight, Elapsed) %>% count() %>% ungroup() %>% #  summarise(mean=mean(n)) # 16.3
  mutate(perc=n/Elapsed) %>% filter(Elapsed!=0) %>% # summarise(mean=mean(perc)) # 53% 
  ggplot(aes(perc)) +
  xlim(0,1) +
  geom_density(colour="deepskyblue4", size=1, fill="deepskyblue4", alpha=0.3) +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Proportion of the Months Between Start Triptan & Start Oral CGRP \n Truly Spent ON Triptan") +
  ylab("Patient Density \n")


MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl("-", Treat)) %>%
  group_by(patid, weight, Elapsed) %>% count() %>% ungroup() %>%
   ggplot(aes(n)) +
  geom_density(colour="black", size=1, fill="black", alpha=0.3) +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Numbr of  Months Between Start Triptan & Start Oral CGRP \n Lapsed (no therapy)") +
  ylab("Patient Density \n")


Diff_Triptans <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Triptan, Treat)) %>%
  group_by(patid, weight, Treat) 

Diff_Triptans <- separate_rows(Diff_Triptans, Treat, sep = ",", convert=T )
Diff_Triptans <- Diff_Triptans%>% filter(grepl(string_Triptan, Treat))
Diff_Triptans <- Diff_Triptans %>% ungroup() %>% select(patid, weight, Treat) %>% distinct() %>%
  group_by(patid, weight) %>% count()

Diff_Triptans %>% ungroup() %>% summarise(mean=mean(n))


Classes_Tried <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(!grepl("-", Treat)) %>%
  group_by(patid, weight, Treat) %>% distinct()

Classes_Tried <- separate_rows(Classes_Tried, Treat, sep = ",", convert=T )
Classes_Tried <- Classes_Tried%>%   select(patid, weight, Treat) %>% distinct()

data.frame(Classes_Tried %>% left_join(Drug_formulary %>% select(drug_id, drug_class), by=c("Treat"="drug_id")) %>%
  ungroup() %>% select(patid, weight, drug_class) %>% distinct() %>%
  group_by(drug_class) %>% summarise(tot=sum(weight)))


Boxes_Tried <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(!grepl("-", Treat)) %>%
  group_by(patid, weight, Month, Treat) %>% distinct()

Boxes_Tried <- separate_rows(Boxes_Tried, Treat, sep = ",", convert=T )
Boxes_Tried <- Boxes_Tried %>% select(-c(First_Oral_CGRP, First_Triptan, Elapsed))

Boxes_Tried <- Boxes_Tried %>% left_join(Drug_formulary %>% select(drug_id, drug_group), by=c("Treat"="drug_id")) %>%
  ungroup() %>% select(patid, weight, Month, drug_group) %>% distinct() %>%
    mutate(exp=1) %>% ungroup() %>% spread(key=drug_group, value=exp) 

Boxes_Tried[is.na(Boxes_Tried)] <- 0



CGRP_Inj <- Boxes_Tried %>% filter(`CGRP Injectable`==1) %>% select(patid, weight) %>% distinct()
sum(CGRP_Inj$weight) # 241701

Prev <- Boxes_Tried %>% filter(Preventive ==1) %>% select(patid, weight) %>% 
  anti_join(CGRP_Inj) %>% distinct()
sum(Prev$weight) # 303087.8

Triptans <- Boxes_Tried %>% filter(Triptans ==1) %>% select(patid, weight) %>% 
  anti_join(CGRP_Inj) %>% anti_join(Prev) %>%  distinct()
sum(Triptans$weight) # 88690.62





Boxes_Tried <- Boxes_Tried %>% mutate(Box= ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                         ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                ifelse(Preventive==1&Triptans==1, "Prev_Acute",
                                                              ifelse(Preventive==1&Symptomatic==1, "Prev_Sympt",
                                                                     ifelse(Preventive==1, "Prev",
                                                                            ifelse(Triptans==1, "Acute",
                                                                                          ifelse(Symptomatic==1, "Sympt", "Lapsed")))))))) %>%
  select(patid, weight, Month, Box) %>% distinct()


Boxes_Tried <- Boxes_Tried %>% select(-Month) %>% distinct() %>% group_by(patid, weight) %>%
  mutate(Box=paste0(Box, collapse = ", "))

Boxes_Tried <- Boxes_Tried %>% distinct() %>% ungroup() %>% group_by(Box) %>% summarise(tot=sum(weight))


Boxes_Tried <- Boxes_Tried %>% arrange(-tot)

fwrite(Boxes_Tried, "Boxes_Tried.csv")


# ------------------------
# Specialties Scripts Nasal Triptan Spray ---------------
Drug_formulary <- fread("Source/Drug_formulary_NS.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group_2=="Nasal Spray" &
                                                                      Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")


MIGUS24_Doses <- fread("Source/MIGUS24 Doses - version NS.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(grepl(string_Triptan_Nasal, drug_id_2))
MIGUS24_Doses <- MIGUS24_Doses %>% select(provcat)

PROVCAT <- read_excel("Source/CDM_Data_Reference_Lookup_Tables_V81.xlsx", sheet="PROVCAT", skip = 5)
PROVCAT <- PROVCAT %>% select(PROVCAT, DESCRIPTION)  %>% mutate(PROVCAT=as.numeric(PROVCAT))

MIGUS24_Doses <- MIGUS24_Doses %>%  inner_join(PROVCAT %>% select(PROVCAT, DESCRIPTION) %>% distinct(), by=c("provcat"="PROVCAT"))

data.frame(MIGUS24_Doses %>% select(DESCRIPTION) %>% distinct())

MIGUS24_Doses %>% mutate(DESCRIPTION=ifelse(DESCRIPTION %in% c("UNKNOWN PROVIDER TYPE", "SPECIAL RN SERVICES", "PHLEBOLOGY", "NEURODEVELOPMENTAL DISABILITIES",
                                                               "URGENT CARE CENTER", "UNKNOWN COSMOS PROVIDER TYPE",
                                                               "FAMILY PRACTICE/CAPITATED CLINIC", "PHYSICAL THERAPY CENTER CODE",
                                                               "UNKNOWN COSMOS FACILITY PROVIDER", "HOSPICE AND PALLIATIVE MEDICINE", "SLEEP STUDY",
                                                               "GENERAL HOSPITAL", "PEDIATRIC HOSPITAL"), "Remove",
                                            ifelse(DESCRIPTION %in% c("GASTROENTEROLOGIST", "REHABILITATION MEDICINE SPECIALIST", "EMERGENCY MEDICINE (PHYSICIANS)",
                                                                      "CARDIOLOGIST", "PAIN MANAGEMENT SPECIALIST", "ALLERGIST", "NEURO-SURGEON",
                                                                      "PHYSICAL MEDICINE SPECIALIST", "PULMONARY DISEASE SPECIALIST",
                                                                      "ANESTHESIOLOGIST", "GENERAL SURGEON", "PSYCHIATRIST",
                                                                      "PEDIATRIC GASTROENTEROLOGIST", "OBSTETRICIAN/GYNECOLOGIST", "DERMATOLOGIST",
                                                                      "GERIATRIC MEDICINE SPECIALIST", "VASCULAR SURGEON", "ENDOCRINOLOGIST",
                                                                      "CLINICAL NEUROPHYSIOLOGIST", "ADOLESCENT PEDIATRIC SPECIALIST",
                                                                      "OTOLARYNGOLOGIST", "HEMATOLOGIST/ONCOLOGIST", "OPHTHALMOLOGIST",
                                                                      "ORTHOPEDIC SURGEON", "VASCULAR INTERVENTION RADIOLOGIST", "RHEUMATOLOGIST",
                                                                      "PEDIATRIC CARDIOLOGIST", "ACCIDENTAL DENTAL/MEDICAL DENTAL/ORAL SURGERY",
                                                                      "HEMATOLOGIST", "PSYCHIATRIST/NEUROLOGIST", "ADDICTION MEDICINE SPECIALIST",
                                                                      "RADIOLOGIST", "NEPHROLOGIST", "PEDIATRIC SPECIALIST", "ALLERGIST & IMMUNOLOGIST",
                                                                      "UROLOGIST", "INFECTIOUS DISEASES SPECIALIST", "NEUROMUSCULAR DISEASE SPECIALIST",
                                                                      "SPORTS MEDICINE SPECIALIST", "MEDICAL ONCOLOGIST", "NEONATOLOGIST",
                                                                      "THORACIC SURGEON", "CARDIOVASCULAR DISEASE SPECIALIST",
                                                                      "PEDIATRIC EMERGENCY MEDICINE (PHYSICIANS)",
                                                                      "NUCLEAR MEDICINE SPECIALIST", "PLASTIC SURGEON", "UNKNOWN SPECIALTY PHYSICIAN",
                                                                      "ENDOCRINOLOGIST (DIABETES SPECIALIST)", "CRITICAL CARE MEDICINE SPECIALIST",
                                                                      "THERAPEUTIC RADIOLOGIST"), "Other Physician",
                                                   ifelse(DESCRIPTION %in% c("PHYSICIANS ASSISTANT", "NURSE PRACTITIONER", "FAMILY NURSE PRACTITIONER",
                                                                             "PHYSICAL THERAPIST", "REGISTERED NURSE", "OTHER MENTAL HEALTH PROFESSIONAL",
                                                                             "CLINICAL SOCIAL WORKER/MENTAL HEALTH COUNSELOR", "DENTIST - DMD",
                                                                             "NURSE PRACTITIONER, CLINICAL SPECIALIST IN MENTAL", "PEDIATRIC NURSE PRACTITIONER",
                                                                             "GENERAL DENTIST - DDS", "NURSE PRACTITIONER, CLINICAL SPECIALIST IN MENTAL HEALTH",
                                                                             "PODIATRIST - NON-MD", "AUDIOLOGIST", "PSYCHOLOGIST", "DOCTOR OF NATUROPATHY",
                                                                             "CERTIFIED NURSE ANESTHETIST", "SURGICAL ASSISTANT (NON-PHYS)"), "Other HCP",
                                                          ifelse( DESCRIPTION %in% c("FAMILY PRACTICE/GENERAL PRACTICE", "FAMILY PRACTITIONER", 
                                                                                     "OTHER NON-PHYSICIAN PROVIDER", "GENERAL PRACTITIONER", "FAMILY PRACTICE SPECIALIST"), "PCP",
                                                                  ifelse(DESCRIPTION %in% c("INTERNIST/GENERAL INTERNIST", "INTERNAL MEDICINE SPECIALIST",
                                                                                            "HOSPITALIST", "INTERNAL MED PEDIATRICS") , "IM",
                                                                         ifelse(DESCRIPTION %in% c("NEUROLOGIST" , "PEDIATRIC NEUROLOGIST"), "Neuro", NA ))))))) %>%
  filter(DESCRIPTION!="Remove") %>% group_by(DESCRIPTION) %>% count() %>% mutate(perc=n/(4776+12182+2860+2098+7610))

# ----------
# Oral CGRP usage among Triptan patients month over month --------

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
sum(MIGUS24_Drug_Histories_Extended_NoComorbs$weight)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, desc(Month))
# 21292150

Drug_formulary <- fread("Source/Drug_formulary.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Triptan <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class=="Triptan"], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class=="CGRP Oral"], collapse = "|"),")\\b")


Triptan_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, Treat) %>% distinct() %>%
  filter(grepl(string_Triptan, Treat)) %>% select(patid, weight)  %>% distinct() 

sum(Triptan_pats$weight) # 8788198 ever 

MIGUS24_Drug_Histories_Extended_NoComorbs <- Triptan_pats %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs)

Triptan_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%  filter(grepl(string_Triptan, Treat)) %>%
  group_by(patid, weight)  %>% filter(Month==min(Month)) %>%
  select(patid, weight, Month) %>% filter(Month<=36)

names(Triptan_pats)[3] <- "First_Triptan"
sum(Triptan_pats$weight) # 6773084 first 3 years

Oral_CGRP_pats <- Triptan_pats %>% 
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  filter(Month>=First_Triptan) %>%
  filter(grepl(string_CGRP_Oral, Treat)) %>% select(patid, weight)  %>% distinct() 

sum(Oral_CGRP_pats$weight) # 633479.2 ( 633479.2/6773084 = 0.09352892)


Oral_CGRP_pats <- Triptan_pats %>% 
  inner_join(Oral_CGRP_pats) %>%
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  filter(Month>=First_Triptan) %>%
  filter(grepl(string_CGRP_Oral, Treat)) %>% filter(Month==min(Month)) %>%
  select(patid, weight, Month) 


names(Oral_CGRP_pats)[3] <- "First_Oral_CGRP"
sum(Oral_CGRP_pats$weight) # 633479.2 

df <- Triptan_pats %>% left_join(Oral_CGRP_pats) 
range(df$First_Triptan)

breaks <- seq(0, 36, by = 6)

df$bucket <- cut(df$First_Triptan, breaks = breaks)

df %>% group_by(bucket) %>% summarise(tot=sum(weight))  %>%
  left_join(
    df %>% filter(!is.na(First_Oral_CGRP)) %>% group_by(bucket) %>% summarise(cgrp=sum(weight)) 
  ) %>%
  mutate(perc=cgrp/tot)

# ------------------

#Average Number of Different Molecules relative to initiation different molecules --------

# Zavegepant

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
  ylim(0,6.2) +
  xlab("\n Month Relative to Zavegepant Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  

















# Ubrogepant
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_UBRO <- flMIG %>% 
  filter(grepl("137", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_UBRO"="p2")

First_UBRO <- First_UBRO %>% filter(First_UBRO<=49)

flMIG <- First_UBRO %>% left_join(flMIG)

flMIG <- flMIG %>% filter(First_UBRO>1&First_UBRO<60)  %>% mutate(before=First_UBRO-1) %>% mutate(after=First_UBRO+1) 

before <- flMIG %>% filter(p2==before) %>% select(patient, d2)
current <- flMIG %>% filter(p2==First_UBRO) %>% select(patient, d2)
after <- flMIG %>% filter(p2==after) %>% select(patient, d2)

before <- separate_rows(before, d2, sep = ",", convert=T )
current <- separate_rows(current, d2, sep = ",", convert=T )
after <- separate_rows(after, d2, sep = ",", convert=T )


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


flMIG <- flMIG %>% select(patient, First_UBRO, p2, d2)
length(unique(flMIG$patient)) # 4069

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG %>% mutate(p2=p2-First_UBRO) %>%
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>%
  select(patient, p2, generic) %>% distinct() %>%
  group_by(patient, p2) %>% count() %>% ungroup() %>%
  group_by(p2) %>% summarise(mean=mean(n)) %>%
  ggplot(aes(p2, mean)) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_line(size=2, alpha=0.5, colour="deepskyblue4") +
  theme_bw() +
  ylim(0,6.2) +
  xlab("\n Month Relative to Ubrogepant Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  





# Rimegepant
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_RIME <- flMIG %>% 
  filter(grepl("136", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_RIME"="p2")

First_RIME <- First_RIME %>% filter(First_RIME<=49)

flMIG <- First_RIME %>% left_join(flMIG)

flMIG <- flMIG %>% filter(First_RIME>1&First_RIME<60)  %>% mutate(before=First_RIME-1) %>% mutate(after=First_RIME+1) 

before <- flMIG %>% filter(p2==before) %>% select(patient, d2)
current <- flMIG %>% filter(p2==First_RIME) %>% select(patient, d2)
after <- flMIG %>% filter(p2==after) %>% select(patient, d2)

before <- separate_rows(before, d2, sep = ",", convert=T )
current <- separate_rows(current, d2, sep = ",", convert=T )
after <- separate_rows(after, d2, sep = ",", convert=T )


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


flMIG <- flMIG %>% select(patient, First_RIME, p2, d2)
length(unique(flMIG$patient)) # 3090

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG %>% mutate(p2=p2-First_RIME) %>%
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>%
  select(patient, p2, generic) %>% distinct() %>%
  group_by(patient, p2) %>% count() %>% ungroup() %>%
  group_by(p2) %>% summarise(mean=mean(n)) %>%
  ggplot(aes(p2, mean)) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_line(size=2, alpha=0.5, colour="deepskyblue4") +
  theme_bw() +
  ylim(0,6.2) +
  xlab("\n Month Relative to Rimegepant Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  







# Topiramate
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_TOPI <- flMIG %>% 
  filter(grepl("91", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_TOPI"="p2")

First_TOPI <- First_TOPI %>% filter(First_TOPI<=49)

flMIG <- First_TOPI %>% left_join(flMIG)

flMIG <- flMIG %>% filter(First_TOPI>1&First_TOPI<60)  %>% mutate(before=First_TOPI-1) %>% mutate(after=First_TOPI+1) 

before <- flMIG %>% filter(p2==before) %>% select(patient, d2)
current <- flMIG %>% filter(p2==First_TOPI) %>% select(patient, d2)
after <- flMIG %>% filter(p2==after) %>% select(patient, d2)

before <- separate_rows(before, d2, sep = ",", convert=T )
current <- separate_rows(current, d2, sep = ",", convert=T )
after <- separate_rows(after, d2, sep = ",", convert=T )


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


flMIG <- flMIG %>% select(patient, First_TOPI, p2, d2)
length(unique(flMIG$patient)) # 3090

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG %>% mutate(p2=p2-First_TOPI) %>%
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>%
  select(patient, p2, generic) %>% distinct() %>%
  group_by(patient, p2) %>% count() %>% ungroup() %>%
  group_by(p2) %>% summarise(mean=mean(n)) %>%
  ggplot(aes(p2, mean)) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_line(size=2, alpha=0.5, colour="deepskyblue4") +
  theme_bw() +
  ylim(0,6.2) +
  xlab("\n Month Relative to Topiramate Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  







# Galcanezumab
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_GAL <- flMIG %>% 
  filter(grepl("141", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_GAL"="p2")

First_GAL <- First_GAL %>% filter(First_GAL<=49)

flMIG <- First_GAL %>% left_join(flMIG)

flMIG <- flMIG %>% filter(First_GAL>1&First_GAL<60)  %>% mutate(before=First_GAL-1) %>% mutate(after=First_GAL+1) 

before <- flMIG %>% filter(p2==before) %>% select(patient, d2)
current <- flMIG %>% filter(p2==First_GAL) %>% select(patient, d2)
after <- flMIG %>% filter(p2==after) %>% select(patient, d2)

before <- separate_rows(before, d2, sep = ",", convert=T )
current <- separate_rows(current, d2, sep = ",", convert=T )
after <- separate_rows(after, d2, sep = ",", convert=T )


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


flMIG <- flMIG %>% select(patient, First_GAL, p2, d2)
length(unique(flMIG$patient)) # 3090

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG %>% mutate(p2=p2-First_GAL) %>%
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>%
  select(patient, p2, generic) %>% distinct() %>%
  group_by(patient, p2) %>% count() %>% ungroup() %>%
  group_by(p2) %>% summarise(mean=mean(n)) %>%
  ggplot(aes(p2, mean)) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_line(size=2, alpha=0.5, colour="deepskyblue4") +
  theme_bw() +
  ylim(0,6.2) +
  xlab("\n Month Relative to Galcanezumab Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  


# Botulinum
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_BOT <- flMIG %>% 
  filter(grepl("131", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_BOT"="p2")

First_BOT <- First_BOT %>% filter(First_BOT<=49)

flMIG <- First_BOT %>% left_join(flMIG)

flMIG <- flMIG %>% filter(First_BOT>1&First_BOT<60)  %>% mutate(before=First_BOT-1) %>% mutate(after=First_BOT+1) 

before <- flMIG %>% filter(p2==before) %>% select(patient, d2)
current <- flMIG %>% filter(p2==First_BOT) %>% select(patient, d2)
after <- flMIG %>% filter(p2==after) %>% select(patient, d2)

before <- separate_rows(before, d2, sep = ",", convert=T )
current <- separate_rows(current, d2, sep = ",", convert=T )
after <- separate_rows(after, d2, sep = ",", convert=T )


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


flMIG <- flMIG %>% select(patient, First_BOT, p2, d2)
length(unique(flMIG$patient)) # 3090

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG %>% mutate(p2=p2-First_BOT) %>%
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>%
  select(patient, p2, generic) %>% distinct() %>%
  group_by(patient, p2) %>% count() %>% ungroup() %>%
  group_by(p2) %>% summarise(mean=mean(n)) %>%
  ggplot(aes(p2, mean)) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_line(size=2, alpha=0.5, colour="deepskyblue4") +
  theme_bw() +
  ylim(0,6.2) +
  xlab("\n Month Relative to Botulinum toxin Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  



# Meloxicam
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_MELO <- flMIG %>% 
  filter(grepl("13", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_MELO"="p2")

First_MELO <- First_MELO %>% filter(First_MELO<=49)

flMIG <- First_MELO %>% left_join(flMIG)

flMIG <- flMIG %>% filter(First_MELO>1&First_MELO<60)  %>% mutate(before=First_MELO-1) %>% mutate(after=First_MELO+1) 

before <- flMIG %>% filter(p2==before) %>% select(patient, d2)
current <- flMIG %>% filter(p2==First_MELO) %>% select(patient, d2)
after <- flMIG %>% filter(p2==after) %>% select(patient, d2)

before <- separate_rows(before, d2, sep = ",", convert=T )
current <- separate_rows(current, d2, sep = ",", convert=T )
after <- separate_rows(after, d2, sep = ",", convert=T )


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


flMIG <- flMIG %>% select(patient, First_MELO, p2, d2)
length(unique(flMIG$patient)) # 3090

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG %>% mutate(p2=p2-First_MELO) %>%
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>%
  select(patient, p2, generic) %>% distinct() %>%
  group_by(patient, p2) %>% count() %>% ungroup() %>%
  group_by(p2) %>% summarise(mean=mean(n)) %>%
  ggplot(aes(p2, mean)) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_line(size=2, alpha=0.5, colour="deepskyblue4") +
  theme_bw() +
  ylim(0,6.2) +
  xlab("\n Month Relative to Meloxicam Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  




# Sumatriptan
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_SUMA <- flMIG %>% 
  filter(grepl("74", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_SUMA"="p2")

First_SUMA <- First_SUMA %>% filter(First_SUMA<=49)

flMIG <- First_SUMA %>% left_join(flMIG)

flMIG <- flMIG %>% filter(First_SUMA>1&First_SUMA<60)  %>% mutate(before=First_SUMA-1) %>% mutate(after=First_SUMA+1) 

before <- flMIG %>% filter(p2==before) %>% select(patient, d2)
current <- flMIG %>% filter(p2==First_SUMA) %>% select(patient, d2)
after <- flMIG %>% filter(p2==after) %>% select(patient, d2)

before <- separate_rows(before, d2, sep = ",", convert=T )
current <- separate_rows(current, d2, sep = ",", convert=T )
after <- separate_rows(after, d2, sep = ",", convert=T )


MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
Drugs_lookup <- MIGUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


flMIG <- flMIG %>% select(patient, First_SUMA, p2, d2)
length(unique(flMIG$patient)) # 3090

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG %>% mutate(p2=p2-First_SUMA) %>%
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>%
  select(patient, p2, generic) %>% distinct() %>%
  group_by(patient, p2) %>% count() %>% ungroup() %>%
  group_by(p2) %>% summarise(mean=mean(n)) %>%
  ggplot(aes(p2, mean)) +
  geom_point(shape = 1, size = 2, stroke = 2) +
  geom_line(size=2, alpha=0.5, colour="deepskyblue4") +
  theme_bw() +
  ylim(0,6.2) +
  xlab("\n Month Relative to Sumatriptan Initiation") + 
  ylab("Average Number of Different Molecules ON \n") 
  



# --------------
# Cummulative Number of Molecules Relative to follow-up / RIME Initiation ---------

# Rimegepant
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_RIME <- flMIG %>% 
  filter(grepl("136", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_RIME"="p2")

df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- df %>% filter(version=="NEW_ZAV") %>% select(patid, weight, month1:month60)
df <- gather(df, Month, Treat, month1:month60, factor_key=TRUE)
df$Month <- parse_number(as.character(df$Month))
df <- First_RIME %>% inner_join(df, by=c("patient"="patid"))
df <- df %>% ungroup()
df <- separate_rows(df, Treat, sep = ",", convert=T )


#df %>% filter(patient=="802666960742820") %>%
#  select(Treat) %>% distinct()



df <- df %>% filter(Treat!="-")
df$patient <- as.character(df$patient)

df <- df %>% group_by(patient, First_RIME, weight, Treat) %>%
  filter(Month==min(Month)) %>% ungroup()

df <- df %>% arrange(patient, First_RIME, weight, Month, Treat)

df <- df %>% group_by(patient, First_RIME, weight, Month) %>% count() %>%
  ungroup() %>% group_by(patient, First_RIME, weight) %>%
  mutate(cum=cumsum(n)) %>% ungroup() %>% select(-n) 
  
length(unique(df$Month))

df <- df %>% select(patient, First_RIME, weight) %>% distinct() %>% mutate(link=1) %>%
  full_join(df %>% select(Month) %>% distinct() %>% mutate(link=1)) %>%
  left_join(df %>% select(patient, Month, cum) %>% distinct())

df <- df %>% arrange(patient, First_RIME, weight, link, Month, cum)


df <- df %>%
  group_by(patient) %>%
  fill(cum, .direction = "down") %>%
  ungroup()


df <- df %>% select(-link)

df[is.na(df)] <- 0


df <- df %>% left_join(df %>% filter(Month==1) %>% select(patient, cum) %>% rename("start"="cum") %>%
  left_join(df %>% filter(Month==60) %>% select(patient, cum) %>% rename("end"="cum")) %>%
  mutate(diff=end-start))

range(df$diff)


df$group <- as.numeric(cut(df$diff,10))

unique(df$group)

df %>% select(patient, weight, First_RIME, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))

unique(df$group)

df <- df %>% mutate(group=ifelse(group>=7,7,group))

df %>% mutate(group=as.factor(group*5)) %>%   
  rename("Delta_N_molecules"="group") %>%
  ggplot(aes(Month, cum, colour=Delta_N_molecules, fill=Delta_N_molecules)) +
  geom_smooth() +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Months Since Start of Follow-up") +
  ylab("Number of Different Molecules Tried \n") +
  scale_fill_viridis_d(option="C") +
  scale_colour_viridis_d(option="C") 
  


df %>% mutate(group=as.factor(group*5)) %>%   
  rename("Delta_N_molecules"="group") %>%
  mutate(Month=Month-First_RIME) %>%
  ggplot(aes(Month, cum, colour=Delta_N_molecules, fill=Delta_N_molecules)) +
  geom_smooth() +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Months Since Start of RIMEGEPANT") +
  ylab("Number of Different Molecules Tried \n") +
  scale_fill_viridis_d(option="C") +
  scale_colour_viridis_d(option="C") 
  



df %>% mutate(group=as.factor(group*5)) %>%   
  rename("Delta_N_molecules"="group") %>%
  mutate(First_RIME=cut(First_RIME,10)) %>%
  ggplot(aes(Month, cum, colour=First_RIME, fill=First_RIME)) +
  geom_smooth() +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Months Since Start of Follow-up") +
  ylab("Number of Different Molecules Tried \n") +
  scale_fill_viridis_d(option="C") +
  scale_colour_viridis_d(option="C") 
  

# ---------

# Cummulative Number of Molecules Relative to follow-up / Sumatriptan Initiation ---------

# Sumatriptan
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_SUMA <- flMIG %>% 
  filter(grepl("74", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_SUMA"="p2")

df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- df %>% filter(version=="NEW_ZAV") %>% select(patid, weight, month1:month60)
df <- gather(df, Month, Treat, month1:month60, factor_key=TRUE)
df$Month <- parse_number(as.character(df$Month))
df <- First_SUMA %>% inner_join(df, by=c("patient"="patid"))
df <- df %>% ungroup()
df <- separate_rows(df, Treat, sep = ",", convert=T )


df <- df %>% filter(Treat!="-")
df$patient <- as.character(df$patient)

df <- df %>% group_by(patient, First_SUMA, weight, Treat) %>%
  filter(Month==min(Month)) %>% ungroup()

df <- df %>% arrange(patient, First_SUMA, weight, Month, Treat)

df <- df %>% group_by(patient, First_SUMA, weight, Month) %>% count() %>%
  ungroup() %>% group_by(patient, First_SUMA, weight) %>%
  mutate(cum=cumsum(n)) %>% ungroup() %>% select(-n) 
  
length(unique(df$Month))

df <- df %>% select(patient, First_SUMA, weight) %>% distinct() %>% mutate(link=1) %>%
  full_join(df %>% select(Month) %>% distinct() %>% mutate(link=1)) %>%
  left_join(df %>% select(patient, Month, cum) %>% distinct())

df <- df %>% arrange(patient, First_SUMA, weight, link, Month, cum)


df <- df %>%
  group_by(patient) %>%
  fill(cum, .direction = "down") %>%
  ungroup()


df <- df %>% select(-link)

df[is.na(df)] <- 0


df <- df %>% left_join(df %>% filter(Month==1) %>% select(patient, cum) %>% rename("start"="cum") %>%
  left_join(df %>% filter(Month==60) %>% select(patient, cum) %>% rename("end"="cum")) %>%
  mutate(diff=end-start))

range(df$diff)


df$group <- as.numeric(cut(df$diff,10))

unique(df$group)

df %>% select(patient, weight, First_SUMA, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))

# 1     1 2087974.
# 2     2 2342256.
# 3     3 1224009.
# 4     4  365483.
# 5     5  129890.
# 6     6   22096.
# 7     7    3498.
# 8     8     517.
# 9    10     185.



unique(df$group)

df <- df %>% mutate(group=ifelse(group>=7,7,group))

df %>% mutate(group=as.factor(group*5)) %>%   
  rename("Delta_N_molecules"="group") %>%
  ggplot(aes(Month, cum, colour=Delta_N_molecules, fill=Delta_N_molecules)) +
  geom_smooth() +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Months Since Start of Follow-up") +
  ylab("Number of Different Molecules Tried \n") +
  scale_fill_viridis_d(option="C") +
  scale_colour_viridis_d(option="C") 
  


df %>% mutate(group=as.factor(group*5)) %>%   
  rename("Delta_N_molecules"="group") %>%
  mutate(Month=Month-First_SUMA) %>%
  ggplot(aes(Month, cum, colour=Delta_N_molecules, fill=Delta_N_molecules)) +
  geom_smooth() +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Months Since Start of SUMATRIPTAN") +
  ylab("Number of Different Molecules Tried \n") +
  scale_fill_viridis_d(option="C") +
  scale_colour_viridis_d(option="C") 
  



df %>% mutate(group=as.factor(group*5)) %>%   
  rename("Delta_N_molecules"="group") %>%
  mutate(First_SUMA=cut(First_SUMA,10)) %>%
  ggplot(aes(Month, cum, colour=First_SUMA, fill=First_SUMA)) +
  geom_smooth() +
  theme_minimal() +
   theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
    coord_cartesian(ylim = c(0,18)) +
  xlab("\n Number of Months Since Start of Follow-up") +
  ylab("Number of Different Molecules Tried \n") +
  scale_fill_viridis_d(option="C") +
  scale_colour_viridis_d(option="C") 
  


# ---------
# Cummulative Number of Molecules Relative to follow-up /  the entire MIG population -----------

flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_Rx <- flMIG %>% 
  filter(!grepl("-", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_Rx"="p2")

df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- df %>% filter(version=="NEW_ZAV") %>% select(patid, weight, month1:month60)
df <- gather(df, Month, Treat, month1:month60, factor_key=TRUE)
df$Month <- parse_number(as.character(df$Month))
df <- First_Rx %>% inner_join(df, by=c("patient"="patid"))
df <- df %>% ungroup()
df <- separate_rows(df, Treat, sep = ",", convert=T )


df <- df %>% filter(Treat!="-")
df$patient <- as.character(df$patient)

df <- df %>% group_by(patient, First_Rx, weight, Treat) %>%
  filter(Month==min(Month)) %>% ungroup()

df <- df %>% arrange(patient, First_Rx, weight, Month, Treat)

df <- df %>% group_by(patient, First_Rx, weight, Month) %>% count() %>%
  ungroup() %>% group_by(patient, First_Rx, weight) %>%
  mutate(cum=cumsum(n)) %>% ungroup() %>% select(-n) 
  
length(unique(df$Month))

df <- df %>% select(patient, First_Rx, weight) %>% distinct() %>% mutate(link=1) %>%
  full_join(df %>% select(Month) %>% distinct() %>% mutate(link=1)) %>%
  left_join(df %>% select(patient, Month, cum) %>% distinct())

df <- df %>% arrange(patient, First_Rx, weight, link, Month, cum)


df <- df %>%
  group_by(patient) %>%
  fill(cum, .direction = "down") %>%
  ungroup()


df <- df %>% select(-link)

df[is.na(df)] <- 0


df <- df %>% left_join(df %>% filter(Month==1) %>% select(patient, cum) %>% rename("start"="cum") %>%
  left_join(df %>% filter(Month==60) %>% select(patient, cum) %>% rename("end"="cum")) %>%
  mutate(diff=end-start))

range(df$diff)


df$group <- as.numeric(cut(df$diff,10))

unique(df$group)

df %>% select(patient, weight, First_Rx, group) %>% distinct() %>%
  group_by(group) %>% summarise(n=sum(weight))

Mod_Sev <- fread("Source/Mod_Sev.txt")


df %>% select(patient, weight, First_Rx, group) %>% mutate(patient=as.character(patient)) %>% distinct() %>%
  inner_join(Mod_Sev %>% rename("patient"="patid") %>% mutate(patient=as.character(patient)) %>% select(-group) ) %>%
  group_by(group) %>% summarise(n=sum(weight))

# -----
# Number of Molecules the month before vs month after ----------

# Rimegepant
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_RIME <- flMIG %>% 
  filter(grepl("136", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_RIME"="p2")

df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- df %>% filter(version=="NEW_ZAV") %>% select(patid, weight, month1:month60)
df <- gather(df, Month, Treat, month1:month60, factor_key=TRUE)
df$Month <- parse_number(as.character(df$Month))
df <- First_RIME %>% inner_join(df, by=c("patient"="patid"))
df <- df %>% ungroup()
df <- separate_rows(df, Treat, sep = ",", convert=T )

df$patient <- as.character(df$patient)


df %>% filter(Month==First_RIME-1 | Month==First_RIME+1) %>% 
  filter(First_RIME>=2&First_RIME<=59) %>%
  mutate(Month=Month-First_RIME) %>%
  filter(Treat!="-") %>% select(-First_RIME) %>%
  group_by(patient, weight, Month) %>% count() %>% spread(key=Month, value=n) %>%
  mutate(`-1`=ifelse(is.na(`-1`),0,`-1`)) %>%
  mutate(`1`=ifelse(is.na(`1`),0,`1`)) %>%
  mutate(`1`=ifelse(`1`>=8,8,`1`)) %>%
  mutate(`-1`=ifelse(`-1`>=8,8,`-1`)) %>%
  ungroup() %>% group_by(`-1`,`1`) %>% summarise(tot=sum(weight)) %>%
  spread(key=`1`, value=tot)

# ---------------
# How many of the Ever Rimegepant are on symptomatic JAN 24 ------
# Rimegepant
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_RIME <- flMIG %>% 
  filter(grepl("136", d1)|grepl("136", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, weight, p2) %>% rename("First_RIME"="p2")

sum(First_RIME$weight)




# Rimegepant
flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2)) %>% filter(p2==60) %>%
  select(patient, weight, d2) %>% inner_join(First_RIME)
 


Drug_formulary <- fread("Source/Drug_formulary.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Sympt <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Symptomatic"], collapse = "|"),")\\b")

flMIG %>% filter(grepl(string_Sympt, d2)) %>% summarise(n=sum(weight))

# -------------

# How many of the MOD SEV  are on symptomatic JAN 24 ------

Mod_Sev <- fread("Source/Mod_Sev.txt")


Drug_formulary <- fread("Source/Drug_formulary.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Sympt <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Symptomatic"], collapse = "|"),")\\b")



flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2)) %>% filter(p2==60) %>%
  select(patient, weight, d2) %>% inner_join(Mod_Sev, by=c("patient"="patid"))

sum(flMIG$weight)

flMIG %>% filter(grepl(string_Sympt, d2)) %>% summarise(n=sum(weight))

# --------

 

# 12 month penetrance vs 12 month duration ------------
Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(-group)


Drug_formulary <- fread("Source/Drug_formulary.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())


df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- gather(df, Month, Treat, month1:month60, factor_key=TRUE)
df$Month <- parse_number(as.character(df$Month))
df <- df %>% filter(Treat != "-")
df <- df %>% filter(Month>=49)
df <- df %>% select(-Month) %>% distinct()
df <- separate_rows(df, Treat, sep = ",", convert=T )
df <- df %>% distinct()

df <- df %>% left_join(Drug_formulary %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
df <- df %>% select(patid, weight, drug_class) %>% distinct()

pen <- data.frame(df %>% inner_join(Mod_Sev) %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 12768773)*100)) %>% arrange(-sum_weights_percent)



df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- gather(df, Month, Treat, month1:month60, factor_key=TRUE)
df$Month <- parse_number(as.character(df$Month))
df <- df %>% filter(Treat != "-")
df <- df %>% filter(Month>=49)
df <- separate_rows(df, Treat, sep = ",", convert=T )

df <- df %>% left_join(Drug_formulary %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
df <- df %>% select(patid, weight, Month, drug_class) %>% distinct()

Mod_Sev %>% mutate(link=1) %>%
  full_join(Drug_formulary %>% select(drug_class) %>% distinct() %>% mutate(link=1)) %>%
  select(-link) %>%
  left_join(df %>% inner_join(Mod_Sev) %>% group_by(patid, drug_class) %>% count()) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n))


time <- data.frame(df %>% inner_join(Mod_Sev) %>% group_by(patid, drug_class) %>% count() %>%
  ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n)))


df <- pen %>% inner_join(time)


names(df) <- c("drug_class", "pop", "penetrance", "duration")


library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(df, aes(x=penetrance, y=duration, size = penetrance, fill=penetrance, colour=penetrance)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = drug_class), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(.1, 24))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 10))+
  scale_colour_viridis_c(option = "A")+
  xlab("\n12-month Penetrance (% Population)")+
  ylab("Weighted Average Duration (months)\n")+
  ylim(0,11)+ xlim(0,50)

# ----------
# 12-month rolling sum of different number of molecules used past 12 months  -----------

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

df <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
df <- Mod_Sev %>% inner_join(df)
df <- df %>% filter(version=="NEW_ZAV") %>% select(patid, weight, month1:month60)
df <- gather(df, Month, Treat, month1:month60, factor_key=TRUE)
df$Month <- parse_number(as.character(df$Month))
df <- df %>% ungroup()
df <- df %>% filter(Treat!="-")
df <- separate_rows(df, Treat, sep = ",", convert=T )
df$patid <- as.character(df$patid)

df <- df %>% arrange(patid, Month, Treat)



result_list <- list()



for (i in 1:49) {
  
  print(i)
  
  start_month <- i
  
  end_month <- i + 11
  
  filtered_data <- df %>%
    filter(Month >= start_month & Month <= end_month) %>%
    select(-Month) %>% distinct() %>%
    group_by(patid, weight) %>%
    count() %>% rename("loop"="n")

  result_list[[i]] <- filtered_data
  
}

final_result <- bind_rows(result_list, .id = "Iteration")

unique(final_result$Iteration)

unique(final_result$loop)


# 48 vs 60 -> 37 vs  49

final_result %>% ungroup() %>% group_by(Iteration, loop) %>%
  summarise(pop=sum(weight)) 

plot <- final_result %>% ungroup() %>% filter(Iteration==37 | Iteration==49) %>%
  spread(key=Iteration, value=loop) %>%
  mutate(`37`=ifelse(is.na(`37`),0,`37` )) %>%
  mutate(`49`=ifelse(is.na(`49`),0,`49` )) 

unique(plot$`37`)
unique(plot$`49`)

data.frame(plot %>% drop_na() %>% 
  rename("before"=`37`) %>%
  rename("after"=`49`) %>%
    mutate(before=ifelse(before>=12,12,before)) %>%
    mutate(after=ifelse(after>=12,12,after)) %>%
  group_by(before, after) %>% summarise(pop=sum(weight)) %>%
  spread(key=after, value=pop))



flMIG <- fread("Source/MIGUS24 Flows_Long.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

First_RIME <- flMIG %>% 
  filter(grepl("136", d2)) %>% group_by(patient) %>% filter(p2==min(p2)) %>% select(patient, p2) %>% rename("First_RIME"="p2")


final_result %>% ungroup() %>%  
  left_join(First_RIME %>% rename("patid"="patient") %>% mutate(patid=as.character(patid))) %>%
  drop_na() %>%
  mutate(Iteration=as.numeric(Iteration)+11) %>%
  filter(Iteration==First_RIME | Iteration==First_RIME+12) %>%
  mutate(Iteration=Iteration-First_RIME) %>% arrange(patid, Iteration) %>%
      mutate(loop =ifelse(loop >=12,12,loop )) %>%
  spread(key=Iteration, value=loop) %>% 
  filter(First_RIME<=48) %>%
  drop_na() %>%
   group_by(`0`, `12`) %>% summarise(pop=sum(weight)) %>%
  spread(key=`12`, value=pop)



# --------
# % Inflows to Zavegepant ON Symptomatic, triptan or preventive ------

Drug_formulary <- fread("Source/Drug_formulary.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Sympt <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Preventive"], collapse = "|"),")\\b")



flMIG <- fread("Source/MIG_Flows_Aux_Long.txt")
flMIG <- flMIG %>% filter(p2>=49)

# 192

flMIG %>% filter(!grepl("Z",s1) & grepl("Z",s2)) %>%
  filter(grepl(string_Sympt, d1)) %>% count() %>% mutate(n=n/192)

flMIG %>% filter(!grepl("Z",s1) & grepl("Z",s2)) %>%
  filter(grepl(string_Triptan, d1)) %>% count() %>% mutate(n=n/192)

flMIG %>% filter(!grepl("Z",s1) & grepl("Z",s2)) %>%
  filter(grepl(string_Prev, d1)) %>% count() %>% mutate(n=n/192)



flMIG <- flMIG %>% filter(!grepl("Z",s1) & grepl("Z",s2)) %>%
  select(patient, d1,p2)
  
flMIG <- separate_rows(flMIG, d1, sep = ",", convert=T )

flMIG <- flMIG %>% 
  left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_class, drug_group), 
            by=c("d1"="drug_id"))


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

flMIG <- flMIG %>% left_join(MIGUS24_Demographics)

flMIG <- flMIG %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

flMIG <- flMIG %>% filter(flag==0) %>% select(-flag)

flMIG  %>% filter(grepl(string_Prev, d1)) %>% 
  select(patient, p2) %>% distinct() %>% count() %>% mutate(n=n/192)


flMIG  %>% filter(grepl(string_Triptan, d1)) %>% 
  select(patient, p2) %>% distinct() %>% count() %>% mutate(n=n/192)

flMIG  %>% filter(grepl(string_Sympt, d1)) %>% 
  select(patient, p2) %>% distinct() %>% count() %>% mutate(n=n/192)



# --------------
# Redo inflows outflows without comorbidity drugs -------



Drug_formulary <- fread("Source/Drug_formulary.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Sympt <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Preventive"], collapse = "|"),")\\b")



# INFLOWS 

flMIG <- fread("Source/MIG_Flows_Aux_Long.txt")
flMIG <- flMIG %>% filter(p2>=49) %>% select(patient, d1, p1, s1, s2)

flMIG <- separate_rows(flMIG, d1, sep = ",", convert=T )

flMIG <- flMIG %>% 
  left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_class, drug_group), 
            by=c("d1"="drug_id"))


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

flMIG <- flMIG %>% left_join(MIGUS24_Demographics)

flMIG <- flMIG %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

flMIG <- flMIG %>% filter(flag==0) %>% select(-flag)



temp <- flMIG %>% select(patient, d1, p1, s1, s2, drug_group) %>%  select(-d1) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=drug_group, value=exp) 

temp[is.na(temp)] <- 0


temp <- temp %>% mutate(Box=ifelse(`CGRP Nasal`==1, "Nasal",
                                                                       ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                                              ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                                                     ifelse(Preventive==1&Triptans==1, "Prev_Acute",
                                                                                                   ifelse(Preventive==1&Symptomatic==1, "Prev_Sympt",
                                                                                                          ifelse(Preventive==1, "Prev",
                                                                                                                 ifelse(Triptans==1, "Acute",
                                                                                                                               ifelse(Symptomatic==1, "Sympt", "Lapsed")))))))))



temp %>%
  filter(Preventive==1) %>%
  select(patient, p1, s1, s2, Box) %>%
  filter(!grepl("Z",s1) & grepl("Z",s2)) %>% 
  distinct() %>% group_by(Box) %>% count() %>% mutate(n=n/192) # 10% missing

# 1 Acute      0.0625
# 2 CGRP_Inj   0.219 
# 3 CGRP_Oral  0.417 
# 4 Prev       0.0521
# 5 Prev_Acute 0.0469
# 6 Prev_Sympt 0.0417
# 7 Sympt      0.0625
# 8 Lapsed missing






#OUTFLOWS

flMIG <- fread("Source/MIG_Flows_Aux_Long.txt")
flMIG <- flMIG %>% filter(p2>=49) %>% select(patient, d2, p2, s1, s2)

flMIG <- separate_rows(flMIG, d2, sep = ",", convert=T )

flMIG <- flMIG %>% 
  left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_class, drug_group), 
            by=c("d2"="drug_id"))


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

flMIG <- flMIG %>% left_join(MIGUS24_Demographics)

flMIG <- flMIG %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

flMIG <- flMIG %>% filter(flag==0) %>% select(-flag)



temp <- flMIG %>% select(patient, d2, p2, s1, s2, drug_group) %>%  select(-d2) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=drug_group, value=exp) 

temp[is.na(temp)] <- 0


temp <- temp %>% mutate(Box=ifelse(`CGRP Nasal`==1, "Nasal",
                                                                       ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                                              ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                                                     ifelse(Preventive==1&Triptans==1, "Prev_Acute",
                                                                                                   ifelse(Preventive==1&Symptomatic==1, "Prev_Sympt",
                                                                                                          ifelse(Preventive==1, "Prev",
                                                                                                                 ifelse(Triptans==1, "Acute",
                                                                                                                               ifelse(Symptomatic==1, "Sympt", "Lapsed")))))))))



temp %>% select(patient, p2, s1, s2, Box) %>%
  filter(grepl("Z",s1) & !grepl("Z",s2)) %>% 
  distinct() %>% group_by(Box) %>% count() %>% mutate(n=n/54) # 11% missing


# 1 Acute      0.0370
# 2 CGRP_Inj   0.204 
# 3 CGRP_Oral  0.426 
# 4 Prev       0.0185
# 5 Prev_Acute 0.0556
# 6 Prev_Sympt 0.111 
# 7 Sympt      0.0370
# -------------
# Zavegepant  patients waterfall ON January 2024 W/O Comorbidity --------------



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

ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, month60, sep = ",", convert=T )

ZAVUS24_Demographics <- fread("Source/ZAVUS24 Demographics.txt")

ZAVUS24_Demographics <- ZAVUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% 
  left_join(ZAVUS24_Demographics %>% rename("patient"="patid") %>% mutate(patient=as.character(patient)))

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% 
  left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("month60"="drug_id"))



ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

unique(ZAVUS24_Drug_Histories$flag)

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(flag==0)

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(-c(CV, psychiatric, epileptic, drug_class, flag))

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% arrange(patient, Zavegepant, month60) %>% 
  group_by(patient, Zavegepant) %>% mutate(month60=paste0(month60, collapse = ","))  %>%
  distinct() %>% ungroup()



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


# ---------
 # Redo inflows outflows without comorbidity drugs month over month -------



Drug_formulary <- fread("Source/Drug_formulary.txt")
data.frame(Drug_formulary)
data.frame(Drug_formulary %>% select(drug_class, drug_group) %>% distinct())

string_Sympt <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Preventive"], collapse = "|"),")\\b")



# INFLOWS 

flMIG <- fread("Source/MIG_Flows_Aux_Long.txt")
flMIG <- flMIG %>% filter(p2>=49) %>% select(patient, d1, p1, s1, s2)

flMIG <- separate_rows(flMIG, d1, sep = ",", convert=T )

flMIG <- flMIG %>% 
  left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_class, drug_group), 
            by=c("d1"="drug_id"))


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

flMIG <- flMIG %>% left_join(MIGUS24_Demographics)

flMIG <- flMIG %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

flMIG <- flMIG %>% filter(flag==0) %>% select(-flag)



temp <- flMIG %>% select(patient, d1, p1, s1, s2, drug_group) %>%  select(-d1) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=drug_group, value=exp) 

temp[is.na(temp)] <- 0


temp <- temp %>% mutate(Box=ifelse(`CGRP Nasal`==1, "Nasal",
                                                                       ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                                              ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                                                     ifelse(Preventive==1&Triptans==1, "Prev_Acute",
                                                                                                   ifelse(Preventive==1&Symptomatic==1, "Prev_Sympt",
                                                                                                          ifelse(Preventive==1, "Prev",
                                                                                                                 ifelse(Triptans==1, "Acute",
                                                                                                                               ifelse(Symptomatic==1, "Sympt", "Lapsed")))))))))


temp <- temp %>% filter(!grepl("Z",s1) & grepl("Z",s2)) 

data.frame(temp %>% group_by(p1, Box) %>% count() %>% spread(key=Box, value=n))


# ----------


# Any Acute Before vs After First Zavegepant ------------------

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


unique(Drugs_lookup$drug_class)
unique(Drugs_lookup$drug_group)

Drugs_lookup <- Drugs_lookup %>% mutate(Acutes=ifelse(drug_class %in% c("NSAID", "Analgesic", "Weak Opioid", "Strong Opioid",
                                                        "Steroid", "Ergot", "Triptan", "Ditan", "CGRP Oral"), 1,0))


data.frame(before %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, Acutes) %>% distinct() %>% group_by(Acutes) %>% count() %>% mutate(n=n/length(unique(before$patient))) %>%
  rename("before"="n") %>%
  full_join(
    current %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, Acutes) %>% distinct() %>% group_by(Acutes) %>% count() %>% mutate(n=n/length(unique(current$patient))) %>%
  rename("current"="n")) %>%
  full_join(
    after %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, Acutes) %>% distinct() %>% group_by(Acutes) %>% count() %>% mutate(n=n/length(unique(after$patient))) %>%
  rename("after"="n")
  ))


# -------


# Number on 0 Acutes, 1 Acute, 2 Acutes month 60 -------


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


unique(Drugs_lookup$drug_class)
unique(Drugs_lookup$drug_group)

Drugs_lookup <- Drugs_lookup %>% mutate(Acutes=ifelse(drug_class %in% c("NSAID", "Analgesic", "Weak Opioid", "Strong Opioid",
                                                        "Steroid", "Ergot", "Triptan", "Ditan", "CGRP Oral"), 1,0))


string_Acutes <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$Acutes == 1], collapse = "|"),")\\b")

ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient, month60) #254

ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, month60, sep = ",", convert=T )
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% filter(grepl(string_Acutes, month60))

ZAVUS24_Drug_Histories %>% group_by(patient) %>% count() %>% ungroup() %>% rename("n_mols"="n") %>%
  group_by(n_mols) %>% count()   %>% mutate(perc=round(n/254,3))




ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)


unique(Drugs_lookup$drug_class)
unique(Drugs_lookup$drug_group)

Drugs_lookup <- Drugs_lookup %>% mutate(Acutes=ifelse(drug_class %in% c("NSAID", "Analgesic", "Weak Opioid", "Strong Opioid",
                                                        "Steroid", "Ergot", "Triptan", "Ditan", "CGRP Oral"), 1,0))


string_Acutes <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$Acutes == 1], collapse = "|"),")\\b")

MIGUS24_Drug_Histories <- read.table("Source/MIGUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
length(unique(MIGUS24_Drug_Histories$patient)) # 240747
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(patient, month60) 

MIGUS24_Drug_Histories <- separate_rows(MIGUS24_Drug_Histories, month60, sep = ",", convert=T )
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(grepl(string_Acutes, month60))

MIGUS24_Drug_Histories %>% group_by(patient) %>% count() %>% ungroup() %>% rename("n_mols"="n") %>%
  group_by(n_mols) %>% count()   %>% mutate(perc=round(n/240747,3))






# -------

# Generate table for langchain --------

ZAVUS24_Drug_Histories <- read.table("Source/ZAVUS24 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Box_Histories <- read.table("Source/ZAVUS24 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
ZAVUS24_Box_Specifications <- read.table("Source/ZAVUS24 Box Specifications.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)


ZAVUS24_Drug_Histories <- gather(ZAVUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
names(ZAVUS24_Drug_Histories)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(-c(disease))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(Treat=ifelse(Treat=="*", "-", Treat))

ZAVUS24_Box_Histories <- gather(ZAVUS24_Box_Histories, Month, Box, month1:month60, factor_key=TRUE)
ZAVUS24_Box_Histories <- ZAVUS24_Box_Histories %>% select(-c(disease))

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% left_join(ZAVUS24_Box_Histories)

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% left_join(ZAVUS24_Box_Specifications %>% select(box_code, drug_group, treatment_segment), by=c("Box"="box_code"))
ZAVUS24_Drug_Histories$Month <- parse_number(as.character(ZAVUS24_Drug_Histories$Month))
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(-weight)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(-treatment_segment)
names(ZAVUS24_Drug_Histories) <- c("patient_id", "month", "molecules", "stock_abbreviation", "stock_full_name")
ZAVUS24_Drug_Histories <- separate_rows(ZAVUS24_Drug_Histories, molecules, sep = ",", convert=T )


ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
Drugs_lookup <- ZAVUS24_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% arrange(drug_id)
unique(Drugs_lookup$drug_group)
Drugs_lookup <- Drugs_lookup %>% select(drug_id, generic, drug_class)


ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("molecules"="drug_id"))

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% 
  mutate(generic=ifelse(is.na(generic), "lapsed", generic)) %>%
  mutate(drug_class=ifelse(is.na(drug_class), "lapsed", drug_class))

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(-stock_abbreviation) 
unique(ZAVUS24_Drug_Histories$stock_full_name)
ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% mutate(stock_full_name=ifelse(stock_full_name=="Not visible", "Lapsed", stock_full_name))

ZAVUS24_Drug_Histories <- ZAVUS24_Drug_Histories %>% select(patient_id, month, generic, drug_class, stock_full_name)
names(ZAVUS24_Drug_Histories)[3] <- "molecule_name"
names(ZAVUS24_Drug_Histories)[4] <- "class_name"
names(ZAVUS24_Drug_Histories)[5] <- "patient_stock_name"


fwrite(ZAVUS24_Drug_Histories, "ZAVUS24_Drug_Histories.csv")

# ------
# Number on 0 Acutes, 1 Acute, 2 Acutes Month Before Current After 1st ZAV ------------------

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


unique(Drugs_lookup$drug_class)
unique(Drugs_lookup$drug_group)

Drugs_lookup <- Drugs_lookup %>% mutate(Acutes=ifelse(drug_class %in% c("NSAID", "Analgesic", "Weak Opioid", "Strong Opioid",
                                                        "Steroid", "Ergot", "Triptan", "Ditan", "CGRP Oral"), 1,0))


before %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, d2, Acutes) %>% distinct()  %>% filter(Acutes==1) %>% group_by(patient) %>% count() %>% ungroup() %>%
  rename("mols"="n") %>% group_by(mols) %>% count() %>%
  mutate(n=n/length(unique(before$patient)))

current %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, d2, Acutes) %>% distinct()  %>% filter(Acutes==1) %>% group_by(patient) %>% count() %>% ungroup() %>%
  rename("mols"="n") %>% group_by(mols) %>% count() %>%
  mutate(n=n/length(unique(current$patient)))

after %>% mutate(d2=as.character(d2)) %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("d2"="drug_id")) %>% 
  select(patient, d2, Acutes) %>% distinct()  %>% filter(Acutes==1) %>% group_by(patient) %>% count() %>% ungroup() %>%
  rename("mols"="n") %>% group_by(mols) %>% count() %>%
  mutate(n=n/length(unique(after$patient)))


# -------
# Pathways to preventive - stock before current after ---------------------------------

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|Drugs_lookup$drug_group=="CGRP Injectable")&!(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")



string_Sympt <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive")&(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Nasal <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Nasal"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Demographics %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(ON_Prev=ifelse(grepl(string_Preventive, Treat), 1,0)) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  anti_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month<=12 & ON_Prev==1) %>% select(patid) %>% distinct())

length(unique(MIGUS24_Drug_Histories_Extended_NoComorbs$patid))


Month_before <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  filter(lead(ON_Prev)==1) %>% group_by(patid) %>% filter(Month==min(Month))

sum(Month_before$weight)

Month_before <- Month_before %>% mutate(Box=ifelse(grepl(string_CGRP_Nasal, Treat), "Nasal",
                                   ifelse(grepl(string_CGRP_Oral, Treat), "Oral",
                                          ifelse(grepl(string_CGRP_Inj, Treat), "Inj",
                                                 ifelse(grepl(string_Preventive, Treat), "Prev",
                                                        ifelse(grepl(string_Triptan, Treat), "Triptan",
                                                               ifelse(grepl(string_Sympt, Treat), "Sympt", "Lapsed")))))))


Month_before %>% ungroup() %>% group_by(Box) %>% summarise(n=sum(weight)/(2506132))

Month_before <- separate_rows(Month_before, Treat, sep = ",", convert=T )

Month_before <- Month_before %>% left_join(Drugs_lookup %>% select(drug_id, drug_class) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(before=sum(weight))




Month_current <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  filter(ON_Prev==1) %>% group_by(patid) %>% filter(Month==min(Month))

Month_current <- Month_current %>% mutate(Box=ifelse(grepl(string_CGRP_Nasal, Treat), "Nasal",
                                   ifelse(grepl(string_CGRP_Oral, Treat), "Oral",
                                          ifelse(grepl(string_CGRP_Inj, Treat), "Inj",
                                                 ifelse(grepl(string_Preventive, Treat), "Prev",
                                                        ifelse(grepl(string_Triptan, Treat), "Triptan",
                                                               ifelse(grepl(string_Sympt, Treat), "Sympt", "Lapsed")))))))


Month_current %>% ungroup() %>% group_by(Box) %>% summarise(n=sum(weight)/3998293)

Month_current <- separate_rows(Month_current, Treat, sep = ",", convert=T )

Month_current <- Month_current %>% mutate(Treat=as.character(Treat)) %>%
  left_join(Drugs_lookup %>% select(drug_id, drug_class) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(current=sum(weight))





Month_after <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  filter(lag(ON_Prev)==1) %>% group_by(patid) %>% filter(Month==min(Month))

Month_after <- Month_after %>% mutate(Box=ifelse(grepl(string_CGRP_Nasal, Treat), "Nasal",
                                   ifelse(grepl(string_CGRP_Oral, Treat), "Oral",
                                          ifelse(grepl(string_CGRP_Inj, Treat), "Inj",
                                                 ifelse(grepl(string_Preventive, Treat), "Prev",
                                                        ifelse(grepl(string_Triptan, Treat), "Triptan",
                                                               ifelse(grepl(string_Sympt, Treat), "Sympt", "Lapsed")))))))


Month_after %>% ungroup() %>% group_by(Box) %>% summarise(n=sum(weight)/3201456)

Month_after <- separate_rows(Month_after, Treat, sep = ",", convert=T )

Month_after <- Month_after %>% mutate(Treat=as.character(Treat)) %>%
  left_join(Drugs_lookup %>% select(drug_id, drug_class) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(after=sum(weight))

data.frame(Month_before %>% full_join(Month_current) %>% full_join(Month_after))

# --------------------

# Pathways to preventive - number acute molecules before 1st preventive ---------------------------------

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|Drugs_lookup$drug_group=="CGRP Injectable")&!(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")



string_Sympt <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive")&(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Nasal <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Nasal"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Demographics %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(ON_Prev=ifelse(grepl(string_Preventive, Treat), 1,0)) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  anti_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month<=12 & ON_Prev==1) %>% select(patid) %>% distinct())

length(unique(MIGUS24_Drug_Histories_Extended_NoComorbs$patid))




Month_First <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  filter(ON_Prev==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month, weight) %>%
  rename("First"="Month")

sum(Month_First$weight)


before <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(Month_First)  %>% filter(Month<First)

before <- separate_rows(before, Treat, sep = ",", convert=T )

before <- before %>% select(patid, weight, Treat) %>% distinct()

before  %>% left_join(Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(n=sum(weight)/2506132)
  
  
  
data.frame(before  %>% left_join(Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  filter(drug_class!="Muscle Relaxant" & Treat!="-") %>%
  select(patid, weight, Treat) %>% distinct() %>% group_by(patid, weight) %>% count() %>%
  rename("N_mols"="n") %>% group_by(N_mols) %>% summarise(n=sum(weight)/3201456))

before  %>% left_join(Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  filter(drug_class!="Muscle Relaxant" & Treat!="-") %>%
  select(patid, weight, Treat) %>% distinct() %>% group_by(patid, weight) %>% count() %>% ungroup() %>% summarise(median=median(n))



# ----------------
# Months between onset Dx and 1st prev ---------------------------------

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|Drugs_lookup$drug_group=="CGRP Injectable")&!(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")



string_Sympt <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive")&(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Nasal <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Nasal"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2018-11-16") %>% select(patid, migraine_onset)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Demographics %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(ON_Prev=ifelse(grepl(string_Preventive, Treat), 1,0)) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  anti_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month<=12 & ON_Prev==1) %>% select(patid) %>% distinct())

length(unique(MIGUS24_Drug_Histories_Extended_NoComorbs$patid))




Month_First <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  filter(ON_Prev==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month, weight) %>%
  rename("First"="Month")


elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


diff <- Month_First %>% left_join(MIGUS24_Demographics) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) 

diff <- diff %>% mutate(diff=elapsed_months(migraine_onset, as.Date("2017-11-16"))) %>% arrange(-diff)

diff <- diff %>% mutate(diff=diff-First) %>% arrange(-diff)

diff %>% ggplot(aes(diff)) + geom_density()



# ----------------
# Pathways to preventive - number acute molecules before 1st preventive Pats with 1st Prev AFTER MIG Dx ---------------------------------

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|Drugs_lookup$drug_group=="CGRP Injectable")&!(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")



string_Sympt <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive")&(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Nasal <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Nasal"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid, migraine_onset)




MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Demographics %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(ON_Prev=ifelse(grepl(string_Preventive, Treat), 1,0)) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  anti_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month<=12 & ON_Prev==1) %>% select(patid) %>% distinct())

length(unique(MIGUS24_Drug_Histories_Extended_NoComorbs$patid))



Month_First_Prev <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  filter(ON_Prev==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month, weight) %>%
  rename("First_Prev"="Month")

Month_First_Acute <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  filter(grepl(string_CGRP_Oral, Treat)|grepl(string_Sympt, Treat)|grepl(string_Triptan, Treat)) %>%
  group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month, weight) %>%
  rename("First_Acute"="Month")


Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)  

To_track <- MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% inner_join(Month_First_Prev) %>% filter(First_Prev>=N) %>% select(patid, weight)

sum(To_track$weight) #1657310
 

before <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(Month_First_Prev)  %>% 
  inner_join(To_track) %>% filter(Month<First_Prev)

before <- separate_rows(before, Treat, sep = ",", convert=T )

before <- before %>% select(patid, weight, Treat) %>% distinct()

before  %>% left_join(Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(n=sum(weight)/1657310)
  
  
  
data.frame(before  %>% left_join(Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  filter(drug_class!="Muscle Relaxant" & Treat!="-") %>%
  select(patid, weight, Treat) %>% distinct() %>% group_by(patid, weight) %>% count() %>%
  rename("N_mols"="n") %>% group_by(N_mols) %>% summarise(n=sum(weight)/1657310))


before  %>% left_join(Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  filter(drug_class!="Muscle Relaxant" & Treat!="-") %>%
  select(patid, weight, Treat) %>% distinct() %>% group_by(patid, weight) %>% count() %>% ungroup() %>% summarise(median=median(n))

# ---------


# Months between onset Dx and 1st prev Pats with 1st Prev AFTER MIG Dx ---------------------------------

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|Drugs_lookup$drug_group=="CGRP Injectable")&!(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")



string_Sympt <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive")&(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Nasal <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Nasal"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid, migraine_onset)




MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Demographics %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(ON_Prev=ifelse(grepl(string_Preventive, Treat), 1,0)) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  anti_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month<=12 & ON_Prev==1) %>% select(patid) %>% distinct())

length(unique(MIGUS24_Drug_Histories_Extended_NoComorbs$patid))



Month_First_Prev <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  filter(ON_Prev==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month, weight) %>%
  rename("First_Prev"="Month")

Month_First_Acute <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  filter(grepl(string_CGRP_Oral, Treat)|grepl(string_Sympt, Treat)|grepl(string_Triptan, Treat)) %>%
  group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month, weight) %>%
  rename("First_Acute"="Month")


Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)  

To_track <- MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% inner_join(Month_First_Prev) %>% filter(First_Prev>=N) %>% select(patid, weight)


MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% inner_join(Month_First_Prev) %>% filter(First_Prev>=N) %>%
  mutate(Elapsed=First_Prev-N) %>% # summarise(mean=mean(Elapsed))
  ggplot(aes(Elapsed)) + 
  geom_density()

# ---------


# Pathways to preventive - NONE vs TRIPTAN vs SYMPTOMATIC GROUPS ---------------------------------

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|Drugs_lookup$drug_group=="CGRP Injectable")&!(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")



string_Sympt <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive")&(Drugs_lookup$drug_class!="Muscle Relaxant")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Nasal <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Nasal"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Demographics %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(ON_Prev=ifelse(grepl(string_Preventive, Treat), 1,0)) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  anti_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month<=12 & ON_Prev==1) %>% select(patid) %>% distinct())

length(unique(MIGUS24_Drug_Histories_Extended_NoComorbs$patid))


Month_First <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>%
  filter(ON_Prev==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month, weight) %>%
  rename("First"="Month")

sum(Month_First$weight) # 2506132


before <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(Month_First)  %>% filter(Month<First)

before <- separate_rows(before, Treat, sep = ",", convert=T )

before <- before %>% select(patid, weight, Treat) %>% distinct()

temp <- before  %>% left_join(Drugs_lookup %>% select(drug_id, drug_group) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_group) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=drug_group, value=exp)

temp[is.na(temp)] <- 0

temp %>%
  mutate(group=ifelse(Triptans==1 & Symptomatic==1, "Triptan+Sympt",
                      ifelse(Triptans==1, "Triptan",
                             ifelse(Symptomatic==1, "Sympt", "none")))) %>%
  group_by(group) %>% summarise(n=100*sum(weight)/2506132)

  
groups <- temp %>%
  mutate(group=ifelse(Triptans==1 & Symptomatic==1, "Triptan+Sympt",
                      ifelse(Triptans==1, "Triptan",
                             ifelse(Symptomatic==1, "Sympt", "none"))))



groups <- groups %>% select(patid, weight, group)
unique(groups$group)
groups <- groups %>% mutate(group=ifelse(group=="Triptan+Sympt", "Triptan", group))

groups <- groups %>% left_join(N_sympt) %>% 
  mutate(n=ifelse(is.na(n),0,n)) %>% 
  mutate(n=ifelse(n<4,0,1)) %>%
  mutate(group=ifelse(group=="Sympt"&n==0, "Sympt_Low",
                      ifelse(group=="Sympt"&n==1, "Sympt_High", group))) %>% select(-n)


groups %>% group_by(group) %>% summarise(n=sum(weight))

groups <- fread("Groups_Up_to_First_Preventive.txt")

# N TRIPTANS / SYMPTOMATIC BEFORE 1st PREVENTIVE

N_tripans <- before %>% filter(Treat!="-" & Treat!="77"& Treat!="78"& Treat!="79"& Treat!="80"& Treat!="81") %>%
  filter(grepl(string_Triptan, Treat)) %>%
  group_by(patid, weight) %>% count()


groups %>% left_join(N_tripans) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  group_by(group) %>% summarise(mean=mean(n))



N_sympt <- before %>% filter(Treat!="-" & Treat!="77"& Treat!="78"& Treat!="79"& Treat!="80"& Treat!="81") %>%
  filter(grepl(string_Sympt, Treat)) %>%
  group_by(patid, weight) %>% count()

groups %>% left_join(N_sympt) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  group_by(group) %>% summarise(mean=mean(n))


groups %>% left_join(N_sympt) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  mutate(n=ifelse(n>=8,8,n)) %>%
  group_by(group, n) %>% summarise(tot=sum(weight)) %>%
  spread(key=n, value=tot)


# EVER CGRP

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- groups %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>% filter(Treat!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, Treat)|grepl(string_CGRP_Oral, Treat)|grepl(string_CGRP_Nasal, Treat)) %>%
  select(patid) %>% mutate(CGRP="CGRP")

groups %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  group_by(group, CGRP) %>% count() %>% spread(key=CGRP, value=n) %>%
  mutate(perc=CGRP/(CGRP+`<NA>`))


# EVER CGRP Inj

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- groups %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>% filter(Treat!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, Treat)) %>%
  select(patid) %>% mutate(CGRP="CGRP")

groups %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  group_by(group, CGRP) %>% count() %>% spread(key=CGRP, value=n) %>%
  mutate(perc=CGRP/(CGRP+`<NA>`))



# EVER CGRP Oral

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- groups %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>% filter(Treat!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Oral, Treat)) %>%
  select(patid) %>% mutate(CGRP="CGRP")

groups %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  group_by(group, CGRP) %>% count() %>% spread(key=CGRP, value=n) %>%
  mutate(perc=CGRP/(CGRP+`<NA>`))

# MONTHS LAPSED

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- groups %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%  distinct() %>% filter(Treat=="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% count()

groups %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>% ungroup() %>%
  group_by(group) %>% mutate(n=ifelse(is.na(n),0,n)) %>% summarise(mean=mean(n))


# MONTHS PREVENTIVE

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- groups %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%  distinct() %>% filter(grepl(string_Preventive, Treat))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% count()

groups %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>% ungroup() %>%
  group_by(group) %>% mutate(n=ifelse(is.na(n),0,n)) %>% summarise(mean=mean(n))



# SPECIALTY FIRST PREVENTIVE

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")

MIGUS24_Doses <- MIGUS24_Doses %>% filter(drug_group=="Preventive"|drug_group=="CGRP Injectable")
unique(MIGUS24_Doses$drug_group)

MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, from_dt, provcat) %>% distinct() %>% 
  mutate(from_dt=as.Date(from_dt)) %>% group_by(patid) %>% filter(from_dt==min(from_dt)) %>% distinct()

PROVCAT <- read_excel("Source/CDM_Data_Reference_Lookup_Tables_V81.xlsx", sheet="PROVCAT", skip = 5)
PROVCAT <- PROVCAT %>% select(PROVCAT, DESCRIPTION)  %>% mutate(PROVCAT=as.numeric(PROVCAT))

MIGUS24_Doses <- MIGUS24_Doses %>%  inner_join(PROVCAT %>% select(PROVCAT, DESCRIPTION) %>% distinct(), by=c("provcat"="PROVCAT"))
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(groups %>% select(patid))

unique(MIGUS24_Doses$DESCRIPTION)

MIGUS24_Doses %>% filter(grepl("NEURO", DESCRIPTION)) %>% ungroup() %>% select(DESCRIPTION) %>% distinct()

MIGUS24_Doses <- MIGUS24_Doses %>% filter(grepl("NEUROL", DESCRIPTION)) %>% select(patid) %>% distinct() %>% mutate(NEURO="NEURO")


groups %>% left_join(MIGUS24_Doses) %>% ungroup() %>%
  group_by(group, NEURO) %>% count() %>% 
  spread(key=NEURO, value=n) %>%
  mutate(perc=NEURO/(NEURO+`<NA>`))






# THERAPY LINES 

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- groups %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>%  distinct() %>% filter(Treat!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% count()

groups %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>% ungroup() %>%
  group_by(group) %>% mutate(n=ifelse(is.na(n),0,n)) %>% summarise(mean=mean(n))

# MONTHS FROM DX TO 1st PREV

Month_First

Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid,migraine_onset)

MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% inner_join(Month_First) %>% 
  inner_join(groups) %>%
  mutate(elapsed=First-N) %>% mutate(elapsed=ifelse(is.na(elapsed),0,elapsed)) %>%
  group_by(group) %>% summarise(mean=mean(elapsed))

  
# MONTHS FROM DX TO 1st CGRP



MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- groups %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-")

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  filter(grepl(string_CGRP_Inj, Treat)|grepl(string_CGRP_Oral, Treat)|grepl(string_CGRP_Nasal, Treat)) %>%
  group_by(patid) %>% summarise(First=min(Month)) %>% select(patid, First) %>% ungroup()



Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid,migraine_onset)


MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>% 
  inner_join(groups) %>%
  mutate(elapsed=First-N) %>% mutate(elapsed=ifelse(is.na(elapsed),0,elapsed)) %>%
  group_by(group) %>% summarise(mean=mean(elapsed))


# Specialty 1st Dx

MIGUS24_Migraine_Dxs <- fread("Source/MIGUS24 Migraine Dxs.txt")
ZAVUS24_Migraine_Dxs <- ZAVUS24_Migraine_Dxs %>% mutate()


# ------------------
  
# Class penetrance month before current after 1st preventive by one of the 4 groups ---------

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|Drugs_lookup$drug_group=="CGRP Injectable")&!(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")



string_Sympt <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive")&(Drugs_lookup$drug_class!="Muscle Relaxant")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Nasal <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group=="CGRP Nasal"], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Demographics %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(ON_Prev=ifelse(grepl(string_Preventive, Treat), 1,0)) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  anti_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month<=12 & ON_Prev==1) %>% select(patid) %>% distinct())

length(unique(MIGUS24_Drug_Histories_Extended_NoComorbs$patid))


MIGUS24_Drug_Histories_Extended_NoComorbs_temp <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  inner_join(groups %>% filter(group=="Triptan") %>% select(patid) )


groups %>% group_by(group) %>% summarise(n=sum(weight))

Month_before <- MIGUS24_Drug_Histories_Extended_NoComorbs_temp %>% group_by(patid) %>%
  filter(lead(ON_Prev)==1) %>% group_by(patid) %>% filter(Month==min(Month))

sum(Month_before$weight)

Month_before <- Month_before %>% mutate(Box=ifelse(grepl(string_CGRP_Nasal, Treat), "Nasal",
                                   ifelse(grepl(string_CGRP_Oral, Treat), "Oral",
                                          ifelse(grepl(string_CGRP_Inj, Treat), "Inj",
                                                 ifelse(grepl(string_Preventive, Treat), "Prev",
                                                        ifelse(grepl(string_Triptan, Treat), "Triptan",
                                                               ifelse(grepl(string_Sympt, Treat), "Sympt", "Lapsed")))))))


Month_before <- separate_rows(Month_before, Treat, sep = ",", convert=T )

Month_before <- Month_before %>% left_join(Drugs_lookup %>% select(drug_id, drug_class) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(before=sum(weight))




Month_current <- MIGUS24_Drug_Histories_Extended_NoComorbs_temp %>% group_by(patid) %>%
  filter(ON_Prev==1) %>% group_by(patid) %>% filter(Month==min(Month))

Month_current <- Month_current %>% mutate(Box=ifelse(grepl(string_CGRP_Nasal, Treat), "Nasal",
                                   ifelse(grepl(string_CGRP_Oral, Treat), "Oral",
                                          ifelse(grepl(string_CGRP_Inj, Treat), "Inj",
                                                 ifelse(grepl(string_Preventive, Treat), "Prev",
                                                        ifelse(grepl(string_Triptan, Treat), "Triptan",
                                                               ifelse(grepl(string_Sympt, Treat), "Sympt", "Lapsed")))))))



Month_current <- separate_rows(Month_current, Treat, sep = ",", convert=T )

Month_current <- Month_current %>% mutate(Treat=as.character(Treat)) %>%
  left_join(Drugs_lookup %>% select(drug_id, drug_class) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(current=sum(weight))





Month_after <- MIGUS24_Drug_Histories_Extended_NoComorbs_temp %>% group_by(patid) %>%
  filter(lag(ON_Prev)==1) %>% group_by(patid) %>% filter(Month==min(Month))

Month_after <- Month_after %>% mutate(Box=ifelse(grepl(string_CGRP_Nasal, Treat), "Nasal",
                                   ifelse(grepl(string_CGRP_Oral, Treat), "Oral",
                                          ifelse(grepl(string_CGRP_Inj, Treat), "Inj",
                                                 ifelse(grepl(string_Preventive, Treat), "Prev",
                                                        ifelse(grepl(string_Triptan, Treat), "Triptan",
                                                               ifelse(grepl(string_Sympt, Treat), "Sympt", "Lapsed")))))))


Month_after <- separate_rows(Month_after, Treat, sep = ",", convert=T )

Month_after <- Month_after %>% mutate(Treat=as.character(Treat)) %>%
  left_join(Drugs_lookup %>% select(drug_id, drug_class) %>% 
                             mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id") ) %>%
  select(patid, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(after=sum(weight))

data.frame(Month_before %>% full_join(Month_current) %>% full_join(Month_after)) %>% arrange(drug_class)


# -------
# Compare Heavy vs Low Volume Treat ---------

# Groups
Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid,migraine_onset)
MIGUS24_Demographics <- MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% select(patid, N)

groups <- fread("Source/Groups_Up_to_First_Preventive.txt")
unique(groups$group)


# Nr Lines
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>%
  group_by(patid) %>% count() %>% inner_join(groups) %>% mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low")) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# % Time Treated
MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(Elapsed=60-N+1) %>%
  group_by(patid, Elapsed) %>% count() %>% ungroup() %>%
  mutate(perc=n/Elapsed) %>%
  inner_join(groups %>% mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low"))) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(perc))


# Nr Molecules
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  group_by(patid) %>% count() %>%
  inner_join(groups %>% mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low"))) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# Nr Flows
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% filter(Treat!=lag(Treat)) %>% ungroup() %>%
  group_by(patid) %>% count() %>%
  inner_join(groups %>% mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low"))) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# Preventives 
Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|
                                                                  Drugs_lookup$drug_group=="CGRP Injectable")&
                                                                 !(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 

MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid)) %>%
  filter(grepl(string_Preventive, Treat)) %>%
  group_by(patid) %>% count() %>%
  inner_join(groups %>% mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low"))) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))

MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>% filter(grepl(string_Preventive, Treat))
MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  group_by(patid) %>% count() %>%
  inner_join(groups %>% mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low"))) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# Age
Age <- fread("Source/MIGUS24 Demographics.txt")
Age <- Age %>% select(patid, AGE)

Age  %>% inner_join(groups %>% mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low"))) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(AGE))


# Mig Dxs
MIGUS24_Comorbidities_Extended_Dxs <- fread("Source/MIGUS24 Comorbidities Extended Dxs.txt")
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)) %>% 
  select(patid, date) %>% distinct() %>% group_by(patid) %>% count()

MIGUS24_Comorbidities_Extended_Dxs %>% inner_join(groups %>% mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low"))) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# Neurol
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, provcat) 
  PROVCAT <- read_excel("Source/CDM_Data_Reference_Lookup_Tables_V81.xlsx", sheet="PROVCAT", skip = 5)
PROVCAT <- PROVCAT %>% select(PROVCAT, DESCRIPTION)  %>% mutate(PROVCAT=as.numeric(PROVCAT))
MIGUS24_Doses <- MIGUS24_Doses %>%  inner_join(PROVCAT %>% select(PROVCAT, DESCRIPTION) %>% distinct(), by=c("provcat"="PROVCAT"))
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(groups %>% select(patid))

unique(MIGUS24_Doses$DESCRIPTION)

specialties_drug_doses_alloc <- fread("Source/specialties_drug_doses_alloc.csv")

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(specialties_drug_doses_alloc, by=c("DESCRIPTION"="original")) %>%
  filter(final %in% c("GP", "IM", "NEURO", "OTHER PHYSICIAN") )
  

groups %>% 
  left_join(MIGUS24_Doses %>% filter(final=="NEURO") %>% select(patid) %>% distinct() %>% mutate(NEURO="NEURO")) %>%
  mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low")) %>%
  group_by(group, NEURO) %>% count() %>% spread(key=NEURO, value=n) %>%
  mutate(perc=NEURO/(NEURO+`<NA>`))


groups %>% 
  left_join(MIGUS24_Doses) %>%
  mutate(group=ifelse(group=="Sympt_High"|group=="Triptan", "High", "Low")) %>%
  group_by(group, final) %>% count() %>% drop_na() %>%
  spread(key=final, value=n) %>%
  mutate(perc=NEURO/(NEURO+IM+GP+`OTHER PHYSICIAN`))


# --------

# Compare Start Inj vs Start Oral ---------

# Groups
Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid,migraine_onset)
MIGUS24_Demographics <- MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% select(patid, N)

groups <- fread("Source/Groups_Up_to_First_Preventive.txt")
unique(groups$group)

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="CGRP Injectable")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="CGRP Oral")], collapse = "|"),")\\b")


# CGRP inj pats
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)

First_CGRP <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, Treat)|grepl(string_CGRP_Oral, Treat)) %>% select(patid) %>% distinct() %>%
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  filter(grepl(string_CGRP_Inj, Treat)|grepl(string_CGRP_Oral, Treat)) %>% group_by(patid) %>% filter(Month==min(Month)) %>% select(patid, Month, Treat) %>%
  mutate(Inj=ifelse(grepl(string_CGRP_Inj, Treat),1,0)) %>% mutate(Oral=ifelse(grepl(string_CGRP_Oral, Treat),1,0)) %>%
  select(patid, Month, Inj, Oral) %>% rename("First"="Month")

First_CGRP %>% group_by(Inj, Oral) %>% count()

First_CGRP <- First_CGRP %>% filter(!(Inj==1&Oral==1))

First_CGRP <- First_CGRP %>% 
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, Treat)) %>% select(patid) %>% distinct() %>% mutate(Ever_Inj=1)) %>% 
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Oral, Treat)) %>% select(patid) %>% distinct() %>% mutate(Ever_Oral=1)) 


First_CGRP %>% group_by(Inj, Ever_Oral) %>% count()
First_CGRP %>% group_by(Oral, Ever_Inj) %>% count()


groups <- groups %>% inner_join(First_CGRP)  %>% mutate(group=ifelse(Inj==1, "Inj", "Oral")) %>% select(patid, group)


# Nr Lines
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>%
  group_by(patid) %>% count() %>% inner_join(groups)  %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# % Time Treated
MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(Elapsed=60-N+1) %>%
  group_by(patid, Elapsed) %>% count() %>% ungroup() %>%
  mutate(perc=n/Elapsed) %>%
  inner_join(groups) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(perc))


# Nr Molecules
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  group_by(patid) %>% count() %>%
  inner_join(groups) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# Nr Flows
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% filter(Treat!=lag(Treat)) %>% ungroup() %>%
  group_by(patid) %>% count() %>%
  inner_join(groups ) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# Preventives 
Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|
                                                                  Drugs_lookup$drug_group=="CGRP Injectable")&
                                                                 !(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 

MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid)) %>%
  filter(grepl(string_Preventive, Treat)) %>%
  group_by(patid) %>% count() %>%
  inner_join(groups) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))

MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>% filter(grepl(string_Preventive, Treat))
MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  group_by(patid) %>% count() %>%
  inner_join(groups ) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# Age
Age <- fread("Source/MIGUS24 Demographics.txt")
Age <- Age %>% select(patid, AGE)

Age  %>% inner_join(groups ) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(AGE))


# Mig Dxs
MIGUS24_Comorbidities_Extended_Dxs <- fread("Source/MIGUS24 Comorbidities Extended Dxs.txt")
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)) %>% 
  select(patid, date) %>% distinct() %>% group_by(patid) %>% count()

MIGUS24_Comorbidities_Extended_Dxs %>% inner_join(groups) %>%
  ungroup() %>% group_by(group) %>% summarise(mean=mean(n))


# Neurol
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, provcat) 
  PROVCAT <- read_excel("Source/CDM_Data_Reference_Lookup_Tables_V81.xlsx", sheet="PROVCAT", skip = 5)
PROVCAT <- PROVCAT %>% select(PROVCAT, DESCRIPTION)  %>% mutate(PROVCAT=as.numeric(PROVCAT))
MIGUS24_Doses <- MIGUS24_Doses %>%  inner_join(PROVCAT %>% select(PROVCAT, DESCRIPTION) %>% distinct(), by=c("provcat"="PROVCAT"))
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(groups %>% select(patid))

unique(MIGUS24_Doses$DESCRIPTION)

specialties_drug_doses_alloc <- fread("Source/specialties_drug_doses_alloc.csv")

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(specialties_drug_doses_alloc, by=c("DESCRIPTION"="original")) %>%
  filter(final %in% c("GP", "IM", "NEURO", "OTHER PHYSICIAN") )
  

groups %>% 
  left_join(MIGUS24_Doses %>% filter(final=="NEURO") %>% select(patid) %>% distinct() %>% mutate(NEURO="NEURO")) %>%
  group_by(group, NEURO) %>% count() %>% spread(key=NEURO, value=n) %>%
  mutate(perc=NEURO/(NEURO+`<NA>`))



groups %>% 
  left_join(MIGUS24_Doses) %>%
  group_by(group, final) %>% count() %>% drop_na() %>%
  spread(key=final, value=n) %>%
  mutate(perc=NEURO/(NEURO+IM+GP+`OTHER PHYSICIAN`))


# --------

# Time from Dx to 1st CGRP over time & Probability of starting ---------


Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid,migraine_onset)
MIGUS24_Demographics <- MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% select(patid, N)

groups <- fread("Source/Groups_Up_to_First_Preventive.txt")
unique(groups$group)


Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="CGRP Injectable")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="CGRP Oral")], collapse = "|"),")\\b")



MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)

First_CGRP <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, Treat)|grepl(string_CGRP_Oral, Treat)) %>% 
  group_by(patid) %>% filter(Month==min(Month)) %>% rename("First"="Month") %>% select(patid, First)


MIGUS24_Demographics %>% inner_join(First_CGRP) %>%
  ggplot(aes(N, First-N)) +
  geom_smooth(colour="firebrick", fill="deepskyblue4", method="loess") +
  theme_minimal() +
  xlab("\n Migraine Onset Month") +
  ylab("Number of Elapsed MOnths \n From Migraine Onset to 1st CGRP \n ")



MIGUS24_Demographics %>% left_join(First_CGRP %>% mutate(First=1)) %>%
  mutate(First=ifelse(is.na(First),0,First)) %>%
  ggplot(aes(N, First)) +
  stat_smooth(colour="firebrick", fill="deepskyblue4", method = "glm", family="poisson") +
  theme_minimal() +
  coord_cartesian(ylim=c(0,0.04)) + 
    xlab("\n Migraine Onset Month") +
  ylab("Probability of having tried \n Any CGRP \n ")




MIGUS24_Demographics %>% inner_join(First_CGRP)  %>%
  ggplot(aes(N)) +
  geom_density(colour="firebrick", fill="deepskyblue4", alpha=0.3, linewidth=2) +
   theme_minimal() +
  xlab("\n Migraine Onset Month") +
  ylab("Proportion of CGRP Patients \n ")



MIGUS24_Demographics %>% 
  ggplot(aes(N)) +
  geom_density(colour="firebrick", fill="deepskyblue4", alpha=0.3, linewidth=2) +
   theme_minimal() +
  xlab("\n Migraine Onset Month") +
  ylab("Proportion of All Patients \n ")


# -------------- 
# Pathways Start Inj vs Start Oral ---------

# Groups
Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid,migraine_onset)
MIGUS24_Demographics <- MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% select(patid, N)

groups <- fread("Source/Groups_Up_to_First_Preventive.txt")
unique(groups$group)

Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="CGRP Injectable")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="CGRP Oral")], collapse = "|"),")\\b")


# CGRP pats
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)

CGRP_Inj <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Inj, Treat)) %>% 
  group_by(patid) %>% filter(Month==min(Month)) %>% rename("First_Inj"="Month") %>% select(patid, First_Inj)

CGRP_Oral <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_CGRP_Oral, Treat)) %>% 
  group_by(patid) %>% filter(Month==min(Month)) %>% rename("First_Oral"="Month") %>% select(patid, First_Oral)

CGRP_Firsts <- CGRP_Inj %>% full_join(CGRP_Oral)

CGRP_Firsts <- CGRP_Firsts %>% mutate(First=ifelse(is.na(First_Inj), First_Oral,
                                    ifelse(is.na(First_Oral), First_Inj,
                                           ifelse(First_Inj<=First_Oral, First_Inj, First_Oral))))

CGRP_Firsts <- CGRP_Firsts %>% mutate(second=ifelse(First_Inj>First_Oral, First_Inj,
                                                    ifelse(First_Oral>=First_Inj, First_Oral, NA)))


CGRP_Firsts <- CGRP_Firsts %>% mutate(Inj=ifelse(is.na(First_Inj),0,1)) %>% mutate(Oral=ifelse(is.na(First_Oral),0,1))


# Preventives 
Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|
                                                                  Drugs_lookup$drug_group=="CGRP Injectable")&
                                                                 !(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Month, Treat)




MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(CGRP_Firsts) %>%
  filter(Month<=First) %>% filter(grepl(string_Preventive, Treat)) %>%
  group_by(patid, Inj, Oral) %>% count() %>% ungroup() %>%
  group_by(Inj, Oral) %>% summarise(mean=mean(n))


MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(CGRP_Firsts) %>% filter(Month<=First)

MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl(string_Preventive, Treat)) %>%
  select(-Month) %>% distinct() %>%
  group_by(patid, Inj, Oral) %>% count() %>% ungroup() %>%
  group_by(Inj, Oral) %>% summarise(mean=mean(n))




MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Month, Treat)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(CGRP_Firsts) %>%
  filter(Month<=First) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id")) %>%
  group_by(patid, drug_group) %>% filter(Month==min(Month))



data.frame(MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, drug_group) %>%
  distinct() %>% group_by(patid) %>% mutate(drug_group=paste0(drug_group, collapse = " -> ")) %>%
  distinct() %>% ungroup() %>% group_by(drug_group) %>% count() %>%
  arrange(-n))






# --------

# Acute vs Preventive Riemgepant patients/scripts ---------

groups <- fread("Source/Groups_Up_to_First_Preventive.txt")
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- groups %>% select(patid) %>% inner_join(MIGUS24_Doses)
RIME_pats <- MIGUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid) %>% distinct()
MIGUS24_Doses <- RIME_pats %>% left_join(MIGUS24_Doses) %>% filter(generic=="Rimegepant")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
range(MIGUS24_Doses$from_dt)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% arrange(patid, generic, from_dt) 
MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
MIGUS24_Doses <- MIGUS24_Doses %>% drop_na()
MIGUS24_Doses <- MIGUS24_Doses %>% filter(elapsed <= 92)
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

MIGUS24_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(generic, mean) %>% summarise(n=sum(as.numeric(weight)))


MIGUS24_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  ggplot(aes(mean)) +
  geom_density(colour="firebrick", linewidth=2, fill="deepskyblue4", alpha=0.5) +
  xlim(0,30) +
  theme_minimal() +
  xlab("\n Average Treatment Rate per Month per Patient") + ylab("Patient density \n")





groups <- fread("Source/Groups_Up_to_First_Preventive.txt")
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- groups %>% select(patid) %>% inner_join(MIGUS24_Doses)

MIGUS24_Doses <- MIGUS24_Doses %>% filter(generic=="Rimegepant")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2021-05-15") %>% filter(days_sup  != "")


MIGUS24_Doses <- MIGUS24_Doses %>% arrange(patid, generic, from_dt) 

MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
MIGUS24_Doses <- MIGUS24_Doses %>% drop_na()

data.frame(MIGUS24_Doses) 

MIGUS24_Doses <- MIGUS24_Doses %>% filter(elapsed <= 92)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))


MIGUS24_Doses$from_dt <- substr(as.character(MIGUS24_Doses$from_dt), 1, 7)

data.frame(MIGUS24_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
             filter(from_dt!="2023-12"&from_dt!="2024-01") %>%
  group_by(patid, weight, generic, from_dt) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(from_dt, generic, mean) %>% summarise(n=sum(as.numeric(weight))) %>%
    ungroup() %>%
  spread(key=mean, value=n)) %>%
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Acute_Proportion=100*Acute/(Acute+Prev)) %>%
  ggplot(aes(from_dt, Acute_Proportion, colour="firebrick")) +
  geom_point(colour="firebrick", size=3, alpha=0.5, stroke=2) +
  theme_minimal() +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("\n Month") + ylab("Proportion of Acute patient scripts \n")

    
# --------------
# 2 box model Rimegepant flows ---------------------

groups <- fread("Source/Groups_Up_to_First_Preventive.txt")
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- groups %>% select(patid) %>% inner_join(MIGUS24_Doses)

MIGUS24_Doses <- MIGUS24_Doses %>% filter(generic=="Rimegepant" & status != "G") %>% select(-c(provider, provcat, status, code, NPI, generic, drug_id, drug_class, drug_group))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2021-11-16"&from_dt <= "2023-11-16") %>% filter(days_sup!="")


MIGUS24_Doses <- MIGUS24_Doses %>% arrange(patid, from_dt) 

MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
MIGUS24_Doses <- MIGUS24_Doses %>% drop_na()

data.frame(MIGUS24_Doses) 

#MIGUS24_Doses <- MIGUS24_Doses %>% filter(elapsed <= 92)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))


pats_m48 <- MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  filter(from_dt<="2022-11-16") %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>%  ungroup() %>%
  mutate(diff=as.numeric(as.Date("2022-11-16")-from_dt)) %>% 
  filter(diff<=92) %>% select(patid, weight, rate) %>% rename("m48"="rate")


MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  #filter(from_dt<="2022-06-16") %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>%  ungroup() %>%
  mutate(diff=as.numeric(as.Date("2023-11-16")-from_dt)) %>% 
  filter(diff<=92) %>%
  full_join(pats_m48) %>%  rename("m60"="rate") %>%
  group_by(m48, m60) %>% summarise(n=sum(as.numeric(weight)))
   


MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  #group_by(patid) %>%  filter(from_dt==max(from_dt)) %>% filter(qty==max(qty)) %>% slice(1) %>% ungroup() %>%
  #mutate(diff=as.numeric(as.Date("2023-05-15")-from_dt)) %>% 
  #filter(diff<=122) %>%
  group_by(patid, weight, rate) %>% count() %>%
  spread(key=rate, value=n) %>%
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Prev=ifelse(is.na(Prev),0,Prev)) %>%
  ggplot(aes(Acute, Prev)) +
  geom_jitter(alpha=0.5)
  
   


MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>%
  group_by(patid) %>% filter(rate=="Prev"&lag(rate)=="Acute") %>% ungroup() %>%
  summarise(n=sum(as.numeric(weight)))



MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>%  ungroup() %>%
  mutate(diff=as.numeric(as.Date("2023-11-16")-from_dt)) %>% 
  filter(diff<=92) %>%
  group_by(rate) %>% summarise(n=sum(as.numeric(weight)))
   




MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  group_by(patid) %>%  filter(from_dt==max(from_dt) ) %>% filter(from_dt>="2023-1-16") %>% 
  select(patid, weight, rate) %>% rename("Final"="rate") %>%
  full_join(
    MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  group_by(patid) %>%  filter(from_dt==min(from_dt)) %>% filter(from_dt>="2023-11-16") %>% 
  select(patid, weight, rate) %>% rename("First"="rate")
  ) %>%
  group_by(First, Final) %>% summarise(n=sum(as.numeric(weight)))




MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  #group_by(patid) %>%  filter(from_dt==max(from_dt)) %>% filter(qty==max(qty)) %>% slice(1) %>% ungroup() %>%
  #mutate(diff=as.numeric(as.Date("2023-05-15")-from_dt)) %>% 
  #filter(diff<=122) %>%
  group_by(patid, weight, rate) %>% count() %>%
  spread(key=rate, value=n) %>%
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Prev=ifelse(is.na(Prev),0,Prev)) %>%
  mutate(group=ifelse(Acute>0&Prev==0, "Acute",
                      ifelse(Prev>0&Acute==0, "Prev", "Both"))) %>%
  group_by(group) %>% summarise(n=sum(as.numeric(weight)))


# -------------------------
# Highest Therapy Used Year over Year ------

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")

Mod_Sev <- fread("Source/Mod_Sev.txt")

MIGUS24_Drug_Histories_Extended_NoComorbs <- Mod_Sev %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
sum(MIGUS24_Drug_Histories_Extended_NoComorbs$weight) # 12768773

MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))

Pats_months <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-Treat) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  left_join(Drug_formulary, by=c("Treat"="drug_id")) %>% select(-Treat) %>% distinct()

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)

MIGUS24_Drug_Histories_Extended_NoComorbs[is.na(MIGUS24_Drug_Histories_Extended_NoComorbs)] <- 0

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  arrange(patid, Month) %>% group_by(patid) %>%
  mutate(`CGRP Injectable`=cumsum(`CGRP Injectable`)) %>% mutate(`CGRP Injectable`=ifelse(`CGRP Injectable`==0,0,1)) %>%
  mutate(`CGRP Nasal`=cumsum(`CGRP Nasal`)) %>% mutate(`CGRP Nasal`=ifelse(`CGRP Nasal`==0,0,1)) %>%
  mutate(`CGRP Oral`=cumsum(`CGRP Oral`)) %>% mutate(`CGRP Oral`=ifelse(`CGRP Oral`==0,0,1)) %>%
  mutate(Preventive=cumsum(Preventive)) %>% mutate(Preventive=ifelse(Preventive==0,0,1)) %>%
  mutate(Symptomatic=cumsum(Symptomatic)) %>%  mutate(Symptomatic=ifelse(Symptomatic==0,0,1)) %>%
  mutate(Triptans=cumsum(Triptans)) %>% mutate(Triptans=ifelse(Triptans==0,0,1))


MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(Box=ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                                ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                                       ifelse(Preventive==1, "Prev",
                                                                              ifelse(Triptans==1, "Triptan",
                                                                                     ifelse(Symptomatic==1, "Sympt", "none"))))))



MIGUS24_Drug_Histories_Extended_NoComorbs <- data.frame(Pats_months %>% 
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, Month, Box)) %>%
  arrange(patid, Month) %>%
  group_by(patid) %>% fill(Box) %>%
    mutate(Box=ifelse(is.na(Box), "Lapsed", Box)))

MIGUS24_Drug_Histories_Extended_NoComorbs

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month %in% c(12,24,36,48,60)) %>%
  spread(key=Month, value=Box)


fwrite(MIGUS24_Drug_Histories_Extended_NoComorbs, "Highest_Cumm_Box_Years_MIGUS.txt")

fwrite(MIGUS24_Drug_Histories_Extended_NoComorbs, "Highest_Cumm_Box_Years_MIGUS.csv")



MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`60`=ifelse(`60`=="Sympt", "Triptan", `60`))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`48`=ifelse(`48`=="Sympt", "Triptan", `48`))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`36`=ifelse(`36`=="Sympt", "Triptan", `36`))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`24`=ifelse(`24`=="Sympt", "Triptan", `24`))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`12`=ifelse(`12`=="Sympt", "Triptan", `12`))


MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`60`) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`48`) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`36`) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`24`) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`12`) %>% summarise(n=sum(weight))


MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`48`, `60`) %>% summarise(n=sum(weight)) 
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`36`, `48`) %>% summarise(n=sum(weight)) 
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`24`, `36`) %>% summarise(n=sum(weight)) 
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`12`, `24`) %>% summarise(n=sum(weight)) 


groups <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, `60`) 



# ----------
# Compare kpis based on highest box month 60 ------
Dx_Months <- fread("Source/MIGUS24 Demographics.txt")
Dx_Months <- Dx_Months %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.character(migraine_onset)) %>% mutate(migraine_onset=str_sub(migraine_onset,1L,7L))
Dx_Months <- Dx_Months %>% select(migraine_onset) %>% distinct() %>% arrange(migraine_onset)
Dx_Months <- Dx_Months %>% drop_na() %>% mutate(N=row_number()-13)

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, migraine_onset) %>%
  mutate(migraine_onset=as.Date(migraine_onset)) %>% filter(migraine_onset>="2019-11-16") %>% select(patid,migraine_onset)
MIGUS24_Demographics <- MIGUS24_Demographics %>%  mutate(migraine_onset=as.character(migraine_onset)) %>% 
  mutate(migraine_onset=str_sub(migraine_onset,1L,7L)) %>%
  left_join(Dx_Months) %>% select(patid, N)


# Nr Lines
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)

MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>%
  group_by(patid) %>% count() %>% inner_join(groups)  %>%
  ungroup() %>% group_by(`60`) %>% summarise(mean=mean(n))


# % Time Treated
MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(Elapsed=60-N+1) %>%
  group_by(patid, Elapsed) %>% count() %>% ungroup() %>%
  mutate(perc=n/Elapsed) %>%
  inner_join(groups) %>%
  ungroup() %>% group_by(`60`) %>% summarise(mean=mean(perc))


# Nr Molecules
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  group_by(patid) %>% count() %>%
  inner_join(groups) %>%
  ungroup() %>% group_by(`60`) %>% summarise(mean=mean(n))


# Nr Flows
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% filter(Treat!=lag(Treat)) %>% ungroup() %>%
  group_by(patid) %>% count() %>%
  inner_join(groups ) %>%
  ungroup() %>% group_by(`60`) %>% summarise(mean=mean(n))


# Preventives 
Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="Preventive"|
                                                                  Drugs_lookup$drug_group=="CGRP Injectable")&
                                                                 !(Drugs_lookup$drug_class=="Muscle Relaxant")], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(MIGUS24_Demographics) %>% filter(Month>=N)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") 

MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(groups %>% select(patid)) %>%
  filter(grepl(string_Preventive, Treat)) %>%
  group_by(patid) %>% count() %>%
  inner_join(groups) %>%
  ungroup() %>% group_by(`60`) %>% summarise(mean=mean(n))

MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, Treat) %>% distinct() %>% filter(grepl(string_Preventive, Treat))
MIGUS24_Drug_Histories_Extended_NoComorbs %>%
  group_by(patid) %>% count() %>%
  inner_join(groups ) %>%
  ungroup() %>% group_by(`60`) %>% summarise(mean=mean(n))


# Age
Age <- fread("Source/MIGUS24 Demographics.txt")
Age <- Age %>% select(patid, AGE)

Age  %>% inner_join(groups ) %>%
  ungroup() %>% group_by(`60`) %>% summarise(mean=mean(AGE))


# Mig Dxs
MIGUS24_Comorbidities_Extended_Dxs <- fread("Source/MIGUS24 Comorbidities Extended Dxs.txt")
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)) %>% 
  select(patid, date) %>% distinct() %>% group_by(patid) %>% count()

MIGUS24_Comorbidities_Extended_Dxs %>% inner_join(groups) %>%
  ungroup() %>% group_by(`60`) %>% summarise(mean=mean(n))


# Neurol
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, provcat) 
  PROVCAT <- read_excel("Source/CDM_Data_Reference_Lookup_Tables_V81.xlsx", sheet="PROVCAT", skip = 5)
PROVCAT <- PROVCAT %>% select(PROVCAT, DESCRIPTION)  %>% mutate(PROVCAT=as.numeric(PROVCAT))
MIGUS24_Doses <- MIGUS24_Doses %>%  inner_join(PROVCAT %>% select(PROVCAT, DESCRIPTION) %>% distinct(), by=c("provcat"="PROVCAT"))
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(groups %>% select(patid))

unique(MIGUS24_Doses$DESCRIPTION)

specialties_drug_doses_alloc <- fread("Source/specialties_drug_doses_alloc.csv")

MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(specialties_drug_doses_alloc, by=c("DESCRIPTION"="original")) %>%
  filter(final %in% c("GP", "IM", "NEURO", "OTHER PHYSICIAN") )
  

groups %>% 
  left_join(MIGUS24_Doses %>% filter(final=="NEURO") %>% select(patid) %>% distinct() %>% mutate(NEURO="NEURO")) %>%
  group_by(`60`, NEURO) %>% count() %>% spread(key=NEURO, value=n) %>%
  mutate(perc=NEURO/(NEURO+`<NA>`))



groups %>% 
  left_join(MIGUS24_Doses) %>%
  group_by(`60`, final) %>% count() %>% drop_na() %>%
  spread(key=final, value=n) %>%
  mutate(perc=NEURO/(NEURO+IM+GP+`OTHER PHYSICIAN`))

# ---------
# Flows last 12 months for preventive past highest box m48 ------

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")

Mod_Sev <- fread("Source/Mod_Sev.txt")

MIGUS24_Drug_Histories_Extended_NoComorbs <- Mod_Sev %>% select(patid) %>% inner_join(MIGUS24_Drug_Histories_Extended_NoComorbs)
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
sum(MIGUS24_Drug_Histories_Extended_NoComorbs$weight) # 12768773

MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))

Pats_months <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-Treat) 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-")
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  left_join(Drug_formulary, by=c("Treat"="drug_id")) %>% select(-Treat) %>% distinct()

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)

MIGUS24_Drug_Histories_Extended_NoComorbs[is.na(MIGUS24_Drug_Histories_Extended_NoComorbs)] <- 0

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
  arrange(patid, Month) %>% group_by(patid) %>%
  mutate(`CGRP Injectable`=cumsum(`CGRP Injectable`)) %>% mutate(`CGRP Injectable`=ifelse(`CGRP Injectable`==0,0,1)) %>%
  mutate(`CGRP Nasal`=cumsum(`CGRP Nasal`)) %>% mutate(`CGRP Nasal`=ifelse(`CGRP Nasal`==0,0,1)) %>%
  mutate(`CGRP Oral`=cumsum(`CGRP Oral`)) %>% mutate(`CGRP Oral`=ifelse(`CGRP Oral`==0,0,1)) %>%
  mutate(Preventive=cumsum(Preventive)) %>% mutate(Preventive=ifelse(Preventive==0,0,1)) %>%
  mutate(Symptomatic=cumsum(Symptomatic)) %>%  mutate(Symptomatic=ifelse(Symptomatic==0,0,1)) %>%
  mutate(Triptans=cumsum(Triptans)) %>% mutate(Triptans=ifelse(Triptans==0,0,1))


MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(Box=ifelse(`CGRP Oral`==1, "CGRP_Oral",
                                                                ifelse(`CGRP Injectable`==1, "CGRP_Inj",
                                                                       ifelse(Preventive==1, "Prev",
                                                                              ifelse(Triptans==1, "Triptan",
                                                                                     ifelse(Symptomatic==1, "Sympt", "none"))))))



MIGUS24_Drug_Histories_Extended_NoComorbs <- data.frame(Pats_months %>% 
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, weight, Month, Box)) %>%
  arrange(patid, Month) %>%
  group_by(patid) %>% fill(Box) %>%
    mutate(Box=ifelse(is.na(Box), "Lapsed", Box)))

MIGUS24_Drug_Histories_Extended_NoComorbs

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month %in% c(12,24,36,48,60)) %>%
  spread(key=Month, value=Box)


fwrite(MIGUS24_Drug_Histories_Extended_NoComorbs, "Highest_Cumm_Box_Years_MIGUS.txt")

fwrite(MIGUS24_Drug_Histories_Extended_NoComorbs, "Highest_Cumm_Box_Years_MIGUS.csv")



MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`60`=ifelse(`60`=="Sympt", "Triptan", `60`))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`48`=ifelse(`48`=="Sympt", "Triptan", `48`))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`36`=ifelse(`36`=="Sympt", "Triptan", `36`))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`24`=ifelse(`24`=="Sympt", "Triptan", `24`))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(`12`=ifelse(`12`=="Sympt", "Triptan", `12`))


MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`60`) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`48`) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`36`) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`24`) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`12`) %>% summarise(n=sum(weight))


MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`48`, `60`) %>% summarise(n=sum(weight)) 
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`36`, `48`) %>% summarise(n=sum(weight)) 
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`24`, `36`) %>% summarise(n=sum(weight)) 
MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(`12`, `24`) %>% summarise(n=sum(weight)) 


groups <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(patid, `48`)  


MIGUS24_Flows_Long <- fread("Source/MIGUS24 Flows_Long.txt")
MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% select(patient, weight, p1, p2, d1, d2, s1,s2, flow, stops, starts, re_starts)
MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% inner_join(groups %>% filter(`48`=="Prev"), by=c("patient"="patid"))

MIGUS24_Flows_Long %>% select(patient, weight) %>% distinct() %>% summarise(tot=sum(weight))  # 7457204

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% filter(p1>=48) %>% filter(stops!=1) %>% select(-stops, starts, re_starts, `48`) %>% filter(flow==1) %>% select(-flow)

MIGUS24_Flows_Long %>% select(patient, weight) %>% distinct() %>% summarise(tot=sum(weight))  # 6827262 # 0.9155257

data <- MIGUS24_Flows_Long

Drug_formulary <- fread("Source/Drug_formulary.txt")
unique(Drug_formulary$drug_group)
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")

string_Acute <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Triptans"|Drug_formulary$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Preventative <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")



# Oral CGRP Therapy class (O) - flags
data <- data[, Oral_CGRP_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_CGRP_Oral), str_c(unlist(str_extract_all(x, string_CGRP_Oral)), collapse = ","),"")))]
data <- data[, Oral_CGRP_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_CGRP_Oral), str_c(unlist(str_extract_all(x, string_CGRP_Oral)), collapse = ","),"")))]
data <- data[, nr_Oral_CGRP_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_CGRP_Oral)*1), str_split(x,","))))]
data <- data[, nr_Oral_CGRP_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_CGRP_Oral)*1), str_split(x,","))))]
data <- data[, nr_Oral_CGRP_Unq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Oral_CGRP_d1","Oral_CGRP_d2")] 
data <- data[, Oral_CGRP_flow_type := ifelse(nr_Oral_CGRP_d2 < nr_Oral_CGRP_d1 & nr_Oral_CGRP_Unq_d1d2 > nr_Oral_CGRP_d1, "D+S", 
                                             ifelse(nr_Oral_CGRP_d2 > nr_Oral_CGRP_d1 & nr_Oral_CGRP_Unq_d1d2 > nr_Oral_CGRP_d2, "A+S",
                                                    ifelse(nr_Oral_CGRP_d2 < nr_Oral_CGRP_d1, "D", 
                                                           ifelse(nr_Oral_CGRP_d2 > nr_Oral_CGRP_d1, "A", 
                                                                  ifelse(nr_Oral_CGRP_d2 == nr_Oral_CGRP_d1 & Oral_CGRP_d2 != Oral_CGRP_d1, "S","-")))))] 


# Injectable CGRP Therapy class (I) - flags
data <- data[, Injectable_CGRP_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x,string_CGRP_Inj),str_c(unlist(str_extract_all(x,string_CGRP_Inj)),collapse = ","),"")))]
data <- data[, Injectable_CGRP_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x,string_CGRP_Inj),str_c(unlist(str_extract_all(x,string_CGRP_Inj)),collapse = ","),"")))]
data <- data[, nr_Injectable_CGRP_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_CGRP_Inj)*1), str_split(x,","))))]
data <- data[, nr_Injectable_CGRP_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_CGRP_Inj)*1), str_split(x,","))))]
data <- data[, nr_Injectable_CGRP_Unq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Injectable_CGRP_d1","Injectable_CGRP_d2")] 
data <- data[, Injectable_CGRP_flow_type := ifelse(nr_Injectable_CGRP_d2 < nr_Injectable_CGRP_d1 & nr_Injectable_CGRP_Unq_d1d2 > nr_Injectable_CGRP_d1, "D+S", 
                                                   ifelse(nr_Injectable_CGRP_d2 > nr_Injectable_CGRP_d1 & nr_Injectable_CGRP_Unq_d1d2 > nr_Injectable_CGRP_d2, "A+S",
                                                          ifelse(nr_Injectable_CGRP_d2 < nr_Injectable_CGRP_d1, "D", 
                                                                 ifelse(nr_Injectable_CGRP_d2 > nr_Injectable_CGRP_d1, "A", 
                                                                        ifelse(nr_Injectable_CGRP_d2 == nr_Injectable_CGRP_d1 & Injectable_CGRP_d2 != Injectable_CGRP_d1, "S","-")))))]


# Acute Therapy class (A) - flags
data <- data[, Acute_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x,string_Acute),str_c(unlist(str_extract_all(x,string_Acute)),collapse = ","),"")))]
data <- data[, Acute_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x,string_Acute),str_c(unlist(str_extract_all(x,string_Acute)),collapse = ","),"")))]
data <- data[, nr_Acute_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Acute)*1), str_split(x,","))))]
data <- data[, nr_Acute_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Acute)*1), str_split(x,","))))]
data <- data[, nr_Acute_Unq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Acute_d1","Acute_d2")] 
data <- data[, Acute_flow_type := ifelse(nr_Acute_d2 < nr_Acute_d1 & nr_Acute_Unq_d1d2 > nr_Acute_d1, "D+S", 
                                         ifelse(nr_Acute_d2 > nr_Acute_d1 & nr_Acute_Unq_d1d2 > nr_Acute_d2, "A+S",
                                                ifelse(nr_Acute_d2 < nr_Acute_d1, "D", 
                                                       ifelse(nr_Acute_d2 > nr_Acute_d1, "A", 
                                                              ifelse(nr_Acute_d2 == nr_Acute_d1 & Acute_d2 != Acute_d1, "S","-")))))]





# Preventative Therapy class (p) - flags
data <- data[, Preventative_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x,string_Preventative),str_c(unlist(str_extract_all(x,string_Preventative)),collapse = ","),"")))]
data <- data[, Preventative_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x,string_Preventative),str_c(unlist(str_extract_all(x,string_Preventative)),collapse = ","),"")))]
data <- data[, nr_Preventative_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Preventative)*1), str_split(x,","))))]
data <- data[, nr_Preventative_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Preventative)*1), str_split(x,","))))]
data <- data[, nr_Preventative_Unq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Preventative_d1","Preventative_d2")] 
data <- data[, Preventative_flow_type := ifelse(nr_Preventative_d2 < nr_Preventative_d1 & nr_Preventative_Unq_d1d2 > nr_Preventative_d1, "D+S", 
                                                ifelse(nr_Preventative_d2 > nr_Preventative_d1 & nr_Preventative_Unq_d1d2 > nr_Preventative_d2, "A+S",
                                                       ifelse(nr_Preventative_d2 < nr_Preventative_d1, "D", 
                                                              ifelse(nr_Preventative_d2 > nr_Preventative_d1, "A", 
                                                                     ifelse(nr_Preventative_d2 == nr_Preventative_d1 & Preventative_d2 != Preventative_d1, "S","-")))))]





data %>% select(patient, weight, Preventative_flow_type) %>% distinct() %>% group_by(Preventative_flow_type) %>% summarise(n=sum(weight)/7457204)
data %>% select(patient, weight, Oral_CGRP_flow_type) %>% distinct() %>% group_by(Oral_CGRP_flow_type) %>% summarise(n=sum(weight)/7457204)
data %>% select(patient, weight, Injectable_CGRP_flow_type) %>% distinct() %>% group_by(Injectable_CGRP_flow_type) %>% summarise(n=sum(weight)/7457204)
data %>% select(patient, weight, Acute_flow_type) %>% distinct() %>% group_by(Acute_flow_type) %>% summarise(n=sum(weight)/7457204)


# -----------------
# 2 box model Rimegepant flows drugs tried up until 1st Rime ---------------------

groups <- fread("Source/Groups_Up_to_First_Preventive.txt")
MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- groups %>% select(patid) %>% inner_join(MIGUS24_Doses)

MIGUS24_Doses <- MIGUS24_Doses %>% filter(generic=="Rimegepant" & status != "G") %>% select(-c(provider, provcat, status, code, NPI, generic, drug_id, drug_class, drug_group))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2021-11-16"&from_dt <= "2023-11-16") %>% filter(days_sup!="")


MIGUS24_Doses <- MIGUS24_Doses %>% arrange(patid, from_dt) 

MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
MIGUS24_Doses <- MIGUS24_Doses %>% drop_na()

data.frame(MIGUS24_Doses) 

#MIGUS24_Doses <- MIGUS24_Doses %>% filter(elapsed <= 92)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))


pats_m48 <- MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  filter(from_dt<="2022-11-16") %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>%  ungroup() %>%
  mutate(diff=as.numeric(as.Date("2022-11-16")-from_dt)) %>% 
  filter(diff<=92) %>% select(patid, weight, rate) %>% rename("m48"="rate")


M48_to_m60 <- MIGUS24_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  #filter(from_dt<="2022-06-16") %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>%  ungroup() %>%
  mutate(diff=as.numeric(as.Date("2023-11-16")-from_dt)) %>% 
  filter(diff<=92) %>%
  full_join(pats_m48) %>%  rename("m60"="rate") %>%
  select(patid, weight, m48, m60)


M48_to_m60 <- M48_to_m60 %>% filter(is.na(m48))

unique(M48_to_m60$weight)


Drugs_lookup <- fread("Source/Drugs_lookup.txt")
unique(Drugs_lookup$drug_group)
string_CGRP_Inj <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="CGRP Injectable")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[(Drugs_lookup$drug_group=="CGRP Oral")], collapse = "|"),")\\b")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(M48_to_m60 %>% select(patid))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>%  select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, Month) %>% group_by(patid)
MIGUS24_Drug_Histories_Extended_NoComorbs$patid <- as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$patid)


MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% 
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   


 
# MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% 
#   filter(grepl(string_CGRP_Inj, lead(Treat)) |  grepl(string_CGRP_Oral, lead(Treat))) %>%
#   group_by(patid) %>% filter(Month==min(Month))


MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-c(weight, Month)) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T ) 


M48_to_m60 %>% group_by(m60) %>% summarise(n=sum(weight))

# 1 Acute 17373.
# 2 Prev   7586.

data.frame(M48_to_m60 %>% mutate(patid=as.character(patid)) %>%
  left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%  distinct() %>%
  left_join(Drugs_lookup %>% select(drug_id, drug_group) %>% mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id")) %>%
  select(patid, weight, m60, drug_group) %>% distinct() %>%
  group_by(m60, drug_group) %>% summarise(n=sum(weight)) %>% mutate(n=ifelse(m60=="Acute", n/17373, n/7586)) %>%
  spread(key=m60, value=n))


# -------
    

# Quantify Acute vs preventive volume, different options ---------
MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Demographics)
unique(MIGUS24_Doses$drug_class) 

# MIGUS24_Doses <- MIGUS24_Doses %>% filter(drug_class!="Muscle Relaxant"&drug_class!="SNRI"&
#                                             drug_class!="SSRI"&drug_class!="Antipsychotic"&drug_class!="Tricyclic"&
#                                             drug_class!="Calcium Blocker"&drug_class!="Beta Blocker"&drug_class!="Cardiovascular")

MIGUS24_Doses <- MIGUS24_Doses %>% filter(drug_group!="Preventive" | generic %in% c("Propranolol", "Timolol", "Topiramate", "Botulinum Toxin")|  drug_class=="Muscle Relaxant" | drug_class=="Antiepileptic") 


MIGUS24_Doses <- MIGUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(flag==0) %>% select(-flag)
MIGUS24_Doses <- MIGUS24_Doses %>% select(-c(CV, psychiatric, epileptic))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-11-15"&from_dt<="2023-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, weight, drug_group) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)

MIGUS24_Doses[is.na(MIGUS24_Doses)] <- 0

Types <- MIGUS24_Doses %>%
  mutate(Prev=ifelse(`CGRP Injectable`==1|Preventive==1,1,0)) %>%
    mutate(Acute=ifelse(`CGRP Nasal`==1|`CGRP Oral`==1|Symptomatic==1|Triptans==1,1,0)) %>%
 mutate(Type=ifelse(Prev==1&Acute==1, "Both",
                    ifelse(Prev==1,"Prev", "Acute"))) %>% select(patid, weight, Type)


Types %>% group_by(Type) %>% summarise(n=sum(weight))




# Volume

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Demographics)

# MIGUS24_Doses <- MIGUS24_Doses %>% filter(drug_class!="Muscle Relaxant"&drug_class!="SNRI"&
#                                             drug_class!="SSRI"&drug_class!="Antipsychotic"&drug_class!="Tricyclic"&
#                                             drug_class!="Calcium Blocker"&drug_class!="Beta Blocker"&drug_class!="Cardiovascular")


MIGUS24_Doses <- MIGUS24_Doses %>% filter(drug_group!="Preventive" | generic %in% c("Propranolol", "Timolol", "Topiramate", "Botulinum Toxin")| drug_class=="Muscle Relaxant" | drug_class=="Antiepileptic")

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(flag==0) %>% select(-flag)
MIGUS24_Doses <- MIGUS24_Doses %>% select(-c(CV, psychiatric, epileptic))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-11-15"&from_dt<="2023-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, weight, days_sup, drug_group)
MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, drug_group) %>% summarise(volume=sum(weight*days_sup))
MIGUS24_Doses <- MIGUS24_Doses %>% ungroup() %>% mutate(vol_type=ifelse(drug_group %in% c("Preventive" , "CGRP Injectable"), "Prev", "Acute" )) %>%
  select(patid, volume, vol_type) %>% group_by(patid, vol_type) %>% summarise(volume=sum(volume))

Types %>% inner_join(MIGUS24_Doses) %>% filter(weight!=1) %>% inner_join(Mod_Sev) %>%
  group_by(Type, vol_type) %>% summarise(volume=sum(volume))



# Scripts

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Demographics)

# MIGUS24_Doses <- MIGUS24_Doses %>% filter(drug_class!="Muscle Relaxant"&drug_class!="SNRI"&
#                                             drug_class!="SSRI"&drug_class!="Antipsychotic"&drug_class!="Tricyclic"&
#                                             drug_class!="Calcium Blocker"&drug_class!="Beta Blocker"&drug_class!="Cardiovascular")


MIGUS24_Doses <- MIGUS24_Doses %>% filter(drug_group!="Preventive" | generic %in% c("Propranolol", "Timolol", "Topiramate", "Botulinum Toxin") | drug_class=="Muscle Relaxant" | drug_class=="Antiepileptic")

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(flag==0) %>% select(-flag)
MIGUS24_Doses <- MIGUS24_Doses %>% select(-c(CV, psychiatric, epileptic))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-11-15"&from_dt<="2023-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, weight, days_sup, drug_group)
MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, drug_group) %>% count()
MIGUS24_Doses <- MIGUS24_Doses %>% ungroup() %>% mutate(vol_type=ifelse(drug_group %in% c("Preventive" , "CGRP Injectable"), "Prev", "Acute" )) %>%
  select(patid, n, vol_type) %>% group_by(patid, vol_type) %>% summarise(volume=sum(n))

Types %>% inner_join(MIGUS24_Doses) %>% filter(weight!=1) %>% inner_join(Mod_Sev) %>%
  group_by(Type, vol_type) %>% summarise(volume=sum(volume))

# ------------------
# Waterfall last year vs last month ---------


Mod_Sev <- fread("Source/Mod_Sev.txt")
unique(Mod_Sev$group)
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt") # 21292150
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, weight, CV, psychiatric, epileptic) 
sum(MIGUS24_Demographics$weight)

Mod_Sev %>% inner_join(MIGUS24_Demographics) %>% summarise(n=sum(weight)) # 13061689

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
sum(MIGUS24_Drug_Histories_Extended_NoComorbs$weight) # 21292150

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% inner_join(Mod_Sev)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Month>=60) %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )

Drug_formulary <- fread("Source/Drug_formulary.txt")

Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  select(patid, weight, drug_group) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)

MIGUS24_Drug_Histories_Extended_NoComorbs[is.na(MIGUS24_Drug_Histories_Extended_NoComorbs)] <- 0

MIGUS24_Drug_Histories_Extended_NoComorbs %>% mutate(Prev=ifelse(`CGRP Injectable`==1|Preventive==1, 1, 0)) %>%
  mutate(Acute=ifelse(`CGRP Oral`==1|`CGRP Nasal`==1|Triptans==1|Symptomatic==1, 1, 0)) %>%
  mutate(group=ifelse(Prev==1&Acute==1, "Both",
                       ifelse(Prev==1, "Prev", "Acute"))) %>%
  group_by(group) %>% summarise(n=sum(weight))


# --------
# Excluding preventive drugs for patients with no migraine-specific drug ever ---------

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)
unique(Drug_formulary$drug_group)


MIG_spec <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  filter(drug_group %in% c("CGRP Oral", "CGRP Injectable", "CGRP Nasal", "Triptans")) %>%
  select(patid) %>% distinct() %>% mutate(MIG_spec=1)



MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Demographics)
unique(MIGUS24_Doses$drug_class) 
   
MIGUS24_Doses <- MIGUS24_Doses %>% left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0)


MIGUS24_Doses <- MIGUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(flag==0) %>% select(-flag)
MIGUS24_Doses <- MIGUS24_Doses %>% select(-c(CV, psychiatric, epileptic))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-11-15"&from_dt<="2023-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, weight, drug_group) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)

MIGUS24_Doses[is.na(MIGUS24_Doses)] <- 0

Types <- MIGUS24_Doses %>%
  mutate(Prev=ifelse(`CGRP Injectable`==1|Preventive==1,1,0)) %>%
    mutate(Acute=ifelse(`CGRP Nasal`==1|`CGRP Oral`==1|Symptomatic==1|Triptans==1,1,0)) %>%
 mutate(Type=ifelse(Prev==1&Acute==1, "Both",
                    ifelse(Prev==1,"Prev", "Acute"))) %>% select(patid, weight, Type)


Types %>% group_by(Type) %>% summarise(n=sum(weight))


# Volume

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Demographics)


MIGUS24_Doses <- MIGUS24_Doses %>% left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(flag==0) %>% select(-flag)
MIGUS24_Doses <- MIGUS24_Doses %>% select(-c(CV, psychiatric, epileptic))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-11-15"&from_dt<="2023-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, weight, days_sup, drug_group)
MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, drug_group) %>% summarise(volume=sum(weight*days_sup))
MIGUS24_Doses <- MIGUS24_Doses %>% ungroup() %>% mutate(vol_type=ifelse(drug_group %in% c("Preventive" , "CGRP Injectable"), "Prev", "Acute" )) %>%
  select(patid, volume, vol_type) %>% group_by(patid, vol_type) %>% summarise(volume=sum(volume))

Types %>% inner_join(MIGUS24_Doses) %>% filter(weight!=1) %>% inner_join(Mod_Sev) %>%
  group_by(Type, vol_type) %>% summarise(volume=sum(volume))





# Scripts

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Demographics)


MIGUS24_Doses <- MIGUS24_Doses %>% left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(flag==0) %>% select(-flag)
MIGUS24_Doses <- MIGUS24_Doses %>% select(-c(CV, psychiatric, epileptic))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-11-15"&from_dt<="2023-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, weight, days_sup, drug_group)
MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, drug_group) %>% count()
MIGUS24_Doses <- MIGUS24_Doses %>% ungroup() %>% mutate(vol_type=ifelse(drug_group %in% c("Preventive" , "CGRP Injectable"), "Prev", "Acute" )) %>%
  select(patid, n, vol_type) %>% group_by(patid, vol_type) %>% summarise(volume=sum(n))

Types %>% inner_join(MIGUS24_Doses) %>% filter(weight!=1) %>% inner_join(Mod_Sev) %>%
  group_by(Type, vol_type) %>% summarise(volume=sum(volume))



# -------------
# Acutes and Preventives drug usage Rimegepant patients  ------
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)
unique(Drug_formulary$drug_group)

string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")



MIG_spec <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  filter(drug_group %in% c("CGRP Oral", "CGRP Injectable", "CGRP Nasal", "Triptans")) %>%
  select(patid) %>% distinct() %>% mutate(MIG_spec=1)

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs  %>% select(patid, weight, Month, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% arrange(patid, Month)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% inner_join(Mod_Sev)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) , by=c("Treat"="drug_id")) %>%
  left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0) %>% select(-c(MIG_spec, remove))

MIGUS24_Drug_Histories_Extended_NoComorbs$patid <- as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$patid)

Rimegepant_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat=="136") %>% select(patid) %>% distinct()


# Drugs 

Drugs_to_1st_Rime <- Rimegepant_pats %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

Drugs_to_1st_Rime %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(tot=sum(weight)) # 558548

unique(Drugs_to_1st_Rime$drug_group)


N_molecules <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136") %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

data.frame(Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136") %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>% group_by(n) %>% summarise(tot=sum(weight)))

data.frame(Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136") %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>% summarise(mean=mean(n), sd=sd(n)))



N_molecules_Acute <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136" & drug_group %in% c("CGRP Oral", "Triptans", "Symptomatic")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

data.frame(Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136" & drug_group %in% c("CGRP Oral", "Triptans", "Symptomatic")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>% group_by(n) %>% summarise(tot=sum(weight)))

data.frame(Drugs_to_1st_Rime %>% filter(Treat!="-"& Treat!="136" & drug_group %in% c("CGRP Oral", "Triptans", "Symptomatic")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>% summarise(mean=mean(n), sd=sd(n)))



N_molecules_Prev <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136" & drug_group %in% c("Preventive", "CGRP Injectable")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

data.frame(Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136" & drug_group %in% c("Preventive", "CGRP Injectable")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>% group_by(n) %>% summarise(tot=sum(weight)))

data.frame(Drugs_to_1st_Rime %>% filter(Treat!="-"& Treat!="136" & drug_group %in% c("Preventive", "CGRP Injectable")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>% summarise(mean=mean(n), sd=sd(n)))




# Flows

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% arrange(patid, weight, Month, Treat) %>%
  group_by(patid, weight, Month) %>% mutate(Treat=paste0(Treat, collapse = ",")) %>% distinct() 


Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% select(-drug_group) %>% distinct()

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% group_by(patid) %>% 
  mutate(flow=ifelse(Treat!=lag(Treat),1,0))  %>% mutate(flow=ifelse(is.na(flow),0,flow)) %>%
  mutate(start=ifelse(Treat!="-"&lag(Treat)=="-",1,0))  %>% mutate(start=ifelse(is.na(start),0,start)) %>%
  mutate(stop=ifelse(Treat=="-"&lag(Treat)!="-",1,0))  %>% mutate(stop=ifelse(is.na(stop),0,stop)) 


N_flows <- Drugs_to_1st_Rime %>% filter(flow==1) %>% group_by(patid, weight) %>% count() %>% ungroup() 

data.frame(Drugs_to_1st_Rime %>% filter(flow==1) %>% group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% summarise(tot=sum(weight)))

data.frame(Drugs_to_1st_Rime %>% filter(flow==1) %>% group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% ungroup() %>% summarise(mean=mean(n), sd=sd(n)))

N_starts <- Drugs_to_1st_Rime %>% filter(start==1) %>% group_by(patid, weight) %>% count() %>% ungroup()

data.frame(Drugs_to_1st_Rime %>% filter(start==1) %>% group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% summarise(tot=sum(weight)))

data.frame(Drugs_to_1st_Rime %>% filter(start==1) %>% group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% ungroup() %>% summarise(mean=mean(n), sd=sd(n)))


N_stops <- Drugs_to_1st_Rime %>% filter(stop==1) %>% group_by(patid, weight) %>% count() %>% ungroup() 

data.frame(Drugs_to_1st_Rime %>% filter(stop==1) %>% group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% summarise(tot=sum(weight)))

data.frame(Drugs_to_1st_Rime %>% filter(stop==1) %>% group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% ungroup() %>% summarise(mean=mean(n), sd=sd(n)))


# From CGRP INjectable vs From Preventive

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% select(-c(flow, start, stop))

Inflow_source <- Drugs_to_1st_Rime %>% filter(!grepl("136", Treat)) %>% group_by(patid) %>% filter(Month==max(Month))

Inflow_source <- Inflow_source %>% mutate(source=ifelse(grepl(string_CGRP_Inj, Treat), "Inj",
                                       ifelse(grepl(string_Preventive, Treat), "Prev", "Other")))

Inflow_source %>% group_by(source) %>% summarise(mean=mean(Month))

Inflow_source <- Inflow_source %>% select(patid, source)

Inflow_source %>% inner_join(N_molecules) %>% group_by(source) %>% summarise(n=mean(n))
Inflow_source %>% inner_join(N_molecules_Acute) %>% group_by(source) %>% summarise(n=mean(n))
Inflow_source %>% inner_join(N_molecules_Prev) %>% group_by(source) %>% summarise(n=mean(n))
Inflow_source %>% inner_join(N_flows) %>% group_by(source) %>% summarise(n=mean(n))
Inflow_source %>% inner_join(N_starts) %>% group_by(source) %>% summarise(n=mean(n))
Inflow_source %>% inner_join(N_stops) %>% group_by(source) %>% summarise(n=mean(n))


MIGUS24_Comorbidities_Extended_Dxs <- fread("Source/MIGUS24 Comorbidities Extended Dxs.txt")
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)) %>% 
  select(patid, date) %>% distinct() %>% group_by(patid) %>% count()

Inflow_source %>% inner_join(MIGUS24_Comorbidities_Extended_Dxs %>% mutate(patid=as.character(patid))) %>% group_by(source) %>% summarise(n=mean(n))

# --------
# Rimegepant vs Atogepant --------

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()

Rime_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl("136", Treat)) %>% select(patid) %>% distinct() %>% mutate(group="Rime")
Ato_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(grepl("135", Treat)) %>% select(patid) %>% distinct() %>% mutate(group="Ato")


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)
unique(Drug_formulary$drug_group)

string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")



MIG_spec <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  filter(drug_group %in% c("CGRP Oral", "CGRP Injectable", "CGRP Nasal", "Triptans")) %>%
  select(patid) %>% distinct() %>% mutate(MIG_spec=1)

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs  %>% select(patid, weight, Month, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% arrange(patid, Month)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% inner_join(Mod_Sev)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) , by=c("Treat"="drug_id")) %>%
  left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0) %>% select(-c(MIG_spec, remove))

MIGUS24_Drug_Histories_Extended_NoComorbs$patid <- as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$patid)


# Drugs 

N_molecules <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-") %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()


N_molecules_Acute <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-" & drug_group %in% c("CGRP Oral", "Triptans", "Symptomatic")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

N_molecules_Prev <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="-" & drug_group %in% c("Preventive", "CGRP Injectable")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

# Flows

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% arrange(patid, weight, Month, Treat) %>%
  group_by(patid, weight, Month) %>% mutate(Treat=paste0(Treat, collapse = ",")) %>% distinct() 

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% select(-drug_group) %>% distinct()

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% group_by(patid) %>% 
  mutate(flow=ifelse(Treat!=lag(Treat),1,0))  %>% mutate(flow=ifelse(is.na(flow),0,flow)) %>%
  mutate(start=ifelse(Treat!="-"&lag(Treat)=="-",1,0))  %>% mutate(start=ifelse(is.na(start),0,start)) %>%
  mutate(stop=ifelse(Treat=="-"&lag(Treat)!="-",1,0))  %>% mutate(stop=ifelse(is.na(stop),0,stop)) 


N_flows <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(flow==1) %>% group_by(patid, weight) %>% count() %>% ungroup() 

N_starts <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(start==1) %>% group_by(patid, weight) %>% count() %>% ungroup()

N_stops <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(stop==1) %>% group_by(patid, weight) %>% count() %>% ungroup() 


Rime_pats <- Rime_pats %>% inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid))


Rime_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_molecules) %>% summarise(mean=mean(n))
Ato_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_molecules) %>% summarise(mean=mean(n))

Rime_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_molecules_Acute) %>% summarise(mean=mean(n))
Ato_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_molecules_Acute) %>% summarise(mean=mean(n))

Rime_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_molecules_Prev) %>% summarise(mean=mean(n))
Ato_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_molecules_Prev) %>% summarise(mean=mean(n))

Rime_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_flows) %>% summarise(mean=mean(n))
Ato_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_flows) %>% summarise(mean=mean(n))

Rime_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_starts) %>% summarise(mean=mean(n))
Ato_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_starts) %>% summarise(mean=mean(n))

Rime_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_stops) %>% summarise(mean=mean(n))
Ato_pats %>% mutate(patid=as.character(patid)) %>% inner_join(N_stops) %>% summarise(mean=mean(n))


MIGUS24_Comorbidities_Extended_Dxs <- fread("Source/MIGUS24 Comorbidities Extended Dxs.txt")
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)) %>% 
  select(patid, date) %>% distinct() %>% group_by(patid) %>% count()

Rime_pats %>% mutate(patid=as.character(patid)) %>% inner_join(MIGUS24_Comorbidities_Extended_Dxs %>% mutate(patid=as.character(patid))) %>%  summarise(n=mean(n))
Ato_pats %>% mutate(patid=as.character(patid)) %>% inner_join(MIGUS24_Comorbidities_Extended_Dxs %>% mutate(patid=as.character(patid))) %>% summarise(n=mean(n))


# ---------
# CGRP Injectable/Preventive Interaction preventives Bella --------

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)
unique(Drug_formulary$drug_group)

string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")

string_Acutes_flows <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Symptomatic"|
                                                                     Drug_formulary$drug_group == "Triptans"|
                                                                     Drug_formulary$drug_group == "CGRP Oral"], collapse = "|"),")\\b")
string_Prevs_flows <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"|Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")



MIG_spec <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  filter(drug_group %in% c("CGRP Oral", "CGRP Injectable", "CGRP Nasal", "Triptans")) %>%
  select(patid) %>% distinct() %>% mutate(MIG_spec=1)

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs  %>% select(patid, weight, Month, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% arrange(patid, Month)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% inner_join(Mod_Sev)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) , by=c("Treat"="drug_id")) %>%
  left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0) %>% select(-c(MIG_spec, remove))

MIGUS24_Drug_Histories_Extended_NoComorbs$patid <- as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$patid)

Rimegepant_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat=="136") %>% 
  select(patid, Month, weight) %>% distinct() %>% group_by(patid) %>% filter(Month==min(Month))

sum(Rimegepant_pats$weight) # 558548

Drugs_to_1st_Rime <- Rimegepant_pats %>% select(-Month) %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% filter(!grepl("136", Treat))

Drugs_to_1st_Rime %>% filter(grepl("CGRP", drug_group)) %>% select(patid, weight, Treat) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% summarise(tot=sum(weight))
   



Rimegepant_pats %>% 
  left_join( Drugs_to_1st_Rime %>% filter(grepl("CGRP", drug_group)) %>% select(patid, weight, Treat) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n=n+1) %>%
  ungroup()  %>% # summarise(mean=mean(n))
  ggplot(aes(Month, n)) +
  geom_jitter(size=1, alpha=0.5, colour="deepskyblue4") +
  geom_smooth(fill="firebrick", colour="deepskyblue4") +
  theme_minimal() +
  xlab("\n Month at which \n Rimegepant was Introduceed") +
  ylab("# CGRP Line at which \n Rimegepant was Introduced \n")


# Injectable/Preventive to Rimegepant 

Injectable_pats <- Drugs_to_1st_Rime %>% filter(grepl(string_CGRP_Inj, Treat))  %>% group_by(patid) %>% 
  filter(Month==min(Month)) %>% select(patid, Month, weight)


Preventive_pats <- Drugs_to_1st_Rime %>% filter(grepl(string_Preventive, Treat))  %>% group_by(patid) %>% 
  filter(Month==min(Month)) %>% select(patid, Month, weight)


elapsed <- Injectable_pats %>% rename("First_Inj"="Month") %>% 
  inner_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
  mutate(elapsed=First_Rime-First_Inj) %>% ungroup() # %>% summarise(mean=mean(elapsed))  # 24.9
   

elapsed_prev <- Preventive_pats %>% rename("First_Prev"="Month") %>% 
  inner_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
  mutate(elapsed=First_Rime-First_Prev) %>% ungroup() # %>% summarise(mean=mean(elapsed))  # 36
   

Drugs_to_1st_Rime_flows <- Drugs_to_1st_Rime %>% arrange(patid, weight, Month, Treat) %>%
  group_by(patid, weight, Month) %>% mutate(Treat=paste0(Treat, collapse = ",")) %>% distinct() 

Drugs_to_1st_Rime_flows <- Drugs_to_1st_Rime_flows %>% select(-drug_group) %>% distinct()

Drugs_to_1st_Rime_flows <- Drugs_to_1st_Rime_flows %>% group_by(patid) %>% 
  mutate(flow=ifelse(Treat!=lag(Treat),1,0))  %>% mutate(flow=ifelse(is.na(flow),0,flow)) 


flows_between <- Drugs_to_1st_Rime_flows %>%
    inner_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
    inner_join(Injectable_pats %>% rename("First_Inj"="Month")) %>%
    filter(Month>First_Inj & Month<First_Rime) %>% distinct() %>%
  filter(flow==1) %>% group_by(patid, weight) %>% count()


flows_between_prev <- Drugs_to_1st_Rime_flows %>%
    inner_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
    inner_join(Preventive_pats %>% rename("First_Prev"="Month")) %>%
    filter(Month>First_Prev & Month<First_Rime) %>% distinct() %>%
  filter(flow==1) %>% group_by(patid, weight) %>% count()

mean(flows_between$n)
mean(flows_between_prev$n)

data.frame(elapsed %>% select(patid, weight, elapsed) %>%
  left_join(flows_between) %>% mutate(n=ifelse(is.na(n),0,n)) %>% # summarise(mean=mean(n))
  group_by(n) %>% summarise(tot=sum(weight)))


data.frame(elapsed_prev %>% select(patid, weight, elapsed) %>%
  left_join(flows_between_prev) %>% mutate(n=ifelse(is.na(n),0,n)) %>% # summarise(mean=mean(n))
  group_by(n) %>% summarise(tot=sum(weight)))

elapsed %>% select(patid, weight, elapsed) %>%
  left_join(flows_between) %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
  ggplot(aes(elapsed, n)) +
  geom_jitter(size=1, alpha=0.5, colour="deepskyblue4") +
   geom_smooth(fill="firebrick", colour="deepskyblue4") +
  theme_minimal() +
  xlab("\n # Elpased Months From \n 1st CGRP Injectable & 1st Rimegepant") +
  ylab("# Flows \n 1st CGRP Injectable & 1st Rimegepant \n")


elapsed_prev %>% select(patid, weight, elapsed) %>%
  left_join(flows_between_prev) %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
  ggplot(aes(elapsed, n)) +
  geom_jitter(size=1, alpha=0.5, colour="deepskyblue4") +
   geom_smooth(fill="firebrick", colour="deepskyblue4") +
  theme_minimal() +
  xlab("\n # Elpased Months From \n 1st Preventive & 1st Rimegepant") +
  ylab("# Flows \n 1st Preventive & 1st Rimegepant \n")



flows <- Drugs_to_1st_Rime_flows

flows <- separate_rows(flows, Treat, sep = ",", convert=T )

pat_months <- flows %>% select(patid, weight, Month) %>% distinct()

flows_acute <- pat_months %>% left_join(flows %>% filter(grepl(string_Acutes_flows, Treat)|Treat=="-")) %>% 
  mutate(Treat=ifelse(is.na(Treat),"-", Treat)) %>% select(-flow)

flows_acute <- flows_acute %>% arrange(patid, weight, Month, Treat) %>%
  group_by(patid, weight, Month) %>% mutate(Treat=paste0(Treat, collapse = ",")) %>% distinct() 

flows_acute <- flows_acute %>% group_by(patid) %>% 
  mutate(flow=ifelse(Treat!=lag(Treat),1,0))  %>% mutate(flow=ifelse(is.na(flow),0,flow)) 

flows_prev <- pat_months %>% left_join(flows %>% filter(grepl(string_Prevs_flows, Treat)|Treat=="-")) %>% 
  mutate(Treat=ifelse(is.na(Treat),"-", Treat)) %>% select(-flow)

flows_prev <- flows_prev %>% arrange(patid, weight, Month, Treat) %>%
  group_by(patid, weight, Month) %>% mutate(Treat=paste0(Treat, collapse = ",")) %>% distinct() 

flows_prev <- flows_prev %>% group_by(patid) %>% 
  mutate(flow=ifelse(Treat!=lag(Treat),1,0))  %>% mutate(flow=ifelse(is.na(flow),0,flow)) 



flows_between_inj <- flows_acute %>%
    inner_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
    inner_join(Injectable_pats %>% rename("First_Inj"="Month")) %>%
    filter(Month>First_Inj & Month<First_Rime) %>% distinct() %>%
  filter(flow==1) %>% group_by(patid, weight) %>% count()

mean(flows_between_inj$n)

flows_between_inj <- flows_prev %>%
    inner_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
    inner_join(Injectable_pats %>% rename("First_Inj"="Month")) %>%
    filter(Month>First_Inj & Month<First_Rime) %>% distinct() %>%
  filter(flow==1) %>% group_by(patid, weight) %>% count()

mean(flows_between_inj$n)



flows_between_prev <- flows_acute %>%
    inner_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
    inner_join(Preventive_pats %>% rename("First_Prev"="Month")) %>%
    filter(Month>First_Prev & Month<First_Rime) %>% distinct() %>%
  filter(flow==1) %>% group_by(patid, weight) %>% count()

mean(flows_between_prev$n)

flows_between_inj <- flows_prev %>%
    inner_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
    inner_join(Preventive_pats %>% rename("First_Prev"="Month")) %>%
    filter(Month>First_Prev & Month<First_Rime) %>% distinct() %>%
  filter(flow==1) %>% group_by(patid, weight) %>% count()

mean(flows_between_inj$n)






# Max Rank Up to first Riemgepant year over year

exp_df <- Drugs_to_1st_Rime %>% filter(Treat!="-") %>% 
  left_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
  select(-c(Treat,Month)) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=drug_group, value=exp)

exp_df[is.na(exp_df)] <- 0

exp_df <- exp_df %>%
  mutate(First_Rime=ifelse(First_Rime>=49, 5,
                           ifelse(First_Rime>=37,4,
                                  ifelse(First_Rime>=25,3,
                                         ifelse(First_Rime>=13,2,1)))))

exp_df %>% group_by(First_Rime, Preventive) %>% summarise(n=sum(weight)) %>% 
  spread(key=Preventive, value=n) %>% mutate(perc=`1`/(`1`+`0`))

exp_df %>% group_by(First_Rime, `CGRP Injectable`) %>% summarise(n=sum(weight)) %>% 
  spread(key=`CGRP Injectable`, value=n) %>% mutate(perc=`1`/(`1`+`0`))

exp_df %>% group_by(First_Rime, `CGRP Oral`) %>% summarise(n=sum(weight)) %>% 
  spread(key=`CGRP Oral`, value=n) %>% mutate(perc=`1`/(`1`+`0`))

exp_df %>% group_by(First_Rime, Triptans) %>% summarise(n=sum(weight)) %>% 
  spread(key=Triptans, value=n) %>% mutate(perc=`1`/(`1`+`0`))

exp_df %>% group_by(First_Rime, Symptomatic) %>% summarise(n=sum(weight)) %>% 
  spread(key=Symptomatic, value=n) %>% mutate(perc=`1`/(`1`+`0`))


exp_df %>% mutate(group=ifelse(`CGRP Oral`==1,"Oral",
                               ifelse(`CGRP Injectable`==1, "Inj",
                                      ifelse(Preventive==1, "Prev",
                                             ifelse(Triptans==1,"Triptan", 
                                                    ifelse(Symptomatic==1, "Sympt", NA)))))) %>%
  group_by(First_Rime, group) %>% summarise(n=sum(weight)) %>%
  spread(key=First_Rime, value=n)


exp_df <- exp_df %>% mutate(group=ifelse(`CGRP Oral`==1,"Oral",
                               ifelse(`CGRP Injectable`==1, "Inj",
                                      ifelse(Preventive==1, "Prev",
                                             ifelse(Triptans==1,"Triptan", 
                                                    ifelse(Symptomatic==1, "Sympt", NA)))))) 






# KPIS based on max rank cumulative up to first rimegepant

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)
unique(Drug_formulary$drug_group)

string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")



MIG_spec <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  filter(drug_group %in% c("CGRP Oral", "CGRP Injectable", "CGRP Nasal", "Triptans")) %>%
  select(patid) %>% distinct() %>% mutate(MIG_spec=1)

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs  %>% select(patid, weight, Month, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% arrange(patid, Month)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% inner_join(Mod_Sev)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) , by=c("Treat"="drug_id")) %>%
  left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0) %>% select(-c(MIG_spec, remove))

MIGUS24_Drug_Histories_Extended_NoComorbs$patid <- as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$patid)

Rimegepant_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat=="136") %>% select(patid) %>% distinct()


# Drugs 

Drugs_to_1st_Rime <- Rimegepant_pats %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

Drugs_to_1st_Rime %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(tot=sum(weight)) # 558548

unique(Drugs_to_1st_Rime$drug_group)


N_molecules <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136") %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

N_molecules_Acute <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136" & drug_group %in% c("CGRP Oral", "Triptans", "Symptomatic")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

N_molecules_Prev <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136" & drug_group %in% c("Preventive", "CGRP Injectable")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()


# Flows

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% arrange(patid, weight, Month, Treat) %>%
  group_by(patid, weight, Month) %>% mutate(Treat=paste0(Treat, collapse = ",")) %>% distinct() 

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% select(-drug_group) %>% distinct()

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% group_by(patid) %>% 
  mutate(flow=ifelse(Treat!=lag(Treat),1,0))  %>% mutate(flow=ifelse(is.na(flow),0,flow)) %>%
  mutate(start=ifelse(Treat!="-"&lag(Treat)=="-",1,0))  %>% mutate(start=ifelse(is.na(start),0,start)) %>%
  mutate(stop=ifelse(Treat=="-"&lag(Treat)!="-",1,0))  %>% mutate(stop=ifelse(is.na(stop),0,stop)) 

N_flows <- Drugs_to_1st_Rime %>% filter(flow==1) %>% group_by(patid, weight) %>% count() %>% ungroup() 

N_starts <- Drugs_to_1st_Rime %>% filter(start==1) %>% group_by(patid, weight) %>% count() %>% ungroup()

N_stops <- Drugs_to_1st_Rime %>% filter(stop==1) %>% group_by(patid, weight) %>% count() %>% ungroup() 


exp_df %>% filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_molecules) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>% filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_molecules_Acute) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>% filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_molecules_Prev) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>% filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_flows) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>% filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_starts) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>%  filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_stops) %>% group_by(group) %>% summarise(n=mean(n))


MIGUS24_Comorbidities_Extended_Dxs <- fread("Source/MIGUS24 Comorbidities Extended Dxs.txt")
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)) %>% 
  select(patid, date) %>% distinct() %>% group_by(patid) %>% count()

exp_df %>% filter(First_Rime==5) %>% select(patid, group) %>% inner_join(MIGUS24_Comorbidities_Extended_Dxs %>% mutate(patid=as.character(patid))) %>% group_by(group) %>% summarise(n=mean(n))


# Switch Mab to Rime vs Add ON





# ---------

# Riemgepant patients and physician Acute vs Start  ------- 

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
RIME_pats <- MIGUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid) %>% distinct()
MIGUS24_Doses <- RIME_pats %>% left_join(MIGUS24_Doses) %>% filter(generic=="Rimegepant")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
range(MIGUS24_Doses$from_dt)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% arrange(patid, generic, from_dt) 
MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
MIGUS24_Doses <- MIGUS24_Doses %>% drop_na()
MIGUS24_Doses <- MIGUS24_Doses %>% filter(elapsed <= 92)
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

means_groups <- MIGUS24_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>% ungroup() %>% select(-generic)

means_groups %>% group_by(mean) %>% summarise(n=sum(weight))

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid, provider, provcat) 

PROVCAT <- read_excel("Source/CDM_Data_Reference_Lookup_Tables_V81.xlsx", sheet="PROVCAT", skip = 5)
PROVCAT <- PROVCAT %>% select(PROVCAT, DESCRIPTION)  %>% mutate(PROVCAT=as.numeric(PROVCAT))
MIGUS24_Doses <- MIGUS24_Doses %>%  inner_join(PROVCAT %>% select(PROVCAT, DESCRIPTION) %>% distinct(), by=c("provcat"="PROVCAT"))

specialties_drug_doses_alloc <- fread("Source/specialties_drug_doses_alloc.csv")

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(specialties_drug_doses_alloc, by=c("DESCRIPTION"="original")) %>%
  filter(final %in% c("GP", "IM", "NEURO", "OTHER PHYSICIAN") ) %>% select(patid, provider)
  
MIGUS24_Doses <- MIGUS24_Doses %>% distinct()


MIGUS24_Doses %>% inner_join(means_groups) %>% 
  select(provider, mean) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=mean, value=exp) %>% 
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Prev=ifelse(is.na(Prev),0,Prev)) %>%
  mutate(group=ifelse(Acute==1&Prev==1, "Both",
                      ifelse(Prev==1,"Prev", "Acute"))) %>% group_by(group) %>% count()



physician_groups <-  MIGUS24_Doses %>% inner_join(means_groups) %>% select(provider, mean) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=mean, value=exp) %>% 
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Prev=ifelse(is.na(Prev),0,Prev)) %>%
  mutate(group=ifelse(Acute==1&Prev==1, "Both",
                      ifelse(Prev==1,"Prev", "Acute"))) %>% group_by(group)



MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid, from_dt, provider, provcat)  %>% mutate(from_dt=as.Date(from_dt)) %>% distinct()

PROVCAT <- read_excel("Source/CDM_Data_Reference_Lookup_Tables_V81.xlsx", sheet="PROVCAT", skip = 5)
PROVCAT <- PROVCAT %>% select(PROVCAT, DESCRIPTION)  %>% mutate(PROVCAT=as.numeric(PROVCAT))
MIGUS24_Doses <- MIGUS24_Doses %>%  inner_join(PROVCAT %>% select(PROVCAT, DESCRIPTION) %>% distinct(), by=c("provcat"="PROVCAT"))

specialties_drug_doses_alloc <- fread("Source/specialties_drug_doses_alloc.csv")

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(specialties_drug_doses_alloc, by=c("DESCRIPTION"="original")) %>%
  filter(final %in% c("GP", "IM", "NEURO", "OTHER PHYSICIAN") ) %>% select(patid, provider, from_dt) %>% distinct()
  

MIGUS24_Doses %>% inner_join(means_groups) %>% select(provider, mean, from_dt) %>% distinct() %>%
  group_by(provider) %>% filter(from_dt==min(from_dt)) %>% ungroup() %>% select(-from_dt) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=mean, value=exp) %>% 
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Prev=ifelse(is.na(Prev),0,Prev)) %>%
  mutate(group_start=ifelse(Acute==1&Prev==1, "Both",
                      ifelse(Prev==1,"Prev", "Acute"))) %>% group_by(group_start) %>% count()


physician_groups_start <- MIGUS24_Doses %>% inner_join(means_groups) %>% select(provider, mean, from_dt) %>% distinct() %>%
  group_by(provider) %>% filter(from_dt==min(from_dt)) %>% ungroup() %>% select(-from_dt) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=mean, value=exp) %>% 
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Prev=ifelse(is.na(Prev),0,Prev)) %>%
  mutate(group_start=ifelse(Acute==1&Prev==1, "Both",
                      ifelse(Prev==1,"Prev", "Acute")))

physician_groups %>% select(provider, group) %>%
  inner_join(physician_groups_start %>% select(provider, group_start)) %>%
  group_by(group, group_start) %>% count()





MIGUS24_Flows_Long <- fread("Source/MIGUS24 Flows_Long.txt")

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% # inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid), by=c("patient"="patid")) %>%
  filter(grepl("136", d2)&!grepl("136", d1) & p1>=48)

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% select(patient, weight, p1, d1)

sum(MIGUS24_Flows_Long$weight) #  56465.32
 
MIGUS24_Flows_Long %>% filter(grepl("135",d1)) %>% summarise(n=sum(weight))

MIGUS24_Flows_Long <- separate_rows(MIGUS24_Flows_Long, d1, sep = ",", convert=T )

Drug_formulary <- fread("Source/Drug_formulary.txt")
unique(Drug_formulary$drug_group)

string_Symptomatic <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")
string_CGRP_Injectable <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Oral"], collapse = "|"),")\\b")


MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)), by=c("d1"="drug_id"))

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 
names(MIGUS24_Demographics)[1] <- "patient"

MIGUS24_Flows_Long <- MIGUS24_Demographics %>% inner_join(MIGUS24_Flows_Long)

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Flows_Long <- MIGUS24_Flows_Long %>% filter(flag==0) %>% select(-flag)

MIGUS24_Flows_Long %>% select(patient, weight, p1,d1) %>%
  arrange(patient, weight, p1, d1) %>% group_by(patient, weight, p1) %>%
  mutate(d1=paste0(d1, collapse = ",")) %>% distinct() %>%
  mutate(box=ifelse(grepl(string_CGRP_Oral,d1), "O",
                          ifelse(grepl(string_CGRP_Injectable,d1), "I",
                                       ifelse(grepl(string_Prev,d1)&grepl(string_Triptan,d1), "K",
                                              ifelse(grepl(string_Prev,d1) & grepl(string_Symptomatic,d1), "Y",
                                                     ifelse(grepl(string_Prev, d1), "P",
                                                            ifelse(grepl(string_Triptan,d1), "T",
                                                                   ifelse(grepl(string_Symptomatic,d1), "S", "X")))))))) %>%
  ungroup() %>% group_by(box) %>% summarise(n=sum(weight)) %>% mutate(n=ifelse(box=="X", n+5771.32,n))


# ----------
# Max Rank up to 1st Riemgepant - preventive only rime pats ----

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
RIME_pats <- MIGUS24_Doses %>% filter(generic=="Rimegepant") %>% select(patid) %>% distinct()
MIGUS24_Doses <- RIME_pats %>% left_join(MIGUS24_Doses) %>% filter(generic=="Rimegepant")
MIGUS24_Doses <- MIGUS24_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
range(MIGUS24_Doses$from_dt)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-11-16") %>% filter(days_sup  != "")
MIGUS24_Doses <- MIGUS24_Doses %>% arrange(patid, generic, from_dt) 
MIGUS24_Doses <- MIGUS24_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
MIGUS24_Doses <- MIGUS24_Doses %>% drop_na()
MIGUS24_Doses <- MIGUS24_Doses %>% filter(elapsed <= 92)
MIGUS24_Doses <- MIGUS24_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

means_groups <- MIGUS24_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>% ungroup() %>% select(-generic)


MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)
unique(Drug_formulary$drug_group)

string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")

string_Acutes_flows <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Symptomatic"|
                                                                     Drug_formulary$drug_group == "Triptans"|
                                                                     Drug_formulary$drug_group == "CGRP Oral"], collapse = "|"),")\\b")
string_Prevs_flows <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"|Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")



MIG_spec <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  filter(drug_group %in% c("CGRP Oral", "CGRP Injectable", "CGRP Nasal", "Triptans")) %>%
  select(patid) %>% distinct() %>% mutate(MIG_spec=1)

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs  %>% select(patid, weight, Month, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% arrange(patid, Month)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% inner_join(Mod_Sev)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) , by=c("Treat"="drug_id")) %>%
  left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0) %>% select(-c(MIG_spec, remove))

MIGUS24_Drug_Histories_Extended_NoComorbs$patid <- as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$patid)

Rimegepant_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat=="136") %>% 
  select(patid, Month, weight) %>% distinct() %>% group_by(patid) %>% filter(Month==min(Month))

sum(Rimegepant_pats$weight) # 558548

Drugs_to_1st_Rime <- Rimegepant_pats %>% select(-Month) %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% filter(!grepl("136", Treat))

Drugs_to_1st_Rime %>% filter(grepl("CGRP", drug_group)) %>% select(patid, weight, Treat) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% summarise(tot=sum(weight))
   


# Max Rank Up to first Riemgepant year over year

exp_df <- Drugs_to_1st_Rime %>% filter(Treat!="-") %>% 
  left_join(Rimegepant_pats %>% rename("First_Rime"="Month")) %>%
  select(-c(Treat,Month)) %>% distinct() %>% mutate(exp=1) %>%
  spread(key=drug_group, value=exp)

exp_df[is.na(exp_df)] <- 0

exp_df <- exp_df %>%
  mutate(First_Rime=ifelse(First_Rime>=49, 5,
                           ifelse(First_Rime>=37,4,
                                  ifelse(First_Rime>=25,3,
                                         ifelse(First_Rime>=13,2,1)))))

exp_df %>% group_by(First_Rime, Preventive) %>% summarise(n=sum(weight)) %>% 
  spread(key=Preventive, value=n) %>% mutate(perc=`1`/(`1`+`0`))

exp_df %>% group_by(First_Rime, `CGRP Injectable`) %>% summarise(n=sum(weight)) %>% 
  spread(key=`CGRP Injectable`, value=n) %>% mutate(perc=`1`/(`1`+`0`))

exp_df %>% group_by(First_Rime, `CGRP Oral`) %>% summarise(n=sum(weight)) %>% 
  spread(key=`CGRP Oral`, value=n) %>% mutate(perc=`1`/(`1`+`0`))

exp_df %>% group_by(First_Rime, Triptans) %>% summarise(n=sum(weight)) %>% 
  spread(key=Triptans, value=n) %>% mutate(perc=`1`/(`1`+`0`))

exp_df %>% group_by(First_Rime, Symptomatic) %>% summarise(n=sum(weight)) %>% 
  spread(key=Symptomatic, value=n) %>% mutate(perc=`1`/(`1`+`0`))


exp_df %>% mutate(group=ifelse(`CGRP Oral`==1,"Oral",
                               ifelse(`CGRP Injectable`==1, "Inj",
                                      ifelse(Preventive==1, "Prev",
                                             ifelse(Triptans==1,"Triptan", 
                                                    ifelse(Symptomatic==1, "Sympt", NA)))))) %>%
  inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid) %>% mutate(patid=as.character(patid))) %>%
  group_by(First_Rime, group) %>% summarise(n=sum(weight)) %>%
  spread(key=First_Rime, value=n)


exp_df <- exp_df %>% mutate(group=ifelse(`CGRP Oral`==1,"Oral",
                               ifelse(`CGRP Injectable`==1, "Inj",
                                      ifelse(Preventive==1, "Prev",
                                             ifelse(Triptans==1,"Triptan", 
                                                    ifelse(Symptomatic==1, "Sympt", NA)))))) 




# KPIS based on max rank cumulative up to first rimegepant

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)
unique(Drug_formulary$drug_group)

string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")



MIG_spec <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  filter(drug_group %in% c("CGRP Oral", "CGRP Injectable", "CGRP Nasal", "Triptans")) %>%
  select(patid) %>% distinct() %>% mutate(MIG_spec=1)

MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV") %>% select(-version)
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs  %>% select(patid, weight, Month, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% arrange(patid, Month)

Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct() %>% inner_join(Mod_Sev)

MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary %>% mutate(drug_id=as.character(drug_id)) , by=c("Treat"="drug_id")) %>%
  left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0) %>% select(-c(MIG_spec, remove))

MIGUS24_Drug_Histories_Extended_NoComorbs$patid <- as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$patid)

Rimegepant_pats <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat=="136") %>% select(patid) %>% distinct()


# Drugs 

Drugs_to_1st_Rime <- Rimegepant_pats %>% left_join(MIGUS24_Drug_Histories_Extended_NoComorbs) %>%
  arrange(patid, Month) %>% group_by(patid) %>%
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

Drugs_to_1st_Rime %>% ungroup() %>% select(patid, weight) %>% distinct() %>% summarise(tot=sum(weight)) # 558548

unique(Drugs_to_1st_Rime$drug_group)


N_molecules <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136") %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

N_molecules_Acute <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136" & drug_group %in% c("CGRP Oral", "Triptans", "Symptomatic")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()

N_molecules_Prev <- Drugs_to_1st_Rime %>% filter(Treat!="-" & Treat!="136" & drug_group %in% c("Preventive", "CGRP Injectable")) %>% select(patid, weight, Treat, drug_group) %>% distinct() %>%
  group_by(patid, weight) %>% count() %>% ungroup()


# Flows

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% arrange(patid, weight, Month, Treat) %>%
  group_by(patid, weight, Month) %>% mutate(Treat=paste0(Treat, collapse = ",")) %>% distinct() 

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% select(-drug_group) %>% distinct()

Drugs_to_1st_Rime <- Drugs_to_1st_Rime %>% group_by(patid) %>% 
  mutate(flow=ifelse(Treat!=lag(Treat),1,0))  %>% mutate(flow=ifelse(is.na(flow),0,flow)) %>%
  mutate(start=ifelse(Treat!="-"&lag(Treat)=="-",1,0))  %>% mutate(start=ifelse(is.na(start),0,start)) %>%
  mutate(stop=ifelse(Treat=="-"&lag(Treat)!="-",1,0))  %>% mutate(stop=ifelse(is.na(stop),0,stop)) 

N_flows <- Drugs_to_1st_Rime %>% filter(flow==1) %>% group_by(patid, weight) %>% count() %>% ungroup() 

N_starts <- Drugs_to_1st_Rime %>% filter(start==1) %>% group_by(patid, weight) %>% count() %>% ungroup()

N_stops <- Drugs_to_1st_Rime %>% filter(stop==1) %>% group_by(patid, weight) %>% count() %>% ungroup() 


exp_df %>%  inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid) %>% mutate(patid=as.character(patid))) %>%
  filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_molecules) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>% inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid) %>% mutate(patid=as.character(patid))) %>%
  filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_molecules_Acute) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>% inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid) %>% mutate(patid=as.character(patid))) %>%
  filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_molecules_Prev) %>% group_by(group) %>% summarise(n=mean(n))

exp_df %>% inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid) %>% mutate(patid=as.character(patid))) %>%
  filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_flows) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>% inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid) %>% mutate(patid=as.character(patid))) %>%
  filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_starts) %>% group_by(group) %>% summarise(n=mean(n))
exp_df %>% inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid) %>% mutate(patid=as.character(patid))) %>%
  filter(First_Rime==5) %>% select(patid, group) %>% inner_join(N_stops) %>% group_by(group) %>% summarise(n=mean(n))


MIGUS24_Comorbidities_Extended_Dxs <- fread("Source/MIGUS24 Comorbidities Extended Dxs.txt")
MIGUS24_Comorbidities_Extended_Dxs <- MIGUS24_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)) %>% 
  select(patid, date) %>% distinct() %>% group_by(patid) %>% count()

exp_df %>% inner_join(means_groups %>% filter(mean=="Prev") %>% select(patid) %>% mutate(patid=as.character(patid))) %>% 
  filter(First_Rime==5) %>% select(patid, group) %>% inner_join(MIGUS24_Comorbidities_Extended_Dxs %>% mutate(patid=as.character(patid))) %>% group_by(group) %>% summarise(n=mean(n))

# ---------
# Supply days over time preventives ------------
MIGUS24_Drug_Histories_Extended_NoComorbs <- fread("Source/MIGUS24_Drug_Histories_Extended_NoComorbs.txt")
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(version=="NEW_ZAV")
MIGUS24_Drug_Histories_Extended_NoComorbs <- gather(MIGUS24_Drug_Histories_Extended_NoComorbs, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories_Extended_NoComorbs$Month <- parse_number(as.character(MIGUS24_Drug_Histories_Extended_NoComorbs$Month))
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% filter(Treat!="*"&Treat!="-") %>% select(patid, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories_Extended_NoComorbs <- separate_rows(MIGUS24_Drug_Histories_Extended_NoComorbs, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories_Extended_NoComorbs <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% distinct()

Drug_formulary <- fread("Source/Drug_formulary.txt")
Drug_formulary <- Drug_formulary %>% select(drug_id, drug_group)
unique(Drug_formulary$drug_group)

string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"], collapse = "|"),")\\b")

string_Acutes_flows <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Symptomatic"|
                                                                     Drug_formulary$drug_group == "Triptans"|
                                                                     Drug_formulary$drug_group == "CGRP Oral"], collapse = "|"),")\\b")
string_Prevs_flows <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group == "Preventive"|Drug_formulary$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")



MIG_spec <- MIGUS24_Drug_Histories_Extended_NoComorbs %>% left_join(Drug_formulary, by=c("Treat"="drug_id")) %>%
  filter(drug_group %in% c("CGRP Oral", "CGRP Injectable", "CGRP Nasal", "Triptans")) %>%
  select(patid) %>% distinct() %>% mutate(MIG_spec=1)




MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 

MIGUS24_Doses <- fread("Source/MIGUS24 Doses.txt")
MIGUS24_Doses <- MIGUS24_Doses %>% inner_join(MIGUS24_Demographics)

MIGUS24_Doses <- MIGUS24_Doses %>% left_join(MIG_spec) %>% mutate(MIG_spec=ifelse(is.na(MIG_spec), 0, MIG_spec)) %>%
  mutate(remove=ifelse(MIG_spec==0 & drug_group=="Preventive", 1,0)) %>% filter(remove==0)

MIGUS24_Doses <- MIGUS24_Doses %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Doses <- MIGUS24_Doses %>% filter(flag==0) %>% select(-flag)
MIGUS24_Doses <- MIGUS24_Doses %>% select(-c(CV, psychiatric, epileptic))

unique(MIGUS24_Doses$drug_group)
MIGUS24_Doses <- MIGUS24_Doses %>% select(patid, weight, generic, drug_class, drug_group, from_dt, days_sup) 
MIGUS24_Doses <- MIGUS24_Doses %>% filter(drug_group=="Preventive"|drug_group=="CGRP Oral")

MIGUS24_Doses$from_dt <- str_sub(as.character(MIGUS24_Doses$from_dt), 1L, 7L)

data.frame(MIGUS24_Doses %>% group_by(from_dt, drug_class) %>% summarise(n=sum(weight*days_sup)) %>%
  spread(key=drug_class, value=n))

data.frame(MIGUS24_Doses %>% filter(drug_class=="CGRP Oral") %>% group_by(from_dt, generic) %>% summarise(n=sum(weight*days_sup)) %>%
  spread(key=generic, value=n))

# ----------
# Compare MIG Population Distribution based on different criteria -------

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>% select(patid, weight, migraine_onset, migraine_last_dx)
sum(MIGUS24_Demographics$weight) # 22418159
Plus2Dxs <- MIGUS24_Demographics %>% filter(migraine_onset!=migraine_last_dx) %>% select(patid) %>% rename("patient"="patid")

MIGUS24_Flows_Long <- fread("Source/MIGUS24 Flows_Long.txt")
MIGUS24_Flows_Long %>% filter(p2==60) %>% group_by(s2) %>% summarise(n=sum(weight)) 
MIGUS24_Flows_Long %>% inner_join(Plus2Dxs) %>% filter(p2==60) %>% group_by(s2) %>% summarise(n=sum(weight)) 
MIGUS24_Flows_Long %>% filter(p1>=48) %>% filter(grepl("136",d2) & !grepl("136",d1)) %>%
  group_by(s1) %>% summarise(n=sum(weight)) 
MIGUS24_Flows_Long %>% inner_join(Plus2Dxs) %>% filter(p1>=48) %>% filter(grepl("136",d2) & !grepl("136",d1)) %>%
  group_by(s1) %>% summarise(n=sum(weight)) 


MIGUS24_Drug_Histories <- fread("Source/MIGUS24 Drug Histories.txt")
MIGUS24_Drug_Histories <- gather(MIGUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(patient, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(Treat!="-")
Drug_formulary <- fread("Source/Drug_formulary.txt")
unique(Drug_formulary$drug_class)
string_muscle_rel <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Muscle Relaxant"], collapse = "|"),")\\b")
string_antiepilep <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Antiepileptic"&Drug_formulary$generic_name != "Topiramate"], collapse = "|"),")\\b")
muscle_rela_pats <- MIGUS24_Drug_Histories %>% filter(grepl(string_muscle_rel, Treat)) %>% select(patient) %>% distinct() 
antiepilep_pats <- MIGUS24_Drug_Histories %>% filter(grepl(string_antiepilep, Treat)) %>% select(patient) %>% distinct() 


MIGUS24_pts_AllDxs <- fread("Source/MIGUS24_pts_AllDxs.txt")
MIGUS24_pts_AllDxs$ICD10_diag <- str_sub(MIGUS24_pts_AllDxs$ICD10_diag, 1L, 2L)
MIGUS24_pts_AllDxs <- MIGUS24_pts_AllDxs %>% select(patid, ICD10_diag) %>% distinct() %>%
  filter(grepl("^[A-Za-z]", ICD10_diag)) %>%  filter(nchar(ICD10_diag) == 2) %>% rename("patient"="patid")
  

all_pats <- MIGUS24_Demographics %>% select(patid, weight) %>% rename("patient"="patid") %>%
  left_join(muscle_rela_pats %>% mutate(muscle_rel=1)) %>%
  left_join(antiepilep_pats %>% mutate(antiepilep=1)) 

all_pats[is.na(all_pats)] <- 0


MIGUS24_pts_AllDxs <- MIGUS24_pts_AllDxs %>% mutate(exp=1) %>% spread(key=ICD10_diag, value=exp)
MIGUS24_pts_AllDxs[is.na(MIGUS24_pts_AllDxs)] <- 0
dim(MIGUS24_pts_AllDxs)

  
MIGUS24_pts_AllDxs <- all_pats %>% left_join(MIGUS24_pts_AllDxs)
MIGUS24_pts_AllDxs[is.na(MIGUS24_pts_AllDxs)] <- 0
dim(MIGUS24_pts_AllDxs)
  








library(xgboost)
library(caret)



shap.score.rank <- function(xgb_model = xgb_mod, shap_approx = TRUE, 
                            X_train = mydata$train_mm){
  require(xgboost)
  require(data.table)
  shap_contrib <- predict(xgb_model, X_train,
                          predcontrib = TRUE, approxcontrib = shap_approx)
  shap_contrib <- as.data.table(shap_contrib)
  shap_contrib[,BIAS:=NULL]
  cat('make SHAP score by decreasing order\n\n')
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = (mean_shap_score)))
}

# a function to standardize feature values into same range
std1 <- function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}


# prep shap data
shap.prep <- function(shap  = shap_result, X_train = mydata$train_mm, top_n){
  require(ggforce)
  # descending order
  if (missing(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  if (!top_n%in%c(1:dim(X_train)[2])) stop('supply correct top_n')
  require(data.table)
  shap_score_sub <- as.data.table(shap$shap_score)
  shap_score_sub <- shap_score_sub[, names(shap$mean_shap_score)[1:top_n], with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  
  # feature values: the values in the original dataset
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, stdfvalue := std1(value), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue; 
  # standarized: stdfvalue
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
  shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2) 
}

plot.shap.summary <- function(data_long){
  x_bound <- max(abs(data_long$value))
  require('ggforce') # for `geom_sina`
  plot1 <- ggplot(data = data_long)+
    coord_flip() + 
    # sina plot: 
    geom_sina(aes(x = variable, y = value, color = stdfvalue)) +
    # print the mean absolute value: 
    geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
              aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
              size = 1, alpha = 0.5,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="gold1", high="blue4", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)) 
    ) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") 
  return(plot1)
}






var_importance <- function(shap_result, top_n=10)
{
  var_importance=tibble(var=names(shap_result$mean_shap_score), importance=shap_result$mean_shap_score)
  
  var_importance=var_importance[1:top_n,]
  
  ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
}


test_df <- MIGUS24_pts_AllDxs %>% select(-c(weight, muscle_rel, patient))
test_df %>% group_by(antiepilep) %>% count()
test_df <- test_df %>% group_by(antiepilep) %>% sample_n(10000) %>% ungroup()



model_hd = xgboost(data = as.matrix(test_df[,-1]), nround = 500,
                   objective = "binary:logistic", label=as.matrix(test_df[,1]))  


shap_result = shap.score.rank(xgb_model = model_hd,  X_train = as.matrix(test_df[,-1]), shap_approx = F)

var_importance(shap_result, top_n=50)

shap_long_hd = shap.prep(X_train = as.matrix(test_df[,-1]) , top_n = 25)

plot.shap.summary(data_long = shap_long_hd)

test_df %>% group_by(antiepilep, F3) %>% count() %>% ungroup() %>%
  spread(key=F3, value=n) %>% mutate(perc=`1`/(`1`+`0`))

test_df %>% group_by(muscle_rel, E6) %>% count() %>% ungroup() %>%
  spread(key=E6, value=n) %>% mutate(perc=`1`/(`1`+`0`))



# ------



# Compare MIG Population Distribution based on different criteria  V2 -------
Mod_Sev <- fread("Source/Mod_Sev.txt")
Mod_Sev <- Mod_Sev %>% select(patid) %>% rename("patient"="patid")


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>% select(patid, weight, migraine_onset, migraine_last_dx)
sum(MIGUS24_Demographics$weight) # 22418159
Plus2Dxs <- MIGUS24_Demographics %>% filter(migraine_onset!=migraine_last_dx) %>% select(patid) %>% rename("patient"="patid")


MIGUS24_Drug_Histories <- fread("Source/MIGUS24 Drug Histories.txt")
MIGUS24_Drug_Histories <- gather(MIGUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(patient, weight, Treat) %>% distinct()
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(Treat!="-")

Drug_formulary <- fread("Source/Drug_formulary.txt")
unique(Drug_formulary$drug_group)

string_MIG_spec <- paste0("\\b(",paste0(Drug_formulary$drug_id[
  Drug_formulary$drug_group == "CGRP Nasal"|
    Drug_formulary$drug_group == "CGRP Oral"|
    Drug_formulary$drug_group == "CGRP Injectable"|
    Drug_formulary$drug_group == "Triptans"], collapse = "|"),")\\b")

string_muscle_rel <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Muscle Relaxant"], collapse = "|"),")\\b")
string_antiepilep <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_class == "Antiepileptic"&Drug_formulary$generic_name != "Topiramate"], collapse = "|"),")\\b")



string_Sympt <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Symptomatic"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="Triptans"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drug_formulary$drug_id[(Drug_formulary$drug_group=="Preventive")], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Nasal <- paste0("\\b(",paste0(Drug_formulary$drug_id[Drug_formulary$drug_group=="CGRP Nasal"], collapse = "|"),")\\b")

MIG_spec_pats <- MIGUS24_Drug_Histories %>% filter(grepl(string_MIG_spec, Treat)) %>% select(patient) %>% distinct()


MIGUS24_Drug_Histories <- fread("Source/MIGUS24 Drug Histories.txt")
MIGUS24_Drug_Histories <- gather(MIGUS24_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(patient, weight,  Month, Treat) %>% distinct()
MIGUS24_Drug_Histories <- separate_rows(MIGUS24_Drug_Histories, Treat, sep = ",", convert=T )

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% 
  left_join(Drug_formulary %>% select(drug_id, drug_class) %>% mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id"))

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(drug_class=ifelse(is.na(drug_class), "Lapsed", drug_class))

MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, CV, psychiatric, epileptic) 

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% inner_join(MIGUS24_Demographics, by=c("patient"="patid"))

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(flag==0) %>% select(-flag)

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(-c(CV, psychiatric, epileptic))

MIGUS24_Drug_Histories$Month <- parse_number(as.character(MIGUS24_Drug_Histories$Month))

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% select(-drug_class) %>% 
  arrange(patient, weight, Month, Treat) %>% group_by(patient, weight, Month) %>%
  mutate(Treat=paste0(Treat, collapse=",")) %>% distinct()


MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% ungroup() %>% distinct()

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(Month>=48)

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(Box=ifelse(grepl(string_CGRP_Nasal, Treat), "Z",
                                   ifelse(grepl(string_CGRP_Oral, Treat), "O",
                                          ifelse(grepl(string_CGRP_Inj, Treat), "I",
                                                 ifelse(grepl(string_Prev, Treat)&grepl(string_Triptan, Treat), "K",
                                                        ifelse(grepl(string_Prev, Treat)&grepl(string_Sympt, Treat), "Y",
                                                              ifelse(grepl(string_Prev, Treat), "P",
                                                                     ifelse(grepl(string_Triptan, Treat), "Triptan",
                                                                            ifelse(grepl(string_Sympt, Treat), "Sympt", "Lapsed")))))))))



All_pats <- fread("Source/MIGUS24 Drug Histories.txt")
All_pats <- gather(All_pats, Month, Treat, month1:month60, factor_key=TRUE)
All_pats <- All_pats %>% select(patient, weight, Month) %>% distinct()
All_pats$Month <- parse_number(as.character(All_pats$Month))

MIGUS24_Drug_Histories <- All_pats %>% left_join(MIGUS24_Drug_Histories) %>% 
  mutate(Treat=ifelse(is.na(Treat),"-", Treat)) %>%
  mutate(Box=ifelse(is.na(Box),"Lapsed", Box))


MIGUS24_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 21292150
MIGUS24_Drug_Histories %>% select(patient, weight) %>% distinct() %>% inner_join(Plus2Dxs) %>% summarise(n=sum(weight)) # 13126433
MIGUS24_Drug_Histories %>% select(patient, weight) %>% distinct() %>% 
  inner_join(Plus2Dxs %>% full_join(MIG_spec_pats) %>% distinct()) %>% summarise(n=sum(weight)) # 15665529

MIGUS24_Drug_Histories %>% filter(Month==60) %>% group_by(Box) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories %>% inner_join(Plus2Dxs) %>%  filter(Month==60) %>% group_by(Box) %>% summarise(n=sum(weight))
MIGUS24_Drug_Histories %>% inner_join(Plus2Dxs %>% full_join(MIG_spec_pats) %>% distinct()) %>%  filter(Month==60) %>% group_by(Box) %>% summarise(n=sum(weight))

MIGUS24_Drug_Histories %>% arrange(patient, Month) %>%
  group_by(patient) %>% filter(!grepl("136",Treat) & grepl("136",lead(Treat))) %>%
  ungroup() %>% group_by(Box) %>% summarise(n=sum(weight))

MIGUS24_Drug_Histories %>% inner_join(Plus2Dxs) %>%  arrange(patient, Month) %>%
  group_by(patient) %>% filter(!grepl("136",Treat) & grepl("136",lead(Treat))) %>%
  ungroup() %>% group_by(Box) %>% summarise(n=sum(weight))

MIGUS24_Drug_Histories %>% inner_join(Plus2Dxs %>% full_join(MIG_spec_pats) %>% distinct())  %>%  arrange(patient, Month) %>%
  group_by(patient) %>% filter(!grepl("136",Treat) & grepl("136",lead(Treat))) %>%
  ungroup() %>% group_by(Box) %>% summarise(n=sum(weight))

MIGUS24_Drug_Histories <- separate_rows(MIGUS24_Drug_Histories, Treat, sep = ",", convert=T )
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% filter(!grepl(string_muscle_rel, Treat)) 
MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(epileptics=ifelse(grepl(string_antiepilep, Treat),1,0))


MIGUS24_Demographics <- fread("Source/MIGUS24 Demographics.txt")
MIGUS24_Demographics <- MIGUS24_Demographics %>%  select(patid, epileptic) 

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% left_join(MIGUS24_Demographics, by=c("patient"="patid")) %>% 
  mutate(to_exclude=ifelse(epileptic==1&epileptics==1,1,0)) %>% filter(to_exclude!=1) %>%
  select(-c(to_exclude, epileptic, epileptics))

MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% arrange(patient, weight, Month, Treat) %>%
  group_by(patient, weight, Month) %>% mutate(Treat=paste0(Treat, collapse=",")) %>% distinct()

MIGUS24_Drug_Histories <- All_pats %>% left_join(MIGUS24_Drug_Histories) %>% 
  mutate(Treat=ifelse(is.na(Treat),"-", Treat)) %>%
  mutate(Box=ifelse(is.na(Box),"Lapsed", Box))



MIGUS24_Drug_Histories <- MIGUS24_Drug_Histories %>% mutate(Box=ifelse(grepl(string_CGRP_Nasal, Treat), "Z",
                                   ifelse(grepl(string_CGRP_Oral, Treat), "O",
                                          ifelse(grepl(string_CGRP_Inj, Treat), "I",
                                                 ifelse(grepl(string_Prev, Treat)&grepl(string_Triptan, Treat), "K",
                                                        ifelse(grepl(string_Prev, Treat)&grepl(string_Sympt, Treat), "Y",
                                                              ifelse(grepl(string_Prev, Treat), "P",
                                                                     ifelse(grepl(string_Triptan, Treat), "Triptan",
                                                                            ifelse(grepl(string_Sympt, Treat), "Sympt", "Lapsed")))))))))



MIGUS24_Drug_Histories %>% filter(Month==60) %>% group_by(Box) %>% summarise(n=sum(weight))


MIGUS24_Drug_Histories %>% arrange(patient, Month) %>%
  group_by(patient) %>% filter(!grepl("136",Treat) & grepl("136",lead(Treat))) %>%
  ungroup() %>% group_by(Box) %>% summarise(n=sum(weight))


# ------
