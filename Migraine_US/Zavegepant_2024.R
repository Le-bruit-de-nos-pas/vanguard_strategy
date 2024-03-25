
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
# ------
# Individual Molecule penetrantion ever ------------
ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, generic, drug_class, drug_group) %>% distinct()
ZAVUS24_Doses <- ZAVUS24_Doses %>% group_by(generic,drug_class, drug_group) %>% count() %>% mutate(n=n/260)
data.frame(ZAVUS24_Doses) %>% arrange(drug_group, desc(n))

# --------------
# Number of classes ever tried ----------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, generic, drug_class, drug_group) %>% distinct()
# -----------
# Number of classes ---------------------

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, drug_class, drug_group) %>% distinct()

data.frame(ZAVUS24_Doses %>% group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n") %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/260,2)))

ZAVUS24_Doses %>% group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n") %>% summarise(mean=mean(classes))

ZAVUS24_Doses <- fread("Source/ZAVUS24 Doses.txt")
ZAVUS24_Doses <- ZAVUS24_Doses %>% select(patid, drug_class, generic) %>% distinct()

data.frame(ZAVUS24_Doses %>% filter(grepl("CGRP", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n") %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/260,2)))

#   classes    n
# 1       1 0.17
# 2       2 0.27
# 3       3 0.25
# 4       4 0.16
# 5       5 0.08
# 6       6 0.05
# 7       7 0.01
# 8       8 0.01


ZAVUS24_Doses %>% filter(grepl("CGRP", drug_class)) %>% 
  group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n") %>% summarise(mean=mean(classes)) # 2.97



# TRIPTAN

unique(ZAVUS24_Doses$drug_class)

data.frame(
ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("Triptan", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/260,2)))

#   classes    n
# 1       0 0.39
# 2       1 0.36
# 3       2 0.17
# 4       3 0.06
# 5       4 0.02

ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("Triptan", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>% summarise(mean=mean(classes)) # 1.0





# NSAID

unique(ZAVUS24_Doses$drug_class)

data.frame(
ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("NSAID", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/260,2)))

# 1       0 0.25
# 2       1 0.30
# 3       2 0.20
# 4       3 0.14
# 5       4 0.07
# 6       5 0.03
# 7       6 0.01
# 8       8 0.00

ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("NSAID", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>% summarise(mean=mean(classes)) # 1.6





# OPIOID

unique(ZAVUS24_Doses$drug_class)

data.frame(
ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("Opioid", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>%
  group_by(classes) %>% count() %>% mutate(n=round(n/260,2)))

# 1        0 0.31
# 2        1 0.17
# 3        2 0.17
# 4        3 0.12
# 5        4 0.09
# 6        5 0.07
# 7        6 0.03
# 8        7 0.03
# 9        8 0.02
# 10       9 0.00

ZAVUS24_Doses %>% select(patid) %>% distinct() %>% left_join(
  ZAVUS24_Doses %>% filter(grepl("Opioid", drug_class)) %>%
             group_by(patid) %>% count() %>% ungroup() %>% rename("classes"="n")
)  %>% mutate(classes=ifelse(is.na(classes),0,classes)) %>% summarise(mean=mean(classes)) # 2.1

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


# ----------