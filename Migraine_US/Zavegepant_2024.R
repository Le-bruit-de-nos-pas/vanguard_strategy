
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
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