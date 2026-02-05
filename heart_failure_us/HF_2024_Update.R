library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
library(gbm)

options(scipen = 999)

# Patients to track ---------------
HFUS24_Demographics <- fread("HFUS24 Demographics.txt")
sum(HFUS24_Demographics$weight) # 17835035

HFUS24_Demographics %>% filter(deceased==0 | death_dt>="2023-02-01" ) %>% summarise(n=sum(weight)) # 12063530
range(as.Date(HFUS24_Demographics$death_dt), na.rm=T)
range(as.Date(HFUS24_Demographics$HF_1st_Rx), na.rm=T)

HFUS24_Demographics <- HFUS24_Demographics %>% filter(deceased==0 | death_dt>="2023-02-01" )
sum(HFUS24_Demographics$weight) # 12063530

HFUS24_Demographics %>% group_by(HF_type) %>% summarise(pop=sum(weight))
HFUS24_Demographics <- HFUS24_Demographics %>% filter(HF_type %in% c("HFpEF_only", "HFrEF_only"))

# 1 HFpEF_HFrEF 2960111.
# 2 HFpEF_only  3592348.
# 3 HFrEF_only  2013925.
# 4 Unknown     3497146.

Pats_to_track <- HFUS24_Demographics %>% select(patid, weight, HF_type)

# ---------

# Drug Usage Top Line ----------------

HFUS24_Doses <- fread("HFUS24 Doses.txt")
head(HFUS24_Doses)
names(HFUS24_Doses)
HFUS24_Doses <- HFUS24_Doses %>% inner_join(Pats_to_track)


range(HFUS24_Doses$from_dt)

HFUS24_Doses <- HFUS24_Doses %>% filter(from_dt>="2023-02-01")


HFUS24_Doses %>% select(drug_class, patid, weight, HF_type) %>% distinct() %>%
  group_by(HF_type, drug_class) %>% summarise(pop=sum(weight)) %>%
  mutate(pop=ifelse(HF_type=="HFpEF_only", 100*pop/3592348, 100*pop/2013925)) %>%
  spread(key=HF_type, value=pop) 


# ---------
# --------------
# Drug Classes Tried Past 12 months -------------

HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")
sum(HFpEF_only_pats$weight)

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")
HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-disease) %>% inner_join(HFpEF_only_pats %>% select(patid))
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% filter(Drugs!="-") 
HFUS24_Drug_Histories <- separate_rows(HFUS24_Drug_Histories, Drugs, sep = ",", convert=T) 


HFUS24_Doses <- fread("HFUS24 Doses/HFUS24 Doses.txt")
Drug_classes_Lookup <- HFUS24_Doses %>% select(drug_id, drug_class) %>% distinct()

fwrite(Drug_classes_Lookup, "Drug_classes_Lookup.txt")

data.frame(HFUS24_Drug_Histories %>% filter(Month>=49) %>% select(patid, weight, Drugs) %>% distinct() %>%
  left_join(Drug_classes_Lookup, by=c("Drugs"="drug_id")) %>%
  group_by(drug_class) %>% summarise(n=sum(weight)/2894070)) %>%
  arrange(-n)

HFUS24_Drug_Histories %>% filter(Month>=49) %>% select(patid, weight, Drugs) %>% distinct() %>%
  left_join(Drug_classes_Lookup, by=c("Drugs"="drug_id"))  %>%
  filter(drug_class %in% c("ARNI", "ACE", "ARB")) %>% select(patid, weight) %>% distinct() %>%
  summarise(n=sum(weight)/2894070)

HFUS24_Drug_Histories %>% filter(Month>=49) %>% select(patid, weight, Drugs) %>% distinct() %>%
  left_join(Drug_classes_Lookup, by=c("Drugs"="drug_id"))  %>%
  filter(drug_class %in% c("Diuretic_Loop", "Diuretic_Thiazide", "Diuretic_Other")) %>% select(patid, weight) %>% distinct() %>%
  summarise(n=sum(weight)/2894070)

# --------
# Drug Usage Before / After 1st Heart Failure Dx -----------

HFUS24_Demographics <- fread("HFUS24 Demographics.txt")
HFUS24_Demographics <- HFUS24_Demographics %>% select(patid, weight, HF_onset )
HFUS24_Demographics <- HFUS24_Demographics %>% mutate(HF_onset  =as.character(HF_onset  ))
HFUS24_Demographics <- HFUS24_Demographics %>% mutate(HF_onset  =str_sub(HF_onset  , 1L, 7L))

HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")
HFUS24_Demographics <- HFpEF_only_pats %>% select(patid) %>% left_join(HFUS24_Demographics)

Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- as.Date(Months_lookup$Month)
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")
Months_lookup$Month <- as.character(Months_lookup$Month)

HFUS24_Demographics <- HFUS24_Demographics %>% left_join(Months_lookup, by=c("HF_onset"="Month")) %>% select(patid, Exact_Month) %>% distinct()

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")
HFUS24_Drug_Histories <- HFUS24_Demographics %>% select(patid) %>% inner_join(HFUS24_Drug_Histories)

names(HFUS24_Demographics)[2] <- "First_HF_Dx"

HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  select(-disease)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))

HFUS24_Drug_Histories <- HFpEF_only_pats %>% select(patid) %>% inner_join(HFUS24_Drug_Histories)

HFUS24_Drug_Histories %>% inner_join(HFpEF_only_pats %>% select(patid))


HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(HFUS24_Demographics) %>% 
  mutate(Lapsed=Month-First_HF_Dx) %>% mutate(Lapsed=Lapsed-3) %>%
  filter((Lapsed>=(-12)) & (Lapsed<=(12)))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(patid, Month) %>% distinct() %>%
  group_by(patid) %>% count() %>% filter(n>=25) %>%
  select(patid) %>% left_join(HFUS24_Drug_Histories)

HFUS24_Drug_Histories %>%  select(patid, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 1004756
#1232597

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  ungroup()

HFUS24_Drug_Histories <- separate_rows(HFUS24_Drug_Histories, Drugs, sep = ",", convert=T)

Drug_classes_Lookup <- fread("Drug_classes_Lookup.txt")

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% left_join(Drug_classes_Lookup %>% mutate(drug_id=as.character(drug_id)), by=c("Drugs"="drug_id")) 

HFUS24_Drug_Histories %>% select(patid, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))/1232597) %>% ungroup() %>%
  spread(key=Lapsed, value=n) %>%
  select(drug_class, `1`)  %>% arrange(-`1`)


HFUS24_Drug_Histories  %>% select(patid, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/1232597) %>%
  mutate(drug_class=ifelse(is.na(drug_class), "Lapsed", drug_class)) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic_Loop","ARB","ACE",
                                                "Diuretic_Thiazide", "Lapsed",
                                                "Vasodilator", "MRA", "SGLT2", "GLP1_Injectable", "Inotropic",
                                                "Diuretic_Other", "Cardiac_Device", "Other", "Diuretic_Injectable",
                                                "ARNI", "GLP1_Oral", "Heart_Transplant"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=0.6) +
  ylim(0,60) +
  xlab("\n No. Elapsed Months \n(Before/After 1st HF Dx)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B482DA","#3977c8","#a49e16","#e2dd5c","#39c8c2","#535353","#dfdfdf","#131174",
"#F1AC02","#AE1641","#dfdfdf","#dfdfdf","#dfdfdf","#dfdfdf","#dfdfdf","#ace25c",
"#e25e5c","#dfdfdf"))

# ----------


# Drug Usage Before / After 1st  SGLT2 / MRA / ARNi / GLP1 Inj -----------


HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(HFpEF_only_pats %>% select(patid))

Drug_classes_Lookup <- fread("Drug_classes_Lookup.txt")

string_SGLT2 <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="MRA"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="ARNI"], collapse = "|"),")\\b")
string_GLP1_Inj <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="GLP1 Injectable"], collapse = "|"),")\\b")


HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  select(-disease)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))

First_ARNI <- HFUS24_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  select(patid, Month) %>% rename("First_ARNI"="Month")
First_SGLT2 <- HFUS24_Drug_Histories %>% filter(grepl(string_SGLT2,Drugs)) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  select(patid, Month) %>% rename("First_SGLT2"="Month")
First_MRA <- HFUS24_Drug_Histories %>% filter(grepl(string_MRA,Drugs)) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  select(patid, Month) %>% rename("First_MRA"="Month")
First_GLP1 <- HFUS24_Drug_Histories %>% filter(grepl(string_GLP1_Inj,Drugs)) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  select(patid, Month) %>% rename("First_GLP1"="Month")


HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")

HFUS24_Drug_Histories <- First_ARNI %>% inner_join(HFUS24_Drug_Histories)

HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  select(-disease)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))

HFUS24_Drug_Histories <- HFpEF_only_pats %>% select(patid) %>% inner_join(HFUS24_Drug_Histories)

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(First_GLP1) %>% 
  mutate(Lapsed=Month-First_GLP1) %>% 
  filter((Lapsed>=(-6)) & (Lapsed<=(6)))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(patid, Month) %>% distinct() %>%
  group_by(patid) %>% count() %>% filter(n>=13) %>%
  select(patid) %>% left_join(HFUS24_Drug_Histories)

HFUS24_Drug_Histories %>%  select(patid, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 1004756
#7050

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  ungroup()

HFUS24_Drug_Histories <- separate_rows(HFUS24_Drug_Histories, Drugs, sep = ",", convert=T)

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% left_join(Drug_classes_Lookup %>% mutate(drug_id=as.character(drug_id)), by=c("Drugs"="drug_id")) 

HFUS24_Drug_Histories %>% select(patid, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))/7050) %>% ungroup() %>%
  spread(key=Lapsed, value=n) %>%
  select(drug_class, `1`)  %>% arrange(-`1`)


HFUS24_Drug_Histories  %>% select(patid, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup()  %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/7050) %>%
  mutate(drug_class=ifelse(is.na(drug_class), "Lapsed", drug_class)) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("Beta_Blocker", "Diuretic_Loop","ARB","ACE",
                                                "Diuretic_Thiazide", "Lapsed",
                                                "Vasodilator", "MRA", "SGLT2", "GLP1_Injectable", "Inotropic",
                                                "Diuretic_Other", "Cardiac_Device", "Other", "Diuretic_Injectable",
                                                "ARNI", "GLP1_Oral", "Heart_Transplant"))) %>%
  ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=0.6) +
  ylim(0,100) +
  xlab("\n No. Elapsed Months \n(Before/After 1st GLP1 Injectable)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B482DA","#3977c8","#a49e16","#e2dd5c","#39c8c2","#535353","#dfdfdf","#131174",
"#F1AC02","#AE1641","#dfdfdf","#dfdfdf","#dfdfdf","#dfdfdf","#dfdfdf","#ace25c",
"#e25e5c","#dfdfdf"))

# ----------


# ARNi MRA SGLT2 population over time ------
HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(HFpEF_only_pats %>% select(patid))

Drug_classes_Lookup <- fread("Drug_classes_Lookup.txt")

string_SGLT2 <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="MRA"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="ARNI"], collapse = "|"),")\\b")
string_GLP1_Inj <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="GLP1 Injectable"], collapse = "|"),")\\b")
string_GLP1_Oral <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="GLP1 Oral"], collapse = "|"),")\\b")


HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  select(-disease)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% filter(!grepl("-", Drugs))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)|grepl(string_SGLT2, Drugs)|
                                                            grepl(string_MRA, Drugs)|grepl(string_GLP1_Oral, Drugs)|grepl(string_GLP1_Inj,Drugs))

HFUS24_Drug_Histories <- separate_rows(HFUS24_Drug_Histories, Drugs, sep = ",", convert=T)

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)|grepl(string_SGLT2, Drugs)|grepl(string_MRA, Drugs)|grepl(string_GLP1_Oral, Drugs)|grepl(string_GLP1_Inj, Drugs))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(Drug_classes_Lookup, by=c("Drugs"="drug_id"))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-Drugs) %>% distinct()


data.frame(HFUS24_Drug_Histories %>% group_by(Month, drug_class) %>% summarise(n=sum(weight))) %>%
  spread(key=drug_class, value=n)

data.frame(HFUS24_Drug_Histories %>% group_by(Month, drug_class) %>% summarise(n=sum(weight))) %>%
  ggplot(aes(Month, n, colour=drug_class)) +
  geom_line(size=2, alpha=0.6) +
  theme_minimal() +
  scale_colour_manual(values=c("#ff0000", "#54752e", "#b5df84", "#0693e3", "#ffa500" )) +
  xlab("\n Month") + ylab("Population ON each class \n")


# ------
# Highest therapy over past 5 years -----

HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(HFpEF_only_pats %>% select(patid))

Drug_classes_Lookup <- fread("Drug_classes_Lookup.txt")

HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  select(-disease)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% filter(!grepl("-", Drugs))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-Month) %>% distinct()

HFUS24_Drug_Histories <- separate_rows(HFUS24_Drug_Histories, Drugs, sep = ",", convert=T)

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(Drug_classes_Lookup, by=c("Drugs"="drug_id"))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-Drugs) %>% distinct()

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% mutate(exp=1) %>% spread(key=drug_class, value=exp)

HFUS24_Drug_Histories[is.na(HFUS24_Drug_Histories)] <- 0


HFUS24_Drug_Histories %>% group_by(SGLT2, MRA) %>% summarise(n=sum(weight))


HFUS24_Drug_Histories %>% 
  mutate(Stock=ifelse(MRA==1|SGLT2==1, "MRA|SGLT2",
                      ifelse( (`Beta Blocker`==1&ACE==1)|(`Beta Blocker`==1&ARB==1), "BB+RAAS",
                              ifelse(`Beta Blocker`==1, "BB",
                                     ifelse(ACE==1|ARB==1, "RAAS", "Other"))))) %>%
  group_by(Stock) %>% summarise(n=sum(weight))


HFUS24_Drug_Histories %>% 
  mutate(Stock=ifelse(MRA==1|SGLT2==1, "MRA|SGLT2",
                      ifelse( (`Beta Blocker`==1&ACE==1)|(`Beta Blocker`==1&ARB==1), "BB+RAAS",
                              ifelse(`Beta Blocker`==1, "BB",
                                     ifelse(ACE==1|ARB==1, "RAAS", "Other"))))) %>%
  group_by(Stock, Diuretic_Loop) %>% summarise(n=sum(weight)) %>%
  spread(key=Diuretic_Loop, value=n)


HFUS24_Drug_Histories %>% 
  mutate(Stock=ifelse(MRA==1|SGLT2==1, "MRA|SGLT2",
                      ifelse( (`Beta Blocker`==1&ACE==1)|(`Beta Blocker`==1&ARB==1), "BB+RAAS",
                              ifelse(`Beta Blocker`==1, "BB",
                                     ifelse(ACE==1|ARB==1, "RAAS", "Other"))))) %>%
  filter(Stock=="MRA|SGLT2") %>%
  filter(`Beta Blocker`==1) %>% summarise(n=sum(weight)) %>%
  filter(ACE==1|ARB==1==1) %>% summarise(n=sum(weight)) %>%
    filter(`Beta Blocker`==1) %>% summarise(n=sum(weight)) %>%

  
  
HFUS24_Drug_Histories %>% 
  mutate(Stock=ifelse(MRA==1|SGLT2==1, "MRA|SGLT2",
                      ifelse( (`Beta Blocker`==1&ACE==1)|(`Beta Blocker`==1&ARB==1), "BB+RAAS",
                              ifelse(`Beta Blocker`==1, "BB",
                                     ifelse(ACE==1|ARB==1, "RAAS", "Other"))))) %>%
  filter(MRA==1&SGLT2==1) %>%
  #filter(`Beta Blocker`==1) %>% summarise(n=sum(weight)) %>%
  #filter(ACE==1|ARB==1) %>% summarise(n=sum(weight)) %>%
  #filter(`Diuretic_Injectable`==1|`Diuretic_Loop`==1|`Diuretic_Other`==1|
   #       `Diuretic_Thiazide`==1) %>% summarise(n=sum(weight)) %>%
   filter(`Cardiac Device`==1|`GLP1 Oral`==1|`Heart Transplant`==1|`Inotropic`==1|
             `GLP1 Injectable`==1|`Other`==1|`Vasodilator`==1) %>% summarise(n=sum(weight)) 


HFUS24_Drug_Histories %>% 
  mutate(Stock=ifelse(MRA==1|SGLT2==1, "MRA|SGLT2",
                      ifelse( (`Beta Blocker`==1&ACE==1)|(`Beta Blocker`==1&ARB==1), "BB+RAAS",
                              ifelse(`Beta Blocker`==1, "BB",
                                     ifelse(ACE==1|ARB==1, "RAAS", "Other"))))) %>%
  filter(MRA==1&SGLT2==1) %>%
  filter(Diuretic_Loop==1) %>% summarise(n=sum(weight))

# ------------
# Highest therapy over past 5 years w/ GLP1 -----

HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(HFpEF_only_pats %>% select(patid))

Drug_classes_Lookup <- fread("Drug_classes_Lookup.txt")

HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  select(-disease)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% filter(!grepl("-", Drugs))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-Month) %>% distinct()

HFUS24_Drug_Histories <- separate_rows(HFUS24_Drug_Histories, Drugs, sep = ",", convert=T)

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(Drug_classes_Lookup, by=c("Drugs"="drug_id"))

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-Drugs) %>% distinct()

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% mutate(exp=1) %>% spread(key=drug_class, value=exp)

HFUS24_Drug_Histories[is.na(HFUS24_Drug_Histories)] <- 0

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% mutate(GLP1=ifelse(`GLP1 Injectable`==1|`GLP1 Oral`==1,1,0))


HFUS24_Drug_Histories %>% group_by(SGLT2, MRA, GLP1) %>% summarise(n=sum(weight))

  SGLT2   MRA  GLP1        n
1     0     0     0 1793028.
2     0     0     1  201621.
3     0     1     0  355455.
4     0     1     1   59292.
5     1     0     0  175517.
6     1     0     1  134624.
7     1     1     0   86259.
8     1     1     1   59174.

HFUS24_Drug_Histories %>% 
  mutate(Stock=ifelse(MRA==1|SGLT2==1|GLP1==1, "MRA|SGLT2|GLP1",
                      ifelse( (`Beta Blocker`==1&ACE==1)|(`Beta Blocker`==1&ARB==1), "BB+RAAS",
                              ifelse(`Beta Blocker`==1, "BB",
                                     ifelse(ACE==1|ARB==1, "RAAS", "Other"))))) %>%
  group_by(Stock) %>% summarise(n=sum(weight))

# ------------
# Source of MRA / SGLT2 Inflows ----------

HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")

HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(HFpEF_only_pats %>% select(patid))

Drug_classes_Lookup <- fread("Drug_classes_Lookup.txt")



string_SGLT2 <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="MRA"], collapse = "|"),")\\b")
string_BB <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="Beta Blocker"], collapse = "|"),")\\b")
string_RAAS <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="ACE"|Drug_classes_Lookup$drug_class=="ARB"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="ARNI"], collapse = "|"),")\\b")
string_GLP1 <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="GLP1 Injectable"|Drug_classes_Lookup$drug_class=="GLP1 Oral"], collapse = "|"),")\\b")


HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  select(-disease)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))



HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%
   mutate(Stock=ifelse(grepl(string_MRA, Drugs)|grepl(string_SGLT2, Drugs), "MRA|SGLT2",
                      ifelse( grepl(string_BB, Drugs)&grepl(string_RAAS, Drugs), "BB+RAAS",
                              ifelse(grepl(string_BB, Drugs), "BB",
                                     ifelse(grepl(string_RAAS, Drugs), "RAAS", "Other"))))) 



HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% mutate(Year=ifelse(Month>=49, 5,
                                             ifelse(Month>=37,4,
                                                    ifelse(Month>=25,3,
                                                           ifelse(Month>=13,2,1)))))


HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% arrange(patid, Month) %>% group_by(patid) 


temp <- HFUS24_Drug_Histories %>% 
  mutate(flow=ifelse(  (!grepl(string_GLP1, Drugs))&(grepl(string_GLP1, lead(Drugs))) , 1,0)) 

temp %>% filter(flow==1) %>% group_by(patid) %>% filter(Month==min(Month)) %>%
  ungroup() %>%  group_by(Year, Stock) %>% summarise(n=sum(weight)) %>%
  spread(key=Stock, value=n)


# -----------

# Persistency ------------

# All episodes
HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")
HFUS24_Doses <- fread("HFUS24 Doses/HFUS24 Doses.txt")
HFUS24_Doses <- HFUS24_Doses %>% inner_join(HFpEF_only_pats %>% select(patid))

HFUS24_Doses <- HFUS24_Doses %>% select(patid, weight, drug_id, generic_name, drug_class, from_dt)

#Drugs_Used <- HFUS24_Doses %>% filter(from_dt>="2020-01-01"&from_dt<="2023-12-31")  %>% select(patid, drug_id) %>% distinct()
Drugs_Used <- HFUS24_Doses %>% select(patid, drug_id) %>% distinct()


Drugs_classes <- HFUS24_Doses %>% select(drug_id, generic_name, drug_class) %>% distinct()
Drugs_classes %>% arrange(drug_class)
Drugs_classes <- Drugs_classes %>% filter(drug_class=="SGLT2"|drug_class=="ARNI"|generic_name=="Tirzepatide Injection"|generic_name=="Semaglutide Injection")
Drugs_classes <- Drugs_classes %>% mutate(generic_name=ifelse(drug_class=="SGLT2", "SGLT2", generic_name))

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")
HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, drug_id, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-disease) %>% inner_join(HFpEF_only_pats %>% select(patid))



HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% filter(drug_id!="-") 
HFUS24_Drug_Histories <- separate_rows(HFUS24_Drug_Histories, drug_id, sep = ",", convert=T) 
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(Drugs_Used)
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(Drugs_classes) %>% select(-drug_class)

HFUS24_Drug_Histories %>% group_by(patid, weight, generic_name) %>% count() %>%
  ungroup() %>% group_by(generic_name) %>% summarise(mean=mean(n))


HFUS24_Drug_Histories %>% group_by(patid, weight, generic_name) %>% count() %>%
  ungroup() %>% 
  ggplot(aes(n, colour=generic_name, fill=generic_name)) +
  geom_histogram(alpha=0.8) +
  facet_wrap(~generic_name, scales="free_y") +
  theme_minimal() +
  scale_colour_manual(values=c("#D3D3D3","#66A4B9", "#FFEFCA", "#C86060")) +
  scale_fill_manual(values=c("#D3D3D3","#66A4B9", "#FFEFCA", "#C86060")) +
  xlab("\n Number of Months ON Drug") + ylab("Patient Sample Count \n") # 700 500



# First Episode Only

HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")
HFUS24_Doses <- fread("HFUS24 Doses/HFUS24 Doses.txt")
HFUS24_Doses <- HFUS24_Doses %>% inner_join(HFpEF_only_pats %>% select(patid))
HFUS24_Doses <- HFUS24_Doses %>% select(patid, weight, drug_id, generic_name, drug_class, from_dt)
Drugs_Used <- HFUS24_Doses %>% filter(from_dt>="2020-01-01"&from_dt<="2023-12-31")  %>% select(patid, drug_id) %>% distinct()

Drugs_classes <- HFUS24_Doses %>% select(drug_id, generic_name, drug_class) %>% distinct()
Drugs_classes %>% arrange(drug_class)
Drugs_classes <- Drugs_classes %>% filter(drug_class=="SGLT2"|drug_class=="ARNI"|generic_name=="Tirzepatide Injection"|generic_name=="Semaglutide Injection")
Drugs_classes <- Drugs_classes %>% mutate(generic_name=ifelse(drug_class=="SGLT2", "SGLT2", generic_name))

string_SGLT2 <- paste0("\\b(",paste0(Drugs_classes$drug_id[Drugs_classes$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(Drugs_classes$drug_id[Drugs_classes$drug_class=="ARNI"], collapse = "|"),")\\b")
string_Sema <- paste0("\\b(",paste0(Drugs_classes$drug_id[Drugs_classes$generic_name=="Semaglutide Injection"], collapse = "|"),")\\b")
string_Terza <- paste0("\\b(",paste0(Drugs_classes$drug_id[Drugs_classes$generic_name=="Tirzepatide Injection"], collapse = "|"),")\\b")

HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")
HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, drug_id, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-disease) %>% inner_join(HFpEF_only_pats %>% select(patid))

SGLT2 <- HFUS24_Drug_Histories %>% arrange(patid, Month) %>% group_by(patid) %>% mutate(SGLT2=ifelse(grepl(string_SGLT2, drug_id), 1,0))
ARNI <- HFUS24_Drug_Histories %>% arrange(patid, Month) %>% group_by(patid) %>% mutate(ARNI=ifelse(grepl(string_ARNI, drug_id), 1,0))
SEMA <- HFUS24_Drug_Histories %>% arrange(patid, Month) %>% group_by(patid) %>% mutate(SEMA=ifelse(grepl(string_Sema, drug_id), 1,0))
TERZA <- HFUS24_Drug_Histories %>% arrange(patid, Month) %>% group_by(patid) %>% mutate(TERZA=ifelse(grepl(string_Terza, drug_id), 1,0))

Drugs_Used <- Drugs_Used %>% mutate(drug_class=ifelse(grepl(string_SGLT2, drug_id), "SGLT2",
                                        ifelse(grepl(string_ARNI, drug_id), "ARNI",
                                               ifelse(grepl(string_Sema, drug_id), "SEMA",
                                                      ifelse(grepl(string_Terza, drug_id), "TERZA", NA)))))

unique(Drugs_Used$drug_class)

Drugs_Used <- Drugs_Used %>% drop_na() %>% select(patid, drug_class)

SGLT2 <- SGLT2 %>%
  group_by(patid) %>% mutate(grp = rle(SGLT2)$lengths %>% {rep(seq(length(.)), .)}) %>%
  filter(SGLT2==1) %>%
  group_by(patid) %>% filter(grp==min(grp)) %>% select(patid, weight, Month) %>% distinct() %>% mutate(drug_class="SGLT2")

ARNI <- ARNI %>%
  group_by(patid) %>% mutate(grp = rle(ARNI)$lengths %>% {rep(seq(length(.)), .)}) %>%
  filter(ARNI==1) %>%
  group_by(patid) %>% filter(grp==min(grp)) %>% select(patid, weight, Month) %>% distinct() %>% mutate(drug_class="ARNI")

SEMA <- SEMA %>%
  group_by(patid) %>% mutate(grp = rle(SEMA)$lengths %>% {rep(seq(length(.)), .)}) %>%
  filter(SEMA==1) %>%
  group_by(patid) %>% filter(grp==min(grp)) %>% select(patid, weight, Month) %>% distinct() %>% mutate(drug_class="SEMA")

TERZA <- TERZA %>%
  group_by(patid) %>% mutate(grp = rle(TERZA)$lengths %>% {rep(seq(length(.)), .)}) %>%
  filter(TERZA==1) %>%
  group_by(patid) %>% filter(grp==min(grp)) %>% select(patid, weight, Month) %>% distinct() %>% mutate(drug_class="TERZA")

SGLT2 %>% bind_rows(ARNI) %>% bind_rows(SEMA) %>% bind_rows(TERZA) %>%
  ungroup() %>%
  inner_join(Drugs_Used %>% distinct()) %>%
  group_by(drug_class, patid, weight) %>% count() %>% ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n))



SGLT2 %>% bind_rows(ARNI) %>% bind_rows(SEMA) %>% bind_rows(TERZA) %>%
  ungroup() %>%
  inner_join(Drugs_Used %>% distinct()) %>%
  group_by(drug_class, patid, weight) %>% count() %>% ungroup() %>%
  ggplot(aes(n, colour=drug_class, fill=drug_class)) +
  geom_histogram(alpha=0.8) +
  facet_wrap(~drug_class, scales="free_y") +
  theme_minimal() +
  scale_colour_manual(values=c("#D3D3D3","#66A4B9", "#FFEFCA", "#C86060")) +
  scale_fill_manual(values=c("#D3D3D3","#66A4B9", "#FFEFCA", "#C86060")) +
  xlab("\n Number of Months ON Drug") + ylab("Patient Sample Count \n") # 700 500

# -------------


# Comorbidities -------


HFUS24_Comorbidities_with_ICD9_events <- fread("HFUS24_Comorbidities_with_ICD9_events.txt")

names(HFUS24_Comorbidities_with_ICD9_events)

NAFLD <- HFUS24_Comorbidities_with_ICD9_events %>% filter(NAFLD==1) %>% select(patid)

Diabetes <- HFUS24_Comorbidities_with_ICD9_events %>% filter(`Type 2 Diabetes With Complications`==1|
                                              `Type 2 Diabetes`==1|
                                              `Secondary Diabetes With Complications`==1|
                                              `Secondary Diabetes`==1|
                                              `Type 1 Diabetes With Complications`==1|
                                              `Type 1 Diabetes`==1|
                                              `General Diabetes With Complications`==1|
                                              `General Diabetes`==1|
                                              `Treatment For Diabetes`==1) %>% select(patid)

CKD <- HFUS24_Comorbidities_with_ICD9_events %>% filter(`Chronic Kidney Disease`==1|
                                              `End Stage Kidney Disease`==1|
                                              `Kidney Transplant`==1) %>% select(patid)

PAD <- HFUS24_Comorbidities_with_ICD9_events %>% filter(`Generalized Atherosclerosis`==1|
                                              `Peripheral Atherosclerosis`==1|
                                              `Peripheral Embolism`==1|
                                              `Peripheral Vascular Disorder`==1) %>% select(patid)


Obesity <- HFUS24_Comorbidities_with_ICD9_events %>% filter(`General Obesity`==1|
                                              `Moderate Obesity`==1|
                                              `Severe Obesity`==1|
                                              `Morbid Obesity`==1) %>% select(patid)


HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")

sum(HFpEF_only_pats$weight) # 2894070

HFpEF_only_pats %>% inner_join(NAFLD) %>% summarise(n=sum(weight)/2894070) # 0.1508255
HFpEF_only_pats %>% inner_join(Diabetes) %>% summarise(n=sum(weight)/2894070) # 0.55887 59
HFpEF_only_pats %>% inner_join(CKD) %>% summarise(n=sum(weight)/2894070) # 0.5160211
HFpEF_only_pats %>% inner_join(PAD) %>% summarise(n=sum(weight)/2894070) # 0.5012425
HFpEF_only_pats %>% inner_join(Obesity) %>% summarise(n=sum(weight)/2894070) # 0.6932299



HFpEF_only_pats <- fread("HFpEF_only_pats_NYHA2_4.txt")
HFUS24_Drug_Histories <- fread("HFUS24 Drug Histories.txt")
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% inner_join(HFpEF_only_pats %>% select(patid))
Drug_classes_Lookup <- fread("Drug_classes_Lookup.txt")

string_SGLT2 <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="SGLT2"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="MRA"], collapse = "|"),")\\b")
string_ARNI <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="ARNI"], collapse = "|"),")\\b")
string_GLP1 <- paste0("\\b(",paste0(Drug_classes_Lookup$drug_id[Drug_classes_Lookup$drug_class=="GLP1 Injectable"|Drug_classes_Lookup$drug_class=="GLP1 Oral"], collapse = "|"),")\\b")

HFUS24_Drug_Histories <- gather(HFUS24_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>%  select(-disease)
HFUS24_Drug_Histories$Month <- parse_number(as.character(HFUS24_Drug_Histories$Month))
HFUS24_Drug_Histories <- HFUS24_Drug_Histories %>% select(-Month) %>% filter(Drugs!="-") %>% distinct()


SGLT2 <- HFUS24_Drug_Histories %>% filter(grepl(string_SGLT2, Drugs)) %>% select(patid) %>% distinct()
MRA <- HFUS24_Drug_Histories %>% filter(grepl(string_MRA, Drugs)) %>% select(patid) %>% distinct()
ARNI <- HFUS24_Drug_Histories %>% filter(grepl(string_ARNI, Drugs)) %>% select(patid) %>% distinct()
GLP1 <- HFUS24_Drug_Histories %>% filter(grepl(string_GLP1, Drugs)) %>% select(patid) %>% distinct()


HFpEF_only_pats %>% inner_join(NAFLD) %>% summarise(n=sum(weight)) # 436499.5
HFpEF_only_pats %>% inner_join(NAFLD) %>% inner_join(SGLT2) %>% summarise(n=sum(weight)/436499.5)  # 0.2188992
HFpEF_only_pats %>% inner_join(NAFLD) %>% inner_join(MRA) %>% summarise(n=sum(weight)/436499.5)  # 0.2375443
HFpEF_only_pats %>% inner_join(NAFLD) %>% inner_join(ARNI) %>% summarise(n=sum(weight)/436499.5)  # 0.01777867
HFpEF_only_pats %>% inner_join(NAFLD) %>% inner_join(GLP1) %>% summarise(n=sum(weight)/436499.5)  # 0.2665298


HFpEF_only_pats %>% inner_join(Diabetes) %>% summarise(n=sum(weight)) # 1617426
HFpEF_only_pats %>% inner_join(Diabetes) %>% inner_join(SGLT2) %>% summarise(n=sum(weight)/1617426)  # 0.2455239
HFpEF_only_pats %>% inner_join(Diabetes) %>% inner_join(MRA) %>% summarise(n=sum(weight)/1617426)  # 0.2089803
HFpEF_only_pats %>% inner_join(Diabetes) %>% inner_join(ARNI) %>% summarise(n=sum(weight)/1617426)  # 0.02072107
HFpEF_only_pats %>% inner_join(Diabetes) %>% inner_join(GLP1) %>% summarise(n=sum(weight)/1617426)  # 0.2555757


HFpEF_only_pats %>% inner_join(CKD) %>% summarise(n=sum(weight)) # 1493401
HFpEF_only_pats %>% inner_join(CKD) %>% inner_join(SGLT2) %>% summarise(n=sum(weight)/1493401)  # 0.1792704
HFpEF_only_pats %>% inner_join(CKD) %>% inner_join(MRA) %>% summarise(n=sum(weight)/1493401)  # 0.2169727
HFpEF_only_pats %>% inner_join(CKD) %>% inner_join(ARNI) %>% summarise(n=sum(weight)/1493401)  # 0.01846934
HFpEF_only_pats %>% inner_join(CKD) %>% inner_join(GLP1) %>% summarise(n=sum(weight)/1493401)  # 0.1675609


HFpEF_only_pats %>% inner_join(PAD) %>% summarise(n=sum(weight)) # 1450631
HFpEF_only_pats %>% inner_join(PAD) %>% inner_join(SGLT2) %>% summarise(n=sum(weight)/1450631)  # 0.1551909
HFpEF_only_pats %>% inner_join(PAD) %>% inner_join(MRA) %>% summarise(n=sum(weight)/1450631)  # 0.1927294
HFpEF_only_pats %>% inner_join(PAD) %>% inner_join(ARNI) %>% summarise(n=sum(weight)/1450631)  # 0.01640881
HFpEF_only_pats %>% inner_join(PAD) %>% inner_join(GLP1) %>% summarise(n=sum(weight)/1450631)  # 0.145651

HFpEF_only_pats %>% inner_join(Obesity) %>% summarise(n=sum(weight)) # 2006256
HFpEF_only_pats %>% inner_join(Obesity) %>% inner_join(SGLT2) %>% summarise(n=sum(weight)/2006256)  # 0.1862699
HFpEF_only_pats %>% inner_join(Obesity) %>% inner_join(MRA) %>% summarise(n=sum(weight)/2006256)  # 0.2133452
HFpEF_only_pats %>% inner_join(Obesity) %>% inner_join(ARNI) %>% summarise(n=sum(weight)/2006256)  # 0.02006751
HFpEF_only_pats %>% inner_join(Obesity) %>% inner_join(GLP1) %>% summarise(n=sum(weight)/2006256)  # 0.2073337


# --------

# Comorbidities sizing HFrEF ----
HFUS24_Demographics <- fread("HFUS24 Demographics.txt")
sum(HFUS24_Demographics$weight) # 17835035

HFUS24_Demographics %>% filter(deceased==0 | death_dt>="2023-02-01" ) %>% summarise(n=sum(weight)) # 12063530
range(as.Date(HFUS24_Demographics$death_dt), na.rm=T)
range(as.Date(HFUS24_Demographics$HF_1st_Rx), na.rm=T)

HFUS24_Demographics <- HFUS24_Demographics %>% filter(deceased==0 | death_dt>="2023-02-01" )
sum(HFUS24_Demographics$weight) # 12063530

HFUS24_Demographics %>% group_by(HF_type) %>% summarise(pop=sum(weight))
HFUS24_Demographics <- HFUS24_Demographics %>% filter(HF_type %in% c("HFpEF_only", "HFrEF_only"))

# 1 HFpEF_HFrEF 2960111.
# 2 HFpEF_only  3592348.
# 3 HFrEF_only  2013925.
# 4 Unknown     3497146.

Pats_to_track <- HFUS24_Demographics %>% select(patid, weight, HF_type)
Pats_to_track <- Pats_to_track %>% filter(HF_type=="HFrEF_only") %>% select(patid, weight)

HFUS24_Additional_Comorbidities_Dx_events <- fread("HFUS24_Additional_Comorbidities_Dx_events.txt")

Comorb <- HFUS24_Additional_Comorbidities_Dx_events %>% filter(grepl("G45", diag_code)|
                                         grepl("I63", diag_code)|
                                         grepl("H34", diag_code)|
                                         grepl("I65", diag_code)|
                                         grepl("I66", diag_code)|
                                         grepl("I69", diag_code)|
                                         grepl("I20", diag_code)|
                                         grepl("I21", diag_code)|
                                         grepl("I22", diag_code)|
                                         grepl("I23", diag_code)|
                                         grepl("I24", diag_code)|
                                         grepl("I25", diag_code)|
                                         grepl("I70", diag_code)|
                                         grepl("I73", diag_code)) %>% select(patid ) %>% distinct() 

Pats_to_track <- Pats_to_track %>% left_join(Comorb %>% mutate(ASCVD="ASCVD"))


ce18_Heart_Failure_pts_HFCmbdBMI_events <- fread("ce18_Heart_Failure_pts_HFCmbdBMI_events.txt")

ce18_Heart_Failure_pts_HFCmbdBMI_events <- ce18_Heart_Failure_pts_HFCmbdBMI_events %>% group_by(PTID) %>%
  filter(BMI_val  == max(BMI_val )) %>% slice(1) %>% select(PTID, BMI_val ) %>% rename("patid"="PTID")

ce18_Heart_Failure_pts_HFCmbdBMI_events <- ce18_Heart_Failure_pts_HFCmbdBMI_events %>% ungroup() %>% 
  select(patid, BMI_val ) %>% mutate(BMI_val =ifelse(BMI_val >30, ">30",
                                                     ifelse(BMI_val >27,">27", "none"))) %>% 
  select(patid, BMI_val ) %>% distinct()


Pats_to_track <- Pats_to_track %>% left_join(ce18_Heart_Failure_pts_HFCmbdBMI_events)


HFUS24_Comorbidities_with_ICD9_events <- fread("HFUS24_Comorbidities_with_ICD9_events.txt")

Diabetes <- HFUS24_Comorbidities_with_ICD9_events %>% filter(`Type 2 Diabetes With Complications`==1|
                                              `Type 2 Diabetes`==1|
                                              `Secondary Diabetes With Complications`==1|
                                              `Secondary Diabetes`==1|
                                              `Type 1 Diabetes With Complications`==1|
                                              `Type 1 Diabetes`==1|
                                              `General Diabetes With Complications`==1|
                                              `General Diabetes`==1|
                                              `Treatment For Diabetes`==1) %>% select(patid)


Pats_to_track <- Pats_to_track %>% left_join(Diabetes %>% mutate(DIA="DIA"))

Pats_to_track %>%  filter(!is.na(BMI_val)) %>% summarise(n=sum(weight)/1)


Pats_to_track %>% mutate(OBE=ifelse(ASCVD=="ASCVD"&BMI_val %in% c(">30", ">27"),1,0)) %>%
  filter(!is.na(BMI_val)) %>% group_by(OBE, DIA) %>% summarise(n=sum(weight)/931094.7)
