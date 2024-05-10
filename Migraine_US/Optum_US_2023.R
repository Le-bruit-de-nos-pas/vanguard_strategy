library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)


# Pills per month V1 ------------------------

RIMUS23_Doses_2 <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)



RIMUS23_Doses_2  %>%  group_by(drug_class) %>% count()
length(unique(RIMUS23_Doses_2$provcat))

df <- data.frame(unique(RIMUS23_Doses_2$provcat))
names(df)[1] <- "provcat"

fwrite(df, "Unique_provcats.txt")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
head(RIMUS23_Doses)

N_distinct_months <- RIMUS23_Doses %>% select(patid, from_dt) %>% distinct() 
N_distinct_months <- N_distinct_months%>% mutate(from_dt=as.Date(from_dt)) %>%  filter(from_dt >= "2022-05-15") 
N_distinct_months$from_dt <- substr(as.character(N_distinct_months$from_dt), 1, 7)
N_distinct_months <- N_distinct_months %>% distinct()
N_distinct_months <- N_distinct_months %>% group_by(patid) %>% count()
mean(N_distinct_months$n)



RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))
range(RIMUS23_Doses$from_dt)

RIMUS23_Doses %>% filter(generic=="Rimegepant")

RIMUS23_Doses <- RIMUS23_Doses %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid,generic) %>% mutate(total_n_pills = sum(as.numeric(days_sup)))
RIMUS23_Doses <- RIMUS23_Doses %>% ungroup() %>% group_by(patid, generic) %>% mutate(pills_per_month = total_n_pills/12)
RIMUS23_Doses <- RIMUS23_Doses %>% ungroup() %>% left_join(N_distinct_months) %>%
  group_by(patid, generic) %>% mutate(pills_per_active_month = total_n_pills/n)


RIMUS23_Doses %>% ungroup() %>% select(patid, weight, generic, pills_per_month) %>% 
  distinct() %>% mutate(frequency = ifelse(pills_per_month>=15, "Chronic",
                                           ifelse(pills_per_month>=4, "Intermediate", "Acute"))) %>%
  group_by(generic, frequency) %>% summarise(pats = sum(as.numeric(weight))) %>%
  spread(key=generic, value=pats)



RIMUS23_Doses %>% ungroup() %>% select(patid, weight, generic, pills_per_active_month) %>% 
  distinct() %>% mutate(frequency = ifelse(pills_per_active_month>=15, "Chronic",
                                           ifelse(pills_per_active_month>=4, "Intermediate", "Acute"))) %>%
  group_by(generic, frequency) %>% summarise(pats = sum(as.numeric(weight))) %>%
  spread(key=generic, value=pats)



RIMUS23_Doses %>% ungroup() %>% select(patid, weight, generic, pills_per_active_month) %>% 
  distinct() %>% group_by(generic) %>% summarise(mean = weighted.mean(pills_per_active_month, as.numeric(weight))) 



RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid,generic) %>% mutate(total_qty = sum(as.numeric(qty)))
RIMUS23_Doses <- RIMUS23_Doses %>% ungroup() %>% group_by(patid, generic) %>% mutate(qty_per_month = total_qty/12)
RIMUS23_Doses <- RIMUS23_Doses %>% ungroup() %>% left_join(N_distinct_months) %>%
  group_by(patid, generic) %>% mutate(qty_per_active_month = total_qty/n)


RIMUS23_Doses %>% ungroup() %>% select(patid, weight, generic, qty_per_month) %>% 
  distinct() %>% mutate(frequency = ifelse(qty_per_month>=15, "Chronic",
                                           ifelse(qty_per_month>=4, "Intermediate", "Acute"))) %>%
  group_by(generic, frequency) %>% summarise(pats = sum(as.numeric(weight))) %>%
  spread(key=generic, value=pats)



RIMUS23_Doses %>% ungroup() %>% select(patid, weight, generic, qty_per_active_month) %>% 
  distinct() %>% mutate(frequency = ifelse(qty_per_active_month>=15, "Chronic",
                                           ifelse(qty_per_active_month>=4, "Intermediate", "Acute"))) %>%
  group_by(generic, frequency) %>% summarise(pats = sum(as.numeric(weight))) %>%
  spread(key=generic, value=pats)




RIMUS23_Doses %>% ungroup() %>% select(patid, weight, generic, pills_per_month) %>% 
  distinct() %>% group_by(generic) %>% summarise(mean = weighted.mean(pills_per_month, as.numeric(weight))) 


RIMUS23_Doses %>% ungroup() %>% select(patid, weight, generic, pills_per_month) %>% 
  distinct() %>%
  ggplot(aes(pills_per_month, colour=generic, fill=generic))+
  geom_density(alpha=0.5)+
  xlim(0,40)+
  theme(panel.background = element_blank())+
  ylab("Patient Sample Proportion\n")+
  xlab("\nTotal Number of Supply Days per Month") +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama()



# ----------------

# Provcats allocation ------------------------


PROVCAT <- readxl::read_xlsx(path="CDM_Data_Reference_Lookup_Tables_V81.xlsx",sheet = "PROVCAT", skip=5, col_types = "text", trim_ws = TRUE)
PROVCAT$PROVCAT <- as.numeric(PROVCAT$PROVCAT)

Unique_provcats <- fread("Unique_provcats.txt")

Unique_provcats <- PROVCAT %>% inner_join(Unique_provcats, by=c("PROVCAT"="provcat"))

fwrite(Unique_provcats, "Unique_provcats.csv")
Unique_provcats <- fread("Unique_provcats.csv")

# ---------

# Pills per month V2 ----------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")


RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()

data.frame(RIMUS23_Doses) 

RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))




RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>% ungroup() %>%
  ggplot(aes(mean, colour=generic, fill=generic)) +
  geom_density(alpha=0.5) +
  xlim(0,50) +
  theme_minimal() +
  xlab("\n Average treatment rate per month per patient") + ylab("Patient density \n") +
  ggsci::scale_fill_nejm() +
  ggsci::scale_colour_nejm()


RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(generic, mean) %>% summarise(n=sum(as.numeric(weight)))



# ----------------------------------------------

# Sizing -----------------
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- RIMUS23_Doses %>% select(drug_id, generic, drug_class, drug_group) %>% distinct()
Drugs_lookup <- Drugs_lookup %>% mutate(drug_id=as.numeric(drug_id)) %>% arrange(drug_id)

fwrite(Drugs_lookup, "Drugs_lookup.csv")

ce18_migpts <- read.table("ce18_migpts.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(ce18_migpts$weight))


Drugs_lookup <- fread("Drugs_lookup.csv")
data.frame(Drugs_lookup)

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories %>% filter(!grepl("136", month59)&grepl("136", month60)) 

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))

RIMUS23_Drug_Histories %>% filter(Month>=49) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))

# -------------------


# Scripts & Patients over time -----------------------------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt)

setDT(RIMUS23_Doses)[, Month_Yr := format(as.Date(from_dt), "%Y-%m") ]
RIMUS23_Doses <- RIMUS23_Doses %>% select(-from_dt)

RIMUS23_Doses %>% 
  filter(Month_Yr>="2018-06"&Month_Yr!="2023-07"&Month_Yr!="2023-06") %>%
  group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Scripts \n")


RIMUS23_Doses %>% 
    filter(Month_Yr>="2018-06"&Month_Yr!="2023-07"&Month_Yr!="2023-06") %>%
  select(patid, weight, Month_Yr) %>% distinct() %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Patients \n")


data.frame(RIMUS23_Doses %>%   filter(Month_Yr>="2018-06"&Month_Yr!="2023-07"&Month_Yr!="2023-06") %>%
             select(patid,weight, Month_Yr) %>% group_by(Month_Yr) %>% mutate(script_count = sum(as.numeric(weight))) %>% ungroup() %>%
             select(patid, weight, Month_Yr, script_count) %>% distinct() %>% group_by(Month_Yr) %>% mutate(pat_count = sum(as.numeric(weight))) %>% 
             select(Month_Yr,script_count, pat_count) %>% distinct() %>% arrange(Month_Yr) %>% mutate(scripts_pat = script_count/pat_count)) %>%
  ggplot(aes(x=Month_Yr, y=scripts_pat))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Scripts per Patient \n")



df <-  data.frame(RIMUS23_Doses %>%   filter(Month_Yr>="2018-06"&Month_Yr!="2023-06"&Month_Yr!="2023-07") %>%
             select(patid,weight, Month_Yr) %>% group_by(Month_Yr) %>% mutate(script_count = sum(as.numeric(weight))) %>% ungroup() %>%
             select(patid, weight, Month_Yr, script_count) %>% distinct() %>% group_by(Month_Yr) %>% mutate(pat_count = sum(as.numeric(weight))) %>% 
             select(Month_Yr,script_count, pat_count) %>% distinct() %>% arrange(Month_Yr) %>% mutate(scripts_pat = script_count/pat_count))

df

library("zoo")        
data.frame(rollmean(df$pat_count   , k = 6) )

# --------------------
# Class penetrance month over month --------------------
Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_class) %>% distinct()

df <- data.frame(RIMUS23_Drug_Histories %>% group_by(Month, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=drug_class, value=n))


fwrite(df, "df.csv")

# -----------
# Classes ever tried  --------------------
Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()

data.frame(RIMUS23_Drug_Histories %>% group_by(drug_class) %>% summarise(n=sum(as.numeric(weight))))

# -------------
# Concomitant classes with Rimegepant ----------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <-RIMUS23_Drug_Histories %>% select(patient, weight, month60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(month60 != "-")  %>% distinct() %>% filter(grepl("136", month60))
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, month60, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("month60"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()
RIMUS23_Drug_Histories$Exp <- 1

RIMUS23_Drug_Histories %>% group_by(drug_class) %>% summarise(tot=sum(as.numeric(weight))/203969)

203968.9

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_class, value=Exp)

RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

sum(as.numeric(RIMUS23_Drug_Histories$weight))


# ----------------------
# Concomitant classes with Ubrogepant ----------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <-RIMUS23_Drug_Histories %>% select(patient, weight, month60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(month60 != "-")  %>% distinct() %>% filter(grepl("137", month60))
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, month60, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("month60"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()
RIMUS23_Drug_Histories$Exp <- 1

data.frame(RIMUS23_Drug_Histories %>% group_by(drug_class) %>% summarise(tot=sum(as.numeric(weight)/209124))) %>%
  arrange(-tot)


RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_class, value=Exp)

RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 209123.5



# ------------
# Pills per month V2 over time ---------------------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")


RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()

data.frame(RIMUS23_Doses) 

RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))


RIMUS23_Doses$from_dt <- substr(as.character(RIMUS23_Doses$from_dt), 1, 7)

data.frame(RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
             filter(from_dt!="2023-07"&from_dt!="2023-06") %>%
  group_by(patid, weight, generic, from_dt) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(from_dt, generic, mean) %>% summarise(n=sum(as.numeric(weight))) %>%
    ungroup() %>%
  spread(key=mean, value=n)) %>%
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Acute_Proportion=100*Acute/(Acute+Prev)) %>%
  ggplot(aes(from_dt, Acute_Proportion, colour=generic     )) +
  geom_point() +
 # facet_wrap(~generic) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("\n Month") + ylab("Proportion of Acute patient scripts \n")





# --------------
# Mild vs ModSev -----------------------

Drugs_lookup <- fread("Drugs_lookup.csv")
data.frame(Drugs_lookup)

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup, by=c("Treat"="drug_id"))
unique(RIMUS23_Drug_Histories$drug_group)

Advanced_Rx_pats <- RIMUS23_Drug_Histories %>% 
  filter(drug_group=="Triptans"|drug_group=="CGRP Injectable"|drug_group=="CGRP Oral") %>%
  select(patient) %>% distinct()

Prev_Sympt_pats <- RIMUS23_Drug_Histories %>% select(patient, generic, drug_class, drug_group) %>% distinct() %>% anti_join(Advanced_Rx_pats)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, migraine_last_dx) %>% mutate(migraine_last_dx=as.Date(migraine_last_dx))
RIMUS23_Demographics <- RIMUS23_Demographics %>% drop_na() 
range(RIMUS23_Demographics$migraine_last_dx)
RIMUS23_Demographics <- RIMUS23_Demographics %>% filter(migraine_last_dx>"2022-05-15") %>% select(patid)
Dx_LY_pats <- RIMUS23_Demographics
names(Dx_LY_pats)[1] <- "patient"

ModHigh_Severe <- Dx_LY_pats %>% full_join(Advanced_Rx_pats)

Prev_Sympt_pats <- Prev_Sympt_pats %>% anti_join(Dx_LY_pats)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, CV, psychiatric, epileptic)
names(RIMUS23_Demographics)[1] <- "patient"

Prev_Sympt_pats <- Prev_Sympt_pats %>% left_join(RIMUS23_Demographics)
unique(Prev_Sympt_pats$drug_class)

Prev_Sympt_pats <- Prev_Sympt_pats %>% mutate(to_maintain=ifelse(drug_group=="Symptomatic", 1, 0))

Prev_Sympt_pats <- Prev_Sympt_pats %>% mutate(to_maintain=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,to_maintain)))))))))

Prev_Sympt_pats <- Prev_Sympt_pats %>% anti_join(Prev_Sympt_pats %>% filter(to_maintain==0) %>% select(patient) %>% distinct()) 

Mild_pats <- Prev_Sympt_pats %>% select(patient) %>% distinct()


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
Naive_pats <- RIMUS23_Drug_Histories %>% anti_join(RIMUS23_Drug_Histories %>% filter(Treat != "-") %>% select(patient) %>% distinct())
unique(Naive_pats$Treat) 
Mild_pats <- Naive_pats %>% select(patient) %>% distinct() %>% full_join(Mild_pats)


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- RIMUS23_Drug_Histories %>% select(patient, weight)
sum(as.numeric(All_pats$weight))

All_pats <- All_pats %>% left_join(Mild_pats %>% mutate(group="Mild"))
All_pats %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))

All_pats <- All_pats %>% mutate(group=ifelse(is.na(group), "ModSev", group))

fwrite(All_pats, "ModSev_vector.txt")

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(ModSev_vector)

data.frame(RIMUS23_Drug_Histories %>% group_by(group, drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>% spread(key=group, value=n) %>%
  mutate(Mild=ifelse(is.na(Mild),0,Mild)) %>%
  mutate(Mild=Mild/9322738., ModSev=ModSev/11856517.)



RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_class) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(ModSev_vector) %>% group_by(group, drug_class, patient, weight) %>% count()

data.frame(RIMUS23_Drug_Histories %>% ungroup() %>% group_by(group, drug_class) %>% summarise(mean=weighted.mean(n, as.numeric(weight))) %>%
             spread(key=group, value=mean))


# ---------------------

# Class Penetrance vs Duration across last 12 month period --------------------------------------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
ModSev_vector %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))
ModSev_vector <- ModSev_vector %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()

data.frame(RIMUS23_Drug_Histories %>% inner_join(ModSev_vector) %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 11856517)*100)) %>% arrange(-sum_weights_percent)



RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_class) %>% distinct()


ModSev_vector %>% mutate(link=1) %>%
  full_join(Drugs_lookup %>% select(drug_class) %>% distinct() %>% mutate(link=1)) %>%
  select(-link) %>%
  left_join(RIMUS23_Drug_Histories %>% inner_join(ModSev_vector) %>% group_by(patient, drug_class) %>% count()) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n))


data.frame(RIMUS23_Drug_Histories %>% inner_join(ModSev_vector) %>% group_by(patient, drug_class) %>% count() %>%
  ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n)))



# Penetration
Penetrance_vs_Duration <- read.csv("Penetrance_vs_Duration.csv")
names(Penetrance_vs_Duration) <- c("drug_class", "penetrance", "duration")

library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(Penetrance_vs_Duration, aes(x=penetrance, y=duration, size = penetrance, fill=penetrance, colour=penetrance)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = drug_class), 
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
  xlab("\n12-month Penetrance (% Population)")+
  ylab("Weighted Average Duration (months)\n")+
  ylim(0,11)+ xlim(0,50)

# --------------------

# Treatment rates pr class -------------------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
ModSev_vector %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))
ModSev_vector <- ModSev_vector %>% filter(group=="ModSev") %>% select(patient)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")

RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, drug_class, generic, from_dt) 

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, drug_class, generic,) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()

data.frame(RIMUS23_Doses) 

RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(days_sup)/elapsed))


RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, drug_class, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))

RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(days_sup)/elapsed))

data.frame(RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, drug_class) %>% summarise(mean=mean(rate)) %>%
  drop_na() %>% ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(mean)))

# ------------------
# Waterfall stocks and experience -----------------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
ModSev_vector %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))
sum(as.numeric(ModSev_vector$weight))

RIMUS23_Box_Histories <- read.table("RIMUS23 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Box_Histories <- RIMUS23_Box_Histories %>% select(patient, weight, month60)

RIMUS23_Box_Histories <- ModSev_vector %>% left_join(RIMUS23_Box_Histories)

RIMUS23_Box_Histories %>% filter(group=="ModSev") %>% group_by(month60) %>% summarise(n=sum(as.numeric(weight)))


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
Lines <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()

RIMUS23_Box_Histories %>% left_join(Lines) %>% summarise(n=weighted.mean(n, as.numeric(weight)))
RIMUS23_Box_Histories %>% left_join(Lines) %>% group_by(group) %>% summarise(n=weighted.mean(n, as.numeric(weight)))
RIMUS23_Box_Histories %>% left_join(Lines) %>% filter(group=="ModSev") %>% group_by(month60) %>% summarise(n=weighted.mean(n, as.numeric(weight)))


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month==60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
Mols <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()



RIMUS23_Box_Histories %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% summarise(n=weighted.mean(n, as.numeric(weight)))
RIMUS23_Box_Histories %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% group_by(group) %>% summarise(n=weighted.mean(n, as.numeric(weight)))
RIMUS23_Box_Histories %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% filter(group=="ModSev") %>% group_by(month60) %>% summarise(n=weighted.mean(n, as.numeric(weight)))



RIMUS23_Box_Histories %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n=ifelse(n<2,0,1)) %>% group_by(n) %>% summarise(pats=sum(as.numeric(weight)))
RIMUS23_Box_Histories %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n=ifelse(n<2,0,1)) %>% group_by(group, n) %>% summarise(pats=sum(as.numeric(weight)))
RIMUS23_Box_Histories %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n=ifelse(n<2,0,1)) %>% filter(group=="ModSev") %>% group_by(month60, n) %>% summarise(pats=sum(as.numeric(weight)))



RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month==60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
Drugs_lookup <- fread("Drugs_lookup.csv")
unique(Drugs_lookup$drug_group)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup , by=c("Treat"="drug_id")) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()

RIMUS23_Box_Histories %>% left_join(RIMUS23_Drug_Histories) %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))


RIMUS23_Box_Histories %>% left_join(RIMUS23_Drug_Histories) %>%
  group_by(group, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% filter(group=="Mild")


RIMUS23_Box_Histories %>% left_join(RIMUS23_Drug_Histories) %>%
  filter(group=="ModSev") %>%
  group_by(month60, drug_group) %>% summarise(n=sum(as.numeric(weight)))  %>%
  spread(key=month60, value=n)


# -----------------
# Max stock and ORAL CGRP use# Physician Profile  -----------------------


ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
ModSev_vector %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))
sum(as.numeric(ModSev_vector$weight))


RIMUS23_Box_Histories <- read.table("RIMUS23 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Box_Histories <- RIMUS23_Box_Histories %>% select(patient, weight, month49:month60)
RIMUS23_Box_Histories <- gather(RIMUS23_Box_Histories, Month, Box, month49:month60, factor_key=TRUE)
RIMUS23_Box_Histories <- RIMUS23_Box_Histories %>% mutate(Stock=ifelse(Box=="N",0,
                                              ifelse(Box=="X",1,
                                                     ifelse(Box=="S",2,
                                                            ifelse(Box=="T",3,
                                                                   ifelse(Box=="P",4,
                                                                          ifelse(Box=="Y",5,
                                                                                 ifelse(Box=="K",6,
                                                                                        ifelse(Box=="I",7,8)))))))))

RIMUS23_Box_Histories <- RIMUS23_Box_Histories %>% select(-Month) %>% group_by(patient, weight) %>% filter(Stock ==max(Stock )) %>% slice(1)


RIMUS23_Box_Histories %>% left_join(ModSev_vector) %>%
  group_by(group, Box) %>%
  summarise(n=sum(as.numeric(weight)))


Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic, drug_class) %>% distinct() %>% filter(drug_class=="CGRP Oral")


RIMUS23_Box_Histories %>% left_join(ModSev_vector) %>%
  inner_join(RIMUS23_Drug_Histories %>% filter(generic=="Rimegepant") %>% select(patient) %>% distinct()) %>%
  group_by(group, Box) %>%
  summarise(n=sum(as.numeric(weight)))

# ------------------
# Physician profile ----------------
Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider , provcat, drug_group) %>% distinct()
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)


RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date   =as.Date(date)) %>% filter(date>="2022-06-01"&date<="2023-05-31")
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(provider, provcat) %>% distinct()
RIMUS23_Migraine_Dxs$Dx <- 1


RIMUS23_Migraine_Dxs %>% group_by(provcat) %>% count() %>% arrange(-n)


All_provs <- RIMUS23_Doses %>% select(provider, provcat) %>% full_join(RIMUS23_Migraine_Dxs %>% select(provider, provcat)) %>% distinct() %>%
  left_join(RIMUS23_Migraine_Dxs %>% select(-provcat)) %>% left_join(RIMUS23_Doses %>% select(-provcat))

All_provs[is.na(All_provs)] <- 0

All_provs <- All_provs %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

All_provs <- All_provs %>% filter(TYPE!="EXCLUDE") %>% drop_na()

All_provs <- All_provs %>% mutate(Physician_Profile=ifelse(Dx==1&`CGRP Injectable`==0&`CGRP Oral`==0&Preventive==0&Symptomatic==0&Triptans==0, "Dx_Only",
                                              ifelse(`CGRP Injectable`==1|`CGRP Oral`==1, "Rx_CGRP",
                                                     ifelse(Preventive==1, "Rx_Preventive", 
                                                            ifelse(Symptomatic==1|Triptans==1, "Rx_Acute", "none")))))

All_provs %>% group_by(Physician_Profile) %>% count() %>% mutate(n=n/321254)

All_provs %>% group_by(TYPE) %>% count()

All_provs %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) %>%
  group_by( TYPE, Physician_Profile) %>% count() %>% 
  mutate(n=ifelse(TYPE=="INTERNAL MEDICINE", n/46602,
                  ifelse(TYPE=="NEUROLOGY", n/18130,
                         ifelse(TYPE=="OBG", n/6611,
                                ifelse(TYPE=="OTHER HCP", n/55027,
                                       ifelse(TYPE=="OTHER PHYSICIAN", n/106189,
                                              ifelse(TYPE=="PRIMARY CARE", n/79862,
                                                     ifelse(TYPE=="PSYCHIATRY",n/8833,NA)))))))) %>%
  spread(key=TYPE, value=n)




# ----------- 
# Rimegepant patients waterfall ON May 2023 --------------
Drugs_lookup <- fread("Drugs_lookup.csv")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, month60) 

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(grepl("136", month60))

sum(as.numeric(RIMUS23_Drug_Histories$weight))

RIMUS23_Drug_Histories$Rimegepant <- 1

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Combo=ifelse(grepl(",", month60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Mabs=ifelse(grepl(string_Mabs, month60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Acute=ifelse(grepl(string_Acute, month60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Symptomatic=ifelse(grepl(string_Symptomatic, month60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Preventive=ifelse(grepl(string_Preventive, month60),1,0))

RIMUS23_Drug_Histories %>% 
  mutate(AcuteSympt=ifelse(Acute==1|Symptomatic==1,1,0)) %>%
  group_by(Rimegepant, Combo, Mabs,  Preventive, AcuteSympt) %>%
  summarise(n=sum(as.numeric(weight)))


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(generic, mean) %>% summarise(n=sum(as.numeric(weight)))

Rimegepant_rate <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% filter(generic=="Rimegepant") %>%
  select(patid, mean)


RIMUS23_Drug_Histories %>% 
  left_join(Rimegepant_rate, by=c("patient"="patid")) %>%
  group_by(mean)  %>%
  summarise(n=sum(as.numeric(weight)))

RIMUS23_Drug_Histories %>% 
  left_join(Rimegepant_rate, by=c("patient"="patid")) %>%
  group_by(Rimegepant, mean, Combo, Preventive, Mabs) %>%
  summarise(n=sum(as.numeric(weight))) %>%
  drop_na() %>%
  mutate(n=ifelse(mean=="Acute",n/131219,n/51917))

# --------------------------

# Oral CGRP patients where are they ON May 2023 --------------
Drugs_lookup <- fread("Drugs_lookup.csv")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, month60) 

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(grepl("136", month60)|grepl("137", month60)|grepl("135", month60))

sum(as.numeric(RIMUS23_Drug_Histories$weight))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Rimegepant=ifelse(grepl("136", month60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Ubrogepant=ifelse(grepl("137", month60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Atogepant=ifelse(grepl("135", month60),1,0))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Combo=ifelse(grepl(",", month60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Mabs=ifelse(grepl(string_Mabs, month60),1,0))
# RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Acute=ifelse(grepl(string_Acute, month60),1,0))
# RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Symptomatic=ifelse(grepl(string_Symptomatic, month60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Preventive=ifelse(grepl(string_Preventive, month60),1,0))

RIMUS23_Drug_Histories %>% 
  filter(Ubrogepant==1) %>%
  group_by(Combo, Mabs, Preventive) %>%
  summarise(n=sum(as.numeric(weight)))

# --------------------
# Oral CGRP patients per brand over time --------------

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat==135|Treat==136|Treat==137)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, Treat) %>% distinct()

df <- data.frame(RIMUS23_Drug_Histories %>% group_by(Month, Treat) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=Treat, value=n))


# --------------------
# Inflows and Outflows to rimegepant --------------
RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 

RIMUS23_Box_Histories <- read.table("RIMUS23 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Box_Histories <- gather(RIMUS23_Box_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Box_Histories$Month <- parse_number(as.character(RIMUS23_Box_Histories$Month))
RIMUS23_Box_Histories <- RIMUS23_Box_Histories %>% select(-disease)
RIMUS23_Box_Histories <- RIMUS23_Box_Histories %>% filter(Month>=49) 


RIMUS23_Drug_Histories %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl("136", Treat)&grepl("136", lead(Treat))) %>%
  select(patient, Month, weight) %>% distinct() %>%
  left_join(RIMUS23_Box_Histories %>% select(patient, Month, Treat, weight)) %>%
  group_by(Treat) %>% summarise(n=sum(as.numeric(weight)))


RIMUS23_Drug_Histories %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl("136", Treat)&grepl("136", lag(Treat))) %>%
  select(patient, Month, weight) %>% distinct() %>%
  left_join(RIMUS23_Box_Histories %>% select(patient, Month, Treat, weight)) %>%
  group_by(Treat) %>% summarise(n=sum(as.numeric(weight)))



RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, CV)
names(RIMUS23_Demographics)[1] <- "patient"


# RIMUS23_Drug_Histories %>% filter(!grepl("136", month59)&grepl("136", month60)) %>%
#   select(patient, weight) %>% distinct() %>%
#   inner_join(RIMUS23_Demographics %>% filter(CV==1)) %>%
#   left_join(RIMUS23_Box_Histories %>% select(patient, month59)) %>%
#   group_by(month59) %>% summarise(n=sum(as.numeric(weight)))
# 
# 
# 
# RIMUS23_Drug_Histories %>% filter(grepl("136", month59)&!grepl("136", month60)) %>%
#   select(patient, weight) %>% distinct() %>%
#   inner_join(RIMUS23_Demographics %>% filter(CV==1)) %>%
#   left_join(RIMUS23_Box_Histories %>% select(patient, month60)) %>%
#   group_by(month60) %>% summarise(n=sum(as.numeric(weight)))

# -------------

# Generate long flows table --------------
MIG_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- read.table("RIMUS23 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

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
flMIG <- flMIG[, disease := "MIG US"]
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

fwrite(flMIG,"MIG_Flows_Aux._Long_v2.txt")

flMIG %>% filter(p1>=49) %>% filter(grepl("136", d1) & !grepl("136", d2)) %>%
  summarise(n=sum(as.numeric(weight)))

# ------------------

# Proportion of preventive patients with comorbdiity explaining it --------------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup, by=c("Treat"="drug_id"))

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, CV, psychiatric, epileptic)
names(RIMUS23_Demographics)[1] <- "patient"

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(RIMUS23_Demographics)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(to_flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))


RIMUS23_Box_Histories <- read.table("RIMUS23 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Box_Histories <- RIMUS23_Box_Histories %>% select(patient, weight, month60)

ModSev_vector %>% filter(group=="ModSev") %>% left_join(RIMUS23_Box_Histories) %>%
  filter(month60=="P"|month60=="Y"|month60=="K") %>% summarise(n=sum(as.numeric(weight)))  # 6756160


ModSev_vector %>% filter(group=="ModSev") %>% left_join(RIMUS23_Box_Histories) %>%
  inner_join(RIMUS23_Drug_Histories %>% filter(to_flag==1) %>% select(patient) %>% distinct()) %>%
  filter(month60=="P"|month60=="Y"|month60=="K") %>% summarise(n=sum(as.numeric(weight)))  # 5364004

# ---------------------------
# Inflows to Rimegepant / Ubrogepant based on cummulative experience -----------------
Drugs_lookup <- fread("Drugs_lookup.csv")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")

flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")

flMIG <- flMIG %>% select(-c(disease, p1_RxExp, flow, stops, starts, re_starts)) %>% arrange(patient, p1, p2)

flMIG <- flMIG %>% group_by(patient) %>% mutate(Rime_exp=cumsum(grepl("136", d1))) %>% mutate(Rime_exp=ifelse(Rime_exp==0,0,1))




flMIG <- flMIG %>% group_by(patient) %>% mutate(Ubro_exp=cumsum(grepl("135", d1))) %>% mutate(Ubro_exp=ifelse(Ubro_exp==0,0,1))
flMIG <- flMIG %>% group_by(patient) %>% mutate(Oral_exp=cumsum(grepl("135", d1)|grepl("136", d1)|grepl("137", d1))) %>%
  mutate(Oral_exp=ifelse(Oral_exp==0,0,1))
flMIG <- flMIG %>% group_by(patient) %>% mutate(Mabs_exp=cumsum(grepl(string_Mabs, d1))) %>%
  mutate(Mabs_exp=ifelse(Mabs_exp==0,0,1))

flMIG %>%
  filter(p1>=49)  %>% filter(!grepl("136", d1) & grepl("136", d2)) %>%
#  group_by(Rime_exp, Oral_exp, Mabs_exp) %>%
  summarise(n=sum(as.numeric(weight)))

flMIG %>%
  filter(p1>=49)  %>% filter(!grepl("137", d1) & grepl("137", d2)) %>%
  group_by(Ubro_exp, Oral_exp, Mabs_exp) %>%
  summarise(n=sum(as.numeric(weight)))

# ---------------
# Do acute/prev patients stay the same ? -------------------------
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")


RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()

data.frame(RIMUS23_Doses) 

RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

range(RIMUS23_Doses$from_dt)




RIMUS23_Doses %>% filter(from_dt<="2022-06-01") %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(generic, mean) %>% summarise(n=sum(as.numeric(weight)))




RIMUS23_Doses %>% filter(from_dt<="2022-06-01") %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>%
  left_join(
    RIMUS23_Doses %>% filter(from_dt>"2022-06-01") %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean_2=mean(rate)) %>%
  mutate(mean_2=ifelse(mean_2>=13, "Prev", "Acute")) %>%
  ungroup()
  ) %>% drop_na() %>%
  group_by(generic, mean, mean_2) %>%
  summarise(n=sum(as.numeric(weight)))
# 



# ---------------------------
# Scripts per physician -----------------------

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G")
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt, provcat, drug_group)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% 
  group_by(patid, drug_group) %>% filter(from_dt==min(from_dt)) %>% slice(1) %>% ungroup()

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

unique(RIMUS23_Doses$TYPE)

RIMUS23_Doses <- RIMUS23_Doses %>% filter(TYPE!="EXCLUDE") %>% drop_na()

unique(RIMUS23_Doses$drug_group)

RIMUS23_Doses %>% filter(drug_group=="CGRP Injectable") %>%
  summarise(n=sum(as.numeric(weight))/4623586) %>%
  arrange(-n)
  

RIMUS23_Doses <- RIMUS23_Doses %>% filter(grepl("CGRP", drug_group)) %>%
  group_by(patid, drug_group) %>% filter(from_dt==min(from_dt)) %>% slice(1) %>% ungroup()


RIMUS23_Doses %>%  group_by(TYPE) %>%
  summarise(n=sum(as.numeric(weight))/1981978) %>%
  arrange(-n)
  

# ---------------------------
# Scripts per physician over time -----------------------

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G"&generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt, provcat)
setDT(RIMUS23_Doses)[, Month_Yr := format(as.Date(from_dt), "%Y-%m") ]
RIMUS23_Doses <- RIMUS23_Doses %>% select(-from_dt)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Doses <- RIMUS23_Doses %>% filter(TYPE!="EXCLUDE") %>% drop_na()

data.frame(RIMUS23_Doses %>%  group_by(Month_Yr, TYPE) %>%
  summarise(n=sum(as.numeric(weight))) %>%
  spread(key=TYPE, value=n))
  

# -----------
# Share of brand per physician specialty  -----------------------

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt, generic, provider , provcat)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Doses <- RIMUS23_Doses %>% filter(TYPE!="EXCLUDE") %>% drop_na()


RIMUS23_Doses %>% select(provider, TYPE) %>% distinct() %>%
  group_by(TYPE) %>% count() %>% rename("TOTAL"="n") %>%
  left_join(
    RIMUS23_Doses %>% filter(generic=="Rimegepant") %>% select(provider, TYPE) %>% distinct() %>%
  group_by(TYPE) %>% count() %>% rename("Rime"="n") 
  ) %>%
    left_join(
    RIMUS23_Doses %>% filter(generic=="Ubrogepant") %>% select(provider, TYPE) %>% distinct() %>%
  group_by(TYPE) %>% count() %>% rename("Ubro"="n") 
  ) %>%
    left_join(
    RIMUS23_Doses %>% filter(generic=="Atogepant") %>% select(provider, TYPE) %>% distinct() %>%
  group_by(TYPE) %>% count() %>% rename("Ato"="n") 
  )



# -----------
# Physician Profile  -----------------------

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider , provcat, drug_group) %>% distinct()
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)


RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date   =as.Date(date)) %>% filter(date>="2022-06-01"&date<="2023-05-31")
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(provider, provcat) %>% distinct()
RIMUS23_Migraine_Dxs$Dx <- 1


RIMUS23_Migraine_Dxs %>% group_by(provcat) %>% count() %>% arrange(-n)


All_provs <- RIMUS23_Doses %>% select(provider, provcat) %>% full_join(RIMUS23_Migraine_Dxs %>% select(provider, provcat)) %>% distinct() %>%
  left_join(RIMUS23_Migraine_Dxs %>% select(-provcat)) %>% left_join(RIMUS23_Doses %>% select(-provcat))

All_provs[is.na(All_provs)] <- 0

All_provs <- All_provs %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

All_provs <- All_provs %>% filter(TYPE!="EXCLUDE") %>% drop_na()

All_provs <- All_provs %>% mutate(Physician_Profile=ifelse(Dx==1&`CGRP Injectable`==0&`CGRP Oral`==0&Preventive==0&Symptomatic==0&Triptans==0, "Dx_Only",
                                              ifelse(`CGRP Injectable`==1|`CGRP Oral`==1, "Rx_CGRP",
                                                     ifelse(Preventive==1, "Rx_Preventive", 
                                                            ifelse(Symptomatic==1|Triptans==1, "Rx_Acute", "none")))))


All_provs %>% group_by(Physician_Profile) %>% count() %>% mutate(n=n/321254)

All_provs %>% group_by(TYPE) %>% count()

All_provs %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) %>%
  group_by( TYPE, Physician_Profile) %>% count() %>% 
  mutate(n=ifelse(TYPE=="INTERNAL MEDICINE", n/46602,
                  ifelse(TYPE=="NEUROLOGY", n/18130,
                         ifelse(TYPE=="OBG", n/6611,
                                ifelse(TYPE=="OTHER HCP", n/55027,
                                       ifelse(TYPE=="OTHER PHYSICIAN", n/106189,
                                              ifelse(TYPE=="PRIMARY CARE", n/79862,
                                                     ifelse(TYPE=="PSYCHIATRY",n/8833,NA)))))))) %>%
  spread(key=TYPE, value=n)




# -----------
# Persistency curves --------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic, drug_class, drug_group), by=c("Treat"="drug_id"))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, generic, drug_class, drug_group) %>% count()
names(RIMUS23_Drug_Histories)[6] <- "Time"

data.frame(RIMUS23_Drug_Histories %>% ungroup() %>% filter(drug_class=="CGRP Oral"|drug_class=="CGRP Injectable"|drug_class=="Triptan") %>%
  group_by(generic, Time) %>% summarise(pats=sum(as.numeric(weight))) %>%
  spread(key=generic,value=pats))


# ----------
# Persistency curves month 36 onwards --------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic, drug_class, drug_group), by=c("Treat"="drug_id"))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter( Month>36)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, generic, drug_class, drug_group) %>% count()
names(RIMUS23_Drug_Histories)[6] <- "Time"

data.frame(RIMUS23_Drug_Histories %>% ungroup() %>% filter(drug_class=="CGRP Oral"|drug_class=="CGRP Injectable"|drug_class=="Triptan") %>%
  group_by(generic, Time) %>% summarise(pats=sum(as.numeric(weight))) %>%
  spread(key=generic,value=pats))



# --------
# Persistency cur
ves month 36 onwards acute vs preventive pats --------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic, drug_class, drug_group), by=c("Treat"="drug_id"))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter( Month>36)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, generic, drug_class, drug_group) %>% count()
names(RIMUS23_Drug_Histories)[6] <- "Time"

data.frame(RIMUS23_Drug_Histories %>% ungroup() %>% filter(drug_class=="CGRP Oral"|drug_class=="CGRP Injectable"|drug_class=="Triptan") %>%
  group_by(generic, Time) %>% summarise(pats=sum(as.numeric(weight))) %>%
  spread(key=generic,value=pats))



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 


data.frame(RIMUS23_Drug_Histories %>% ungroup() %>% filter(drug_class=="CGRP Oral") %>%
             select(patient, weight, generic, Time) %>% 
             left_join(RIMUS23_Doses %>% select(patid, generic, mean), by=c("patient"="patid", "generic"="generic")) %>%
  mutate(mean=ifelse(is.na(mean), "Acute", mean)) %>%
             group_by(mean, generic, Time) %>% summarise(pats=sum(as.numeric(weight))) %>%
  spread(key=generic,value=pats))


# --------
# Marimekko acute vs prev , patients & volume -------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses %>% select(patid) %>% group_by(patid) %>% count() %>% filter(n>1)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[2] <- "OralAcute"
names(RIMUS23_Doses)[3] <- "OralPrev"


ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories <- ModSev_vector %>% filter(group=="ModSev") %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
Experience <- RIMUS23_Drug_Histories

Experience <- Experience %>% left_join(RIMUS23_Doses, by=c("patient"="patid"))

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 

Rimegepan_patst <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% select(patient) %>% distinct()
Rimegepan_patst <- Rimegepan_patst %>% mutate(Rimegepant=1)

Experience <- Experience %>% left_join(Rimegepan_patst) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))
Experience[is.na(Experience)] <- "-"

Experience %>% group_by(OralAcute, OralPrev) %>% summarise(n=sum(as.numeric(weight)))
Experience  <- Experience %>% mutate(OralAcute=ifelse(OralPrev==1&OralAcute==1,0,OralAcute))
Experience <- Experience %>% mutate(OralAcute=ifelse(OralAcute=="-"&`CGRP Oral`==1, "1", OralAcute))

sum(as.numeric(Experience$weight))



data.frame(Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  group_by(Type, Rimegepant, `CGRP Oral`, `CGRP Injectable`, ) %>%
   # group_by(Type) %>%
  summarise(n=sum(as.numeric(weight))))


Experience <- Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  select(patient, weight, Type)


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-05-15"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, generic, drug_class, drug_group)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(class=ifelse(generic=="Rimegepant", "Rimegepant",
                                      ifelse(drug_class=="CGRP Oral", "CGRP Oral",
                                             ifelse(drug_class=="CGRP Injectable", "CGRP Injectable", drug_group))))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, class)
names(RIMUS23_Doses)[1] <- "patient"
unique(RIMUS23_Doses$class)

Experience <- Experience %>% left_join(RIMUS23_Doses) %>% drop_na() %>%
  filter(Type=="Prev") %>% mutate(Volume=as.numeric(weight)*as.numeric(days_sup)) %>%
  select(-c(weight, days_sup))

Experience <- Experience %>% select(-Type)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(generic=ifelse(generic=="Rimegepant", generic, "CGRP Oral")) %>% select(patid, generic, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[3] <- "OralAcute"
names(RIMUS23_Doses)[4] <- "OralPrev"
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(OralAcute=ifelse(OralPrev==1,0,OralAcute))
names(RIMUS23_Doses)[2] <- "class"
names(RIMUS23_Doses)[1] <- "patient"

Experience <- Experience %>% left_join(RIMUS23_Doses)
Experience[is.na(Experience)] <- 0

Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
  group_by(group, class) %>%  summarise(total=sum(as.numeric(Volume)))



# ------------
# Acute vs prev , patients & volume per specialty ---------------
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses %>% select(patid) %>% group_by(patid) %>% count() %>% filter(n>1)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[2] <- "OralAcute"
names(RIMUS23_Doses)[3] <- "OralPrev"


ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(disease, Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories <- ModSev_vector %>% filter(group=="ModSev") %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
Experience <- RIMUS23_Drug_Histories

Experience <- Experience %>% left_join(RIMUS23_Doses, by=c("patient"="patid"))

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 

Rimegepan_patst <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% select(patient) %>% distinct()
Rimegepan_patst <- Rimegepan_patst %>% mutate(Rimegepant=1)

Experience <- Experience %>% left_join(Rimegepan_patst) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))
Experience[is.na(Experience)] <- "-"

Experience %>% group_by(OralAcute, OralPrev) %>% summarise(n=sum(as.numeric(weight)))
Experience  <- Experience %>% mutate(OralAcute=ifelse(OralPrev==1&OralAcute==1,0,OralAcute))
Experience <- Experience %>% mutate(OralAcute=ifelse(OralAcute=="-"&`CGRP Oral`==1, "1", OralAcute))

sum(as.numeric(Experience$weight))



data.frame(Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  group_by(Type, Rimegepant, `CGRP Oral`, `CGRP Injectable`, ) %>%
   # group_by(Type) %>%
  summarise(n=sum(as.numeric(weight))))


Experience <- Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  select(patient, weight, Type)

unique(Experience$Type)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-05-15"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, generic, drug_class, drug_group, provider, provcat)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(class=ifelse(generic=="Rimegepant", "Rimegepant",
                                      ifelse(drug_class=="CGRP Oral", "CGRP Oral",
                                             ifelse(drug_class=="CGRP Injectable", "CGRP Injectable", drug_group))))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, class, provider, provcat)
names(RIMUS23_Doses)[1] <- "patient"
unique(RIMUS23_Doses$class)



Experience <- Experience %>% left_join(RIMUS23_Doses) %>% drop_na() %>%
  #filter(Type=="Prev") %>%
  mutate(Volume=as.numeric(weight)*as.numeric(days_sup)) %>%
  select(-c(weight, days_sup))

# Experience <- Experience %>% select(-Type)

unique(Experience$class)



Unique_provcats <- fread("Unique_provcats.csv")

Experience <- Experience %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na()

unique(Experience$TYPE)

Experience <- Experience %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                ifelse(TYPE=="PRIMARY CARE"|TYPE=="INTERNAL MEDICINE"|TYPE=="INTERAL MEDICINE", "PCP", "OTHER")))


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 


RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(generic=ifelse(generic=="Rimegepant", generic, "CGRP Oral")) %>% select(patid, generic, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[3] <- "OralAcute"
names(RIMUS23_Doses)[4] <- "OralPrev"
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(OralAcute=ifelse(OralPrev==1,0,OralAcute))
names(RIMUS23_Doses)[2] <- "class"
names(RIMUS23_Doses)[1] <- "patient"

Experience <- Experience %>% left_join(RIMUS23_Doses)
Experience[is.na(Experience)] <- 0

data.frame(Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
    mutate(class=ifelse(class=="Rimegepant", "Rimegepant_Acute", class)) %>%
  group_by(Type, TYPE, group, class) %>%  summarise(total=sum(as.numeric(Volume)))
  )

# --------------
# Acute vs prventive year over year ------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>%  filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 

RIMUS23_Doses %>%
  ggplot(aes(from_dt, as.numeric(days_sup), colour=generic, fill=generic)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Exact Script Date") + ylab("Supply days per script \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  ylim(0,40)


RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()

data.frame(RIMUS23_Doses) 

RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

range(RIMUS23_Doses$from_dt)




RIMUS23_Doses %>% mutate(year=ifelse(from_dt<"2018-06-01", "Y1",
                                     ifelse(from_dt<"2019-06-01", "Y2",
                                            ifelse(from_dt<"2020-06-01", "Y3",
                                                   ifelse(from_dt<"2021-06-01", "Y4", 
                                                          ifelse(from_dt<"2022-06-01", "Y5", "Y6")))))) %>%
  mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(year, patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(year, generic, mean) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=year, value=n)

# -------------
# Acute vs prventive year over year ------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>%  filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 


RIMUS23_Doses %>%
  ggplot(aes(from_dt, as.numeric(days_sup), colour=generic, fill=generic)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Exact Script Date") + ylab("Supply days per script \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  ylim(0,40)



RIMUS23_Doses %>%
  left_join(
RIMUS23_Doses %>% group_by(patid, generic) %>% filter(from_dt==min(from_dt)) %>% filter(days_sup==min(as.numeric(days_sup))) %>% slice(1) %>%
  select(patid, generic, days_sup) %>% rename("min"="days_sup")) %>%
  mutate(days_sup=as.numeric(days_sup)-as.numeric(min)) %>%
  ggplot(aes(from_dt, as.numeric(days_sup), colour=generic, fill=generic)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Exact Script Date") + ylab("Increase in Supply days per script per patient \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() 
  
  
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()

data.frame(RIMUS23_Doses) 

RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

range(RIMUS23_Doses$from_dt)




RIMUS23_Doses %>% mutate(year=ifelse(from_dt<"2018-06-01", "Y1",
                                     ifelse(from_dt<"2019-06-01", "Y2",
                                            ifelse(from_dt<"2020-06-01", "Y3",
                                                   ifelse(from_dt<"2021-06-01", "Y4", 
                                                          ifelse(from_dt<"2022-06-01", "Y5", "Y6")))))) %>%
  mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(year, patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% group_by(year, generic, mean) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=year, value=n)

# -------------
# Class Penetrance before and after Oral CGRP Start ------------------------------------------------------------------------------
flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with ORAL CGRP after m48
MIG_CGRP_ORAL_After_48 <- flMIG %>% 
  filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2)) %>% select(patient) %>% distinct()

#pats with ORAL CGRP before m48
MIG_CGRP_ORAL_Before_48 <- flMIG %>% 
  filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2) | grepl("135",d1) | grepl("136",d1) | grepl("137",d1)) %>% select(patient) %>% distinct()

#pats with ORAL CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% anti_join(MIG_CGRP_ORAL_Before_48)
rm(MIG_CGRP_ORAL_Before_48)

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join(flMIG)

# first occurence of ORAL CGRP, month before and after as well, remove those who started on m60
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient) %>% filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1) %>% mutate(after=p2+1) %>% filter(after <= 60)


# collpase month to single column
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% arrange(patient, n)

# add drugs
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((flMIG %>% select(patient, p2, s2, d2)), by=c("patient"="patient", "n"="p2"))

# to long, each molecule
MIG_CGRP_ORAL_After_48 <- separate_rows(MIG_CGRP_ORAL_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_ORAL_After_48)[4] <- "drug_id"


# drugs look up
Drugs_lookup <- fread("Drugs_lookup.csv")
Drugs_lookup <- Drugs_lookup %>% select(drug_id, drug_class) %>% mutate(drug_id=as.character(drug_id))

# add drug classes
MIG_CGRP_ORAL_After_48<- MIG_CGRP_ORAL_After_48 %>% left_join(Drugs_lookup) %>% arrange(patient)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% mutate(n=ifelse(n==min(n), "Before", 
                                                                     ifelse(n==max(n),"After", "Current")))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((flMIG %>% select(patient, weight) %>% distinct()))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% select(patient, n, s2, drug_class, weight) %>% distinct()
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% mutate(drug_class = ifelse(is.na(drug_class), "Lapsed", drug_class))

# Total weights = 179529
MIG_CGRP_ORAL_After_48 %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))

MIG_CGRP_ORAL_After_48 %>% select(patient, weight,n) %>% distinct() %>% group_by(patient) %>% count() %>%
  filter(n==3)




before_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before") %>% group_by(n, drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange(-penetrance) %>%
             mutate(drug_class= as.factor(drug_class)))

current_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Current") %>% group_by(n,  drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange(-penetrance) %>%
             mutate(drug_class= as.factor(drug_class))) 

after_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After") %>% group_by(n,  drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange( -penetrance) %>%
             mutate(drug_class= as.factor(drug_class))) 


before_penetrance %>% select(-n) %>% rename("before"="penetrance") %>%
  full_join(current_penetrance %>% select(-n) %>% rename("current"="penetrance")) %>%
  full_join(after_penetrance %>% select(-n) %>% rename("after"="penetrance")) 

write.csv(before_penetrance, "before_Oral_CGRP_s2_penetrance.csv")
write.csv(current_penetrance, "current_Oral_CGRP_s2_penetrance.csv")
write.csv(after_penetrance, "after_Oral_CGRP_s2_penetrance.csv")
write.csv(MIG_CGRP_ORAL_After_48, "MIG_CGRP_ORAL_Pats_Starting_After_48_Class_Penetrance.csv")




# ---------------

# % Neurologists vs scripts -----------------


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G")
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt, generic, drug_group, provider , provcat)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) 

RIMUS23_Doses %>% filter(TYPE == "NEUROLOGY") %>% select(provider) %>% distinct() # 9336

RIMUS23_Doses %>% filter(TYPE == "NEUROLOGY") %>%  group_by(drug_group) %>% count() 


 RIMUS23_Doses %>% filter(drug_group == "CGRP Injectable") %>% 
  filter(TYPE == "NEUROLOGY") %>% 
  group_by(provider) %>% summarise(n_scripts=n()) %>% arrange(-n_scripts) %>%
  mutate(percent_scripts=(n_scripts/(25896))*100) %>%
  mutate(perc_physic=(1/9336)*100)%>%
  mutate(perc_physic_CUM=cumsum(perc_physic)) %>%
  mutate(percent_scripts_CUM=cumsum(percent_scripts))%>%
  ggplot(aes(perc_physic_CUM, percent_scripts_CUM))+
  geom_point(colour="deeppink4")+
  xlim(0,100)+ ylim(0,100) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nProportion of Neurologists #")+ylab("Proportion of Neurology Injectable CGRP Scripts\n")

# ------------
# % PCPs/IMs vs scripts -----------------


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G")
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt, generic, drug_group, provider , provcat)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) 

unique(RIMUS23_Doses$TYPE)

RIMUS23_Doses %>% filter(TYPE == "PRIMARY CARE"|TYPE == "INTERNAL MEDICINE"|TYPE=="INTERAL MEDICINE") %>% 
  select(provider) %>% distinct() # 93919

RIMUS23_Doses %>% filter(TYPE == "PRIMARY CARE"|TYPE == "INTERNAL MEDICINE"|TYPE=="INTERAL MEDICINE") %>%  group_by(drug_group) %>% count() 



 RIMUS23_Doses %>% filter(grepl("CGRP",drug_group)) %>% 
  filter(TYPE == "PRIMARY CARE"|TYPE == "INTERNAL MEDICINE"|TYPE=="INTERAL MEDICINE") %>% 
  group_by(provider) %>% summarise(n_scripts=n()) %>% arrange(-n_scripts) %>%
  mutate(percent_scripts=(n_scripts/(13728))*100) %>%
  mutate(perc_physic=(1/93919)*100)%>%
  mutate(perc_physic_CUM=cumsum(perc_physic)) %>%
  mutate(percent_scripts_CUM=cumsum(percent_scripts))%>%
  ggplot(aes(perc_physic_CUM, percent_scripts_CUM))+
  geom_point(colour="firebrick")+
  xlim(0,5)+ ylim(0,100) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nProportion of PCPs/IMs #")+ylab("Proportion of PCPs/IMs Any CGRP Scripts\n")

 
# -----------------
# Number of flows per stock --------------------

flMIG <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character", stringsAsFactors = FALSE)

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)

flMIG <- flMIG %>% filter(as.numeric(p2)>=49)

flMIG <- flMIG %>% group_by(patient) %>% summarise(n=sum(as.numeric(flow)))

ModSev_vector %>% filter(group=="ModSev") %>% select(-group) %>%
  left_join(flMIG) %>%
  ungroup() %>% group_by(n) %>% summarise(total=sum(as.numeric(weight))/11856517)


ModSev_vector %>% filter(group=="ModSev") %>% select(-group) %>%
  left_join(flMIG) %>%
  ungroup() %>% summarise(mean=weighted.mean(n, as.numeric(weight)))


ModSev_vector %>% filter(group=="ModSev") %>% select(-group) %>%
  left_join(flMIG) %>%
  ungroup() %>% summarise(mean=weighted.median(n, as.numeric(weight)))


RIMUS23_Box_Histories <- read.table("RIMUS23 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Box_Histories <- RIMUS23_Box_Histories %>% select(patient, weight, month60)

  RIMUS23_Box_Histories %>%  left_join(ModSev_vector) %>% left_join(flMIG) %>%
  ungroup() %>% mutate(month60=ifelse(group=="Mild", "Mild", month60)) %>%
  group_by(group, month60) %>%
  summarise(total=sum(as.numeric(weight)))
  
  
  RIMUS23_Box_Histories %>%  left_join(ModSev_vector) %>% left_join(flMIG) %>%
  ungroup() %>% mutate(month60=ifelse(group=="Mild", "Mild", month60)) %>%
  group_by(group, month60) %>%
  summarise(mean=weighted.median(n,as.numeric(weight)))
  
data.frame(
  RIMUS23_Box_Histories %>%  left_join(ModSev_vector) %>% left_join(flMIG) %>%
  ungroup() %>% mutate(month60=ifelse(group=="Mild", "Mild", month60)) %>%
  group_by(group, month60, n) %>%
  summarise(total=sum(as.numeric(weight))) %>%
  spread(key=month60, value=total)
  )


# -------------------------
# Share of CGRPs Year 4 to year 5 by NEW/OLD Physicians -----------------------------------------------------------------
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G")
names(RIMUS23_Doses)
RIMUS23_Doses <- RIMUS23_Doses %>% select(-c(drug_id, days_sup,  status))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))

#year4
RIMUS23_Doses_Year4 <- RIMUS23_Doses %>%filter(from_dt >= "2021-06-01" & from_dt <= "2022-05-31") 
Physicians_Year4 <- RIMUS23_Doses_Year4 %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") %>% select(provider) %>% distinct() #6608
RIMUS23_Doses_Year4 <- RIMUS23_Doses_Year4 %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") #59417

#year5
RIMUS23_Doses_Year5 <- RIMUS23_Doses %>%filter(from_dt >= "2022-06-01" & from_dt <= "2023-05-31") 
Physicians_Year5 <- RIMUS23_Doses_Year5 %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") %>% select(provider) %>% distinct() #8230
RIMUS23_Doses_Year5 <- RIMUS23_Doses_Year5 %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") #76267


#all of them, plus number of scripts in each year
Summary_Year4to5 <- RIMUS23_Doses_Year4 %>% select(provider) %>% group_by(provider) %>% summarise(n=n()) %>% rename(year4=n) %>%
  full_join(RIMUS23_Doses_Year5 %>% select(provider) %>% group_by(provider) %>% summarise(n2=n()) %>% rename(year5=n2)) %>%
  replace(is.na(.), 0) %>% filter(provider!="")%>% mutate(Diff_4to5=year5-year4) %>%
  mutate(Physician_type=ifelse(year4==0, "NEW",
                               ifelse(year5==0, "STOP",
                                      ifelse(year5>year4, "INCREASE",
                                             ifelse(year4>year5, "DECREASE", 
                                                    ifelse(year4==year5, "MAINTAIN", "NONE"))))))

sum(Summary_Year4to5$year4) #59417
sum(Summary_Year4to5$year5) #76267
sum(Summary_Year4to5$Diff_4to5) #16850

Summary_Year4to5 %>% filter(year4==0) %>% summarise(n=sum(Diff_4to5)) #13413
Summary_Year4to5 %>% filter(year5==0) %>% summarise(n=sum(Diff_4to5)) #-7065
Summary_Year4to5 %>% filter(year4!=0)  %>% filter(year5!=0) %>% filter(year5>year4) %>% summarise(n=sum(Diff_4to5)) #20141
Summary_Year4to5 %>% filter(year4!=0)  %>% filter(year5!=0) %>% filter(year5<year4) %>% summarise(n=sum(Diff_4to5)) #-9639

Summary_Year4to5 %>% group_by(Physician_type) %>% summarise(n=sum(Diff_4to5))
Summary_Year4to5 %>% group_by(Physician_type) %>% count()

Summary_Year4to5 %>% filter(year4!=0) %>% select(provider, Physician_type) %>% left_join(RIMUS23_Doses_Year4) %>%
  select(provider, patid) %>% distinct() %>% group_by(provider) %>% summarise(n=n()) %>% summarise(n2=mean(n)) #1.71

Summary_Year4to5 %>% filter(year4!=0) %>% select(provider, Physician_type) %>% left_join(RIMUS23_Doses_Year4) %>%
  select(Physician_type, provider, patid) %>% distinct() %>% group_by(Physician_type, provider) %>% summarise(n=n()) %>% summarise(n2=mean(n))


Summary_Year4to5 %>% filter(year5!=0) %>% select(provider, Physician_type) %>% left_join(RIMUS23_Doses_Year5) %>%
  select(provider, patid) %>% distinct() %>% group_by(provider) %>% summarise(n=n()) %>% summarise(n2=mean(n)) #1.77


Summary_Year4to5 %>% filter(year5!=0) %>% select(provider, Physician_type) %>% left_join(RIMUS23_Doses_Year5) %>%
  select(Physician_type, provider, patid) %>% distinct() %>% group_by(Physician_type, provider) %>% summarise(n=n()) %>% summarise(n2=mean(n)) 


# ----------------------------------
# Persistency --------------------
Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- ModSev_vector %>% filter(group=="ModSev") %>% select(patient) %>%
  left_join(RIMUS23_Drug_Histories)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup, by=c("Treat"="drug_id"))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  select(patient, weight, Month, drug_class) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, drug_class) %>% count() %>% ungroup()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(drug_class, n) %>% summarise(pop=sum(as.numeric(weight)))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% ungroup() %>% group_by(drug_class) %>% mutate(Total=sum(pop))

data.frame(RIMUS23_Drug_Histories %>% group_by(drug_class) %>%
  summarise(mean=weighted.mean(n,pop)))


RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(drug_class) %>% mutate(left=Total)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-left)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Total) %>% spread(key=drug_class, value=pop)

fwrite(RIMUS23_Drug_Histories, "temp.csv")

# --------------
 
# Types of Intraflows ----------------

Drugs_lookup <- fread("Drugs_lookup.csv")

string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "Triptan"], collapse = "|"),")\\b")
string_CGRPOral <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRPInjectable <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")


flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")

flMIG <- flMIG %>% filter(p2>=49) %>% filter(flow==1) %>% filter(s1==s2) 
flMIG <- flMIG %>% select(patient,weight,p1,p2,d1,d2,s1,s2,flow)
flMIG <- flMIG %>% filter(s2=="T"|s2=="O"|s2=="I")

# Triptan Therapy class - flags

flMIG <- flMIG[, Triptan_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Triptan), str_c(unlist(str_extract_all(x, string_Triptan)), collapse = ","),"")))]
flMIG <- flMIG[, Triptan_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Triptan), str_c(unlist(str_extract_all(x, string_Triptan)), collapse = ","),"")))]
flMIG <- flMIG[, nr_Triptan_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Triptan)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_Triptan_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Triptan)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_TriptanUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Triptan_d1","Triptan_d2")] 
flMIG <- flMIG[, Triptan_flow_type := ifelse(nr_Triptan_d2 < nr_Triptan_d1 & nr_TriptanUnq_d1d2 > nr_Triptan_d1, "D+S", 
                                           ifelse(nr_Triptan_d2 > nr_Triptan_d1 & nr_TriptanUnq_d1d2 > nr_Triptan_d2, "A+S",
                                                  ifelse(nr_Triptan_d2 < nr_Triptan_d1, "D", 
                                                         ifelse(nr_Triptan_d2 > nr_Triptan_d1, "A", 
                                                                ifelse(nr_Triptan_d2 == nr_Triptan_d1 & Triptan_d2 != Triptan_d1, "S","-")))))] 



flMIG %>% filter(s1=="T") %>% group_by(Triptan_flow_type) %>% summarise(n=sum(weight))


# CGRPOral Therapy class - flags
flMIG <- flMIG[, CGRPOral_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_CGRPOral), str_c(unlist(str_extract_all(x, string_CGRPOral)), collapse = ","),"")))]
flMIG <- flMIG[, CGRPOral_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_CGRPOral), str_c(unlist(str_extract_all(x, string_CGRPOral)), collapse = ","),"")))]
flMIG <- flMIG[, nr_CGRPOral_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_CGRPOral)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_CGRPOral_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_CGRPOral)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_CGRPOralUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("CGRPOral_d1","CGRPOral_d2")] 
flMIG <- flMIG[, CGRPOral_flow_type := ifelse(nr_CGRPOral_d2 < nr_CGRPOral_d1 & nr_CGRPOralUnq_d1d2 > nr_CGRPOral_d1, "D+S", 
                                            ifelse(nr_CGRPOral_d2 > nr_CGRPOral_d1 & nr_CGRPOralUnq_d1d2 > nr_CGRPOral_d2, "A+S",
                                                   ifelse(nr_CGRPOral_d2 < nr_CGRPOral_d1, "D", 
                                                          ifelse(nr_CGRPOral_d2 > nr_CGRPOral_d1, "A", 
                                                                 ifelse(nr_CGRPOral_d2 == nr_CGRPOral_d1 & CGRPOral_d2 != CGRPOral_d1, "S","-")))))] 


flMIG %>% filter(s1=="O") %>% group_by(CGRPOral_flow_type) %>% summarise(n=sum(weight))


# CGRPInjectable Therapy class - flags
flMIG <- flMIG[, CGRPInjectable_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_CGRPInjectable), str_c(unlist(str_extract_all(x, string_CGRPInjectable)), collapse = ","),"")))]
flMIG <- flMIG[, CGRPInjectable_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_CGRPInjectable), str_c(unlist(str_extract_all(x, string_CGRPInjectable)), collapse = ","),"")))]
flMIG <- flMIG[, nr_CGRPInjectable_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_CGRPInjectable)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_CGRPInjectable_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_CGRPInjectable)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_CGRPInjectableUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("CGRPInjectable_d1","CGRPInjectable_d2")] 
flMIG <- flMIG[, CGRPInjectable_flow_type := ifelse(nr_CGRPInjectable_d2 < nr_CGRPInjectable_d1 & nr_CGRPInjectableUnq_d1d2 > nr_CGRPInjectable_d1, "D+S", 
                                                  ifelse(nr_CGRPInjectable_d2 > nr_CGRPInjectable_d1 & nr_CGRPInjectableUnq_d1d2 > nr_CGRPInjectable_d2, "A+S",
                                                         ifelse(nr_CGRPInjectable_d2 < nr_CGRPInjectable_d1, "D", 
                                                                ifelse(nr_CGRPInjectable_d2 > nr_CGRPInjectable_d1, "A", 
                                                                       ifelse(nr_CGRPInjectable_d2 == nr_CGRPInjectable_d1 & CGRPInjectable_d2 != CGRPInjectable_d1, "S","-")))))] 

flMIG %>% filter(s1=="I") %>% group_by(CGRPInjectable_flow_type) %>% summarise(n=sum(weight))

# -----------------
# Stocks over time -----------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Box_Histories <- read.table("RIMUS23 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Box_Histories <- ModSev_vector %>% filter(group=="ModSev") %>% select(patient) %>%  left_join(RIMUS23_Box_Histories)

RIMUS23_Box_Histories <- gather(RIMUS23_Box_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Box_Histories$Month <- parse_number(as.character(RIMUS23_Box_Histories$Month))

data.frame(RIMUS23_Box_Histories %>% group_by(Month, Treat) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=Treat, value=n))
  
# ------------
# Number of drugs before and after Oral CGRP Start -4 to +4  --------------

flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))


#pats with ORAL CGRP after m48
MIG_CGRP_ORAL_After_48 <- flMIG %>% 
  filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2)) %>% select(patient) %>% distinct()

#pats with ORAL CGRP before m48
MIG_CGRP_ORAL_Before_48 <- flMIG %>% 
  filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2) | grepl("135",d1) | grepl("136",d1) | grepl("137",d1)) %>% select(patient) %>% distinct()



#pats with ORAL CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% anti_join(MIG_CGRP_ORAL_Before_48)
rm(MIG_CGRP_ORAL_Before_48)


MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join(flMIG)

# first occurence of ORAL CGRP, month before and after as well, remove those who started on m60
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient) %>% filter(grepl("135",d2) | grepl("136",d2)| grepl("137",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% 
  mutate(before=p2-1) %>% mutate(before2=p2-2) %>% mutate(before3=p2-3) %>% mutate(before4=p2-4) %>% 
  mutate(after=p2+1) %>% mutate(after2=p2+2) %>% mutate(after3=p2+3) %>% mutate(after4=p2+4) %>% filter(after4 <= 60)


# collpase month to single column
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% arrange(patient, n)


# add drugs
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((flMIG %>% select(patient, p2, d2)), by=c("patient"="patient", "n"="p2"))


# to long, each molecule
MIG_CGRP_ORAL_After_48 <- separate_rows(MIG_CGRP_ORAL_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_ORAL_After_48)[3] <- "drug_id"

# drugs look up
Drugs_lookup <- fread("Drugs_lookup.csv")
Drugs_lookup <- Drugs_lookup %>% select(drug_id, drug_class) %>% mutate(drug_id=as.character(drug_id))

# add drug classes
MIG_CGRP_ORAL_After_48<- MIG_CGRP_ORAL_After_48 %>% left_join(Drugs_lookup) %>% arrange(patient)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% mutate(n=ifelse(n==min(n), "Before4",ifelse(n==min(n)+1, "Before3", ifelse(n==min(n)+2, "Before2", ifelse(n==min(n)+3, "Before", 
                                                                                                                                                               ifelse(n==max(n), "After4",ifelse(n==max(n)-1, "After3", ifelse(n==max(n)-2, "After2", ifelse(n==max(n)-3, "After", "Current")))))))))

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((flMIG %>% select(patient, weight) %>% distinct()))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% mutate(drug_class = ifelse(is.na(drug_class), "Lapsed", drug_class))

length(unique(MIG_CGRP_ORAL_After_48$patient)) #2597


data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before4") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.184809

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before3") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.199971

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before2") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.218018

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.233168

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Current") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.83243

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.67686

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After2") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.471592

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After3") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.452906

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After4") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#1 3.449997


# -----------------------

# Share of Rimegepant per Physician --------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G")
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt, generic, days_sup, drug_group, provider)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_group=="CGRP Oral") %>% select(-drug_group)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses <- RIMUS23_Doses %>% select(-c(from_dt))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(vol=as.numeric(days_sup)) %>% select(-c(weight, days_sup))


  
RIMUS23_Doses %>% group_by(provider) %>% mutate(total_vol=sum(vol)) %>% select(provider, total_vol) %>%
  left_join(RIMUS23_Doses %>%  filter(generic=="Rimegepant") %>% group_by(provider) %>% mutate(rime_vol=sum(vol)) %>% select(provider, rime_vol)) %>% 
  select(provider, total_vol, rime_vol) %>% distinct() %>% mutate(rime_vol=ifelse(is.na(rime_vol),0,rime_vol)) %>%
   ggplot(aes(total_vol, rime_vol)) +
  geom_jitter(size=0.3, alpha=0.2)+
  xlim(0,25000) + ylim(0,25000) 


RIMUS23_Doses %>%
  ggplot(aes(total_vol, rime_vol)) +
  geom_jitter(size=0.3, alpha=0.2) +
  #  geom_hex(bins = 50)  +
  # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  # scale_x_continuous(expand = c(0, 0)) +
  # scale_y_continuous(expand = c(0, 0)) +
  # scale_fill_continuous(type = "viridis") +
  xlim(0,25000) + ylim(0,25000) +
  # theme_void() +
  xlab("\n Total CGRP Volume Last Year") + ylab("Total Rimegepant Volume Last Year")



# 0, 0.25, 0.75, 1
  
RIMUS23_Doses %>% group_by(provider) %>% mutate(total_vol=sum(vol)) %>% select(provider, total_vol) %>%
  left_join(RIMUS23_Doses %>%  filter(generic=="Rimegepant") %>% group_by(provider) %>% mutate(rime_vol=sum(vol)) %>% select(provider, rime_vol)) %>% 
  select(provider, total_vol, rime_vol) %>% distinct() %>% mutate(rime_vol=ifelse(is.na(rime_vol),0,rime_vol)) %>%
  mutate(share=rime_vol/total_vol) %>% mutate(share=ifelse(share==1,1,
                                                           ifelse(share>=0.75, 0.75,
                                                                  ifelse(share>=0.25, 0.5,
                                                                         ifelse(share>0,0.25,0))))) %>% group_by(share) %>% 
  summarise(sum=sum(total_vol))


RIMUS23_Doses %>% select(provider, patid) %>% distinct() %>% group_by(provider) %>% count() %>%
  filter(n!="369" ) %>%
  ggplot(aes(n)) +
  geom_density()


RIMUS23_Doses %>% group_by(provider) %>% mutate(total_vol=sum(vol)) %>% select(provider, total_vol) %>%
  left_join(RIMUS23_Doses %>%  filter(generic=="Rimegepant") %>% group_by(provider) %>% mutate(rime_vol=sum(vol)) %>% select(provider, rime_vol)) %>% 
  select(provider, total_vol, rime_vol) %>% distinct() %>% mutate(rime_vol=ifelse(is.na(rime_vol),0,rime_vol)) %>%
  mutate(share=rime_vol/total_vol) %>% mutate(share=ifelse(share==1,1,
                                                           ifelse(share>=0.75, 0.75,
                                                                  ifelse(share>=0.25, 0.5,
                                                                         ifelse(share>0,0.25,0))))) %>% group_by(share) %>% 
  count()


# ---------------
# Profile of a Rimegepant patient ---------------------------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
ModSev_vector <- ModSev_vector %>% filter(group=="ModSev")

Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- ModSev_vector %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, generic ), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, generic) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories  %>% mutate(exp=1) %>% spread(key=generic, value=exp)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Demographics)[1] <- "patient"
RIMUS23_Demographics <- RIMUS23_Demographics %>% inner_join(ModSev_vector) 
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patient, GENDER, AGE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% mutate(GENDER=ifelse(GENDER=="M",1,0))
df <- RIMUS23_Demographics %>% left_join(RIMUS23_Drug_Histories)
df[is.na(df)] <- 0

flMIG <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- df %>% select(patient) %>% left_join(flMIG)
df <- df %>% left_join(flMIG %>% group_by(patient) %>% summarise(flows=sum(as.numeric(flow)))) 
df <- df %>% left_join(flMIG %>% group_by(patient) %>% summarise(starts=sum(as.numeric(starts)))) 
df <- df %>% left_join(flMIG %>% group_by(patient) %>% summarise(stops=sum(as.numeric(stops)))) 


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- ModSev_vector %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")   %>% distinct() %>% group_by(patient) %>% count() %>% rename("N_Months_Treat"="n")

df <- df %>% left_join(RIMUS23_Drug_Histories)

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- df %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()
df <- df %>% left_join(RIMUS23_Drug_Histories %>% rename("lines"="n"))



RIMUS23_Comorbidities_Extended_Dxs <- read.table("RIMUS23 Comorbidities Extended Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Comorbidities_Extended_Dxs)[1] <- "patient"
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patient, ICD10_diag) %>% distinct() %>% inner_join(df %>% select(patient))
#RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% group_by(patient, ICD10_diag) %>% count()
#RIMUS23_Comorbidities_Extended_Dxs$ICD10_diag <-  substr(as.character(RIMUS23_Comorbidities_Extended_Dxs$ICD10_diag), 1, 3)
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% distinct()
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs  %>% mutate(exp=1) %>% spread(key=ICD10_diag, value=exp)
RIMUS23_Comorbidities_Extended_Dxs[is.na(RIMUS23_Comorbidities_Extended_Dxs)] <- 0
df <- df %>% left_join(RIMUS23_Comorbidities_Extended_Dxs)

names(df) <- str_replace_all(names(df), " ", "_")

# RIMUS23_Comorbidities_Extended_Dxs <- read.table("RIMUS23 Comorbidities Extended Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
# names(RIMUS23_Comorbidities_Extended_Dxs)[1] <- "patient"
# RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag))
# RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patient, date) %>% distinct() %>% group_by(patient) %>% count()
# RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% rename("N_MIG_Dx"="n")
# 
# df <- df %>% left_join(RIMUS23_Comorbidities_Extended_Dxs)

Unique_provcats <- fread("Unique_provcats.csv")
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, provider , provcat) %>% distinct()
RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, provider, provcat) %>% distinct()
All_provs <- RIMUS23_Doses %>% select(patid, provider, provcat) %>% full_join(RIMUS23_Migraine_Dxs %>% select(patid, provider, provcat)) %>% distinct() 
All_provs <- All_provs %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))
All_provs <- All_provs %>% filter(TYPE=="NEUROLOGY") 
All_provs <- All_provs %>% select(patid) %>% mutate(NEUROLOGY=1)
names(All_provs)[1] <- "patient"

df <- df %>% left_join(All_provs)

df[is.na(df)] <- 0

df <- df %>% distinct()


#df_2 <- df %>% mutate(Oral=ifelse(Rimegepant==1|Ubrogepant==1|Atogepant==1,1,0)) %>% select(-c(Rimegepant, Ubrogepant, Atogepant))

df_2 <- df %>% mutate(Rimegepant=ifelse(Rimegepant==0,0,1))

df_2 %>% group_by(Rimegepant) %>% count()

df_3 <- df_2 %>% group_by(Rimegepant) %>% sample_n(4000) %>% ungroup()

df_3 <- df_3 %>% select(-patient) %>% mutate(AGE=as.numeric(AGE))

library(randomForest)
library(xgboost)
library(caret)


# modelAll_1_randomForest <- randomForest(Rimegepant ~ ., data = df_2)
# 
# summary(modelAll_1_randomForest)
# 
# data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)
# 
# df_2 %>% group_by(Rimegepant) %>% summarise(AGE=mean(AGE))
# df_2 %>% group_by(Botulinum_Toxin,Rimegepant) %>% count()



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


names(df_3)

model_hd = xgboost(data = as.matrix(df_3[,-120]),
                   nround = 500,
                   objective = "binary:logistic",
                   label=as.matrix(df_3[,120]))  



shap_result = shap.score.rank(xgb_model = model_hd, 
                              X_train = as.matrix(df_3[,-120]),
                              shap_approx = F)


var_importance(shap_result, top_n=50)



shap_long_hd = shap.prep(X_train = as.matrix(df_3[,-120]) , top_n = 50)

plot.shap.summary(data_long = shap_long_hd)

shap_long_hd %>% select(variable) %>% distinct()

df_3 %>% group_by(Rimegepant) %>% summarise(mean=mean(stops))

result <- data.frame(df_3 %>% group_by(Rimegepant) %>% summarise_all(mean))

result <- data.frame(names(result)) %>% bind_cols(result %>% transpose())
 

fwrite(result, "result.csv")


# -----------------
# Lapsed patients directly to Rimegepant -----------------

flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")

flMIG <- flMIG %>% select(-c(disease, p1_RxExp, flow, stops, starts, re_starts)) %>% arrange(patient, p1, p2)

Inflows <- flMIG %>% filter(p1>=49) %>%
  filter(!grepl("136",d1)&grepl("136",d2)) %>%  
  select(patient, weight, s1, p1)

length(unique(Inflows$patient))

Inflows <- Inflows %>% group_by(patient, weight) %>% filter(p1==max(p1)) %>% ungroup() 
names(Inflows)[3] <- "Origin"
names(Inflows)[4] <- "Date"


flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")


flMIG <- Inflows %>% left_join(flMIG) %>% filter(p1<=Date) %>% select(-c(disease, p1, d2, s1, s2))
flMIG <- flMIG %>% select(-c(flow, p1_RxExp, stops, starts, re_starts))

flMIG <- flMIG %>% select(-c(p2, Date)) %>% distinct()

flMIG %>% group_by(patient, weight, Origin) %>% count() %>% ungroup() %>%
  mutate(Origin=ifelse(Origin=="N", "X", Origin)) %>%
  group_by(Origin) %>% summarise(mean=weighted.mean(n, weight))



flMIG %>% group_by(patient, weight, Origin) %>% count() %>% ungroup() %>%
  mutate(Origin=ifelse(Origin=="N", "X", Origin)) %>%
  mutate(Origin=ifelse(Origin=="X", "Lapsed",
                       ifelse(Origin=="S", "Symptomatic",
                              ifelse(Origin=="T", "Triptans",
                                     ifelse(Origin=="P", "Preventive",
                                            ifelse(Origin=="Y", "Prev+Sympt",
                                                   ifelse(Origin=="K", "Prev+Triptan",
                                                          ifelse(Origin=="I", "Inj CGRP", "Oral CGRP")))))))) %>%
  mutate(Origin=factor(Origin, levels=c("Lapsed", "Symptomatic", "Triptans", "Preventive", "Prev+Sympt", "Prev+Triptan", "Inj CGRP", "Oral CGRP"))) %>%
  filter(Origin=="Lapsed") %>%
  ggplot(aes(n)) +
  geom_density(show.legend = F, linewidth=2) +
 # facet_wrap(~Origin) +
#  scale_fill_viridis_d() +
 # scale_colour_viridis_d() +
  theme_minimal() +
  ylab("Patient density \n") + xlab("\n Cumulative No. Therapy Lines \n Up Until Rimegepant Initiation")




Lines <- flMIG %>% group_by(patient, weight, Origin) %>% count() %>% ungroup() %>%
  mutate(Origin=ifelse(Origin=="N", "X", Origin)) %>%
  mutate(Origin=ifelse(Origin=="X", "Lapsed",
                       ifelse(Origin=="S", "Symptomatic",
                              ifelse(Origin=="T", "Triptans",
                                     ifelse(Origin=="P", "Preventive",
                                            ifelse(Origin=="Y", "Prev+Sympt",
                                                   ifelse(Origin=="K", "Prev+Triptan",
                                                          ifelse(Origin=="I", "Inj CGRP", "Oral CGRP")))))))) %>%
  mutate(Origin=factor(Origin, levels=c("Lapsed", "Symptomatic", "Triptans", "Preventive", "Prev+Sympt", "Prev+Triptan", "Inj CGRP", "Oral CGRP"))) 



flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")

flMIG <- flMIG %>% select(-c(disease, p1_RxExp, flow, stops, starts, re_starts)) %>% arrange(patient, p1, p2)

temp <- flMIG %>% filter(p1>=49) %>%
  filter(!grepl("136",d1)&grepl("136",d2)) %>%  
  select(patient, weight, s1, p1) %>% 
  group_by(patient, weight) %>% filter(p1==max(p1)) %>% ungroup() 

temp <- temp %>% rename("start"="p1", "stock"="s1")  %>%
  left_join(flMIG) %>% filter(p1>=start) %>%
  filter(grepl("136",d1)|grepl("136", d2)) %>% group_by(patient, weight, stock, start) %>% count() 


temp <- temp %>% ungroup() %>% mutate(vis=60-start) %>% select(patient, vis, n)

Lines %>% rename("lines"="n") %>% inner_join(temp) %>%
  # mutate(n=n/vis) %>%
  filter(Origin=="Lapsed") %>%
  ggplot(aes(lines, n)) +
  geom_jitter(show.legend = F, alpha=0.5) +
  #geom_smooth(show.legend = F, method="lm") +
  # facet_wrap(~Origin) +
  theme_minimal() +
  #  scale_fill_viridis_d() +
  # scale_colour_viridis_d() + 
   ylab("No Months Stayed ON Rimegepant \n (Out of 12 possible months) \n") + 
  xlab("\n Cumulative No. Therapy Lines \n Up Until Rimegepant Initiation")



flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")

flMIG <- flMIG %>% select(-c(disease, p1_RxExp, flow, stops, starts, re_starts)) %>% arrange(patient, p1, p2)

temp <- flMIG %>% filter(p1>=49) %>%
   filter(!grepl("136",d1)&grepl("136",d2)) %>%  
   select(patient, weight, s1, p1) %>% 
   group_by(patient, weight) %>% filter(p1==max(p1)) %>% ungroup()  
# 
# temp <- temp %>% rename("start"="p1", "stock"="s1")  %>%
#    left_join(flMIG) %>% filter(p1<start) %>%
#    filter(grepl("-",d1)|grepl("-", d2)) %>% group_by(patient, weight, stock, start) %>% count() 


max_treat <- temp %>% rename("start"="p1", "stock"="s1")  %>%
  left_join(flMIG) %>% filter(p1<start) %>%
  filter(d1!="-") %>% group_by(patient) %>% filter(p1==max(p1)) %>%
  select(patient, p1) %>% rename("max_treat"="p1")



max_treat %>% inner_join(temp) %>% mutate(lapsed=p1-max_treat) %>%
  filter(s1=="X"|s1=="N") %>%
  mutate(lapsed=ifelse(lapsed<6, "<6",
                       ifelse(lapsed<11, "<11", ">11"))) %>% group_by(lapsed) %>% summarise(tot=sum(weight))

max_treat %>% inner_join(temp) %>% mutate(lapsed=p1-max_treat) %>%
  filter(s1=="X"|s1=="N") %>%
  ggplot(aes(lapsed)) +
  geom_density(linewidth=2) +
 theme_minimal() +
  ylab("Patient density \n") +
  xlab("\n No. of Months Lapsed (i.e., no therapy) \n Right before Rimegepant Initiation")


temp %>%
  ggplot(aes(n, colour=stock, fill=stock)) +
  geom_density() +
  #geom_smooth(show.legend = F, method="lm") +
  facet_wrap(~stock) +
  theme_minimal() +
   scale_fill_viridis_d() +
  scale_colour_viridis_d() + 
   ylab("No Months Stayed ON Rimegepant \n") + 
  xlab("\n Cumulative No. Therapy Lines \n Up Until Rimegepant Initiation")

# ---------------------
# Segmentation source of business ------------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
Rimegepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month) %>% distinct()
names(Rimegepant_Pats)[2] <- "First_Rimegepant"

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Rimegepant_Pats %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(drug_group=ifelse(is.na(drug_group), "Lapsed", drug_group)) %>%
  mutate(exp=1) %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

RIMUS23_Drug_Histories <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month) %>%
  group_by(patient) %>% mutate(CumCGRP=cumsum(`CGRP Injectable`==1|`CGRP Oral`==1)) %>%
  mutate(CumCGRP=ifelse(CumCGRP==0,0,1))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  group_by(patient) %>% mutate(CumPrev=cumsum(Preventive ==1)) %>%
  mutate(CumPrev=ifelse(CumPrev==0,0,1))

RIMUS23_Drug_Histories %>% filter(First_Rimegepant>=25 & First_Rimegepant<=36 ) %>% select(patient, weight) %>% distinct() %>% 
  ungroup() %>% summarise(n=sum(as.numeric(weight))) # 274795 # 106942


RIMUS23_Drug_Histories %>% filter(First_Rimegepant>=25 & First_Rimegepant<=36 & Month==(First_Rimegepant-1) & CumCGRP==1) %>%
  select(patient, weight)  %>% distinct() %>% 
  ungroup() %>% summarise(n=sum(as.numeric(weight)))  # 94595 # 46501
 
CGRP_exp <- RIMUS23_Drug_Histories %>% filter(First_Rimegepant>=25 & First_Rimegepant<=36 & Month==(First_Rimegepant-1) & CumCGRP==1) %>%
  select(patient, weight)  %>% distinct() 

RIMUS23_Drug_Histories %>%  filter(First_Rimegepant>=25 & First_Rimegepant<=36   & Month==(First_Rimegepant-1) & CumPrev==1) %>% anti_join(CGRP_exp) %>%
  select(patient, weight, Lapsed)  %>% distinct() %>% ungroup() %>%
  group_by(Lapsed) %>%
  summarise(n=sum(as.numeric(weight)))

Prev_exp <- RIMUS23_Drug_Histories %>% filter(First_Rimegepant>=37 & First_Rimegepant<=48  & Month==(First_Rimegepant-1) & CumPrev==1) %>% anti_join(CGRP_exp) %>%
  select(patient, weight) %>% distinct()

RIMUS23_Drug_Histories %>% anti_join(Prev_exp) %>% filter(First_Rimegepant>=37 & First_Rimegepant<=48 & Month==(First_Rimegepant-1)) %>%  anti_join(CGRP_exp) %>%
  select(patient, weight, Lapsed)  %>% distinct() %>% ungroup() %>%
  group_by(Lapsed) %>%
  summarise(n=sum(as.numeric(weight)))


# ---------------------

# Number of triptans prior to 1st Riemgepant -------------
Drugs_lookup <- fread("Drugs_lookup.csv")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "Triptan"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
Rimegepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month, weight) %>% distinct()
names(Rimegepant_Pats)[2] <- "First_Rimegepant"

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Rimegepant)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(grepl(string_Triptan, Treat))
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(grepl(string_Triptan, Treat))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight) %>% count()
Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories) %>% mutate(n=ifelse(is.na(n),0,n))

Rimegepant_Pats %>% group_by(n) %>% summarise(tot=sum(as.numeric(weight)))
Rimegepant_Pats %>% ungroup() %>% summarise(mean=weighted.mean(n, as.numeric(weight)))

# -------------
# Proportion of Accompanying Scripts  From the same Rimegepant Provider -------------------------
RIMUS23_Doses_2 <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Doses_2 <- RIMUS23_Doses_2 %>% filter(generic=="Rimegepant") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2023-04-16"&from_dt<="2023-05-15") %>%
  select(patid) %>% distinct() %>% left_join(RIMUS23_Doses_2) %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2023-04-16"&from_dt<="2023-05-15") 
  
RIMUS23_Doses_2 <- RIMUS23_Doses_2 %>% select(patid, weight, from_dt, generic, drug_class, provider)

RIMUS23_Doses_2 <- RIMUS23_Doses_2 %>% filter(generic=="Rimegepant") %>% select(patid, provider) %>% 
  rename("rim_provider"="provider") %>% distinct() %>%
  left_join(RIMUS23_Doses_2)
 

temp <- RIMUS23_Doses_2 %>% filter(generic!="Rimegepant") %>% group_by(patid) %>% summarise(all=sum(as.numeric(weight))) %>%
  left_join(RIMUS23_Doses_2 %>% filter(generic!="Rimegepant" & provider==rim_provider) %>% 
              group_by(patid) %>% summarise(rim_prov=sum(as.numeric(weight))))

temp[is.na(temp)] <- 0

sum(temp$rim_prov)/sum(temp$all)

temp %>% mutate(perc=rim_prov/all) %>%
  ggplot(aes(perc)) +
  geom_density(linewidth=2) +
  xlab(" \n Proportion of Accompanying Scripts \n From the same Rimegepant Provider") +
  ylab("Patient density \n") +
  theme_minimal()


# ---------------
# Exclude non-specific migraine preventive drugs -------------------

Drugs_lookup <- fread("Drugs_lookup.csv")
Drugs_lookup <- Drugs_lookup %>% filter(drug_group!="Preventive" | drug_id==131 | drug_id==91 | drug_id==94 | drug_id== 101 | drug_id== 105) 

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Drugs_lookup %>% select(drug_id), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, weight, Month, Treat)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, Month ) %>% mutate(Treat=paste0(Treat, collapse = ","))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
# RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Month=paste0("month", Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=Month, value=Treat)

RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- "-"

RIMUS23_Drug_Histories_NEW_short <- RIMUS23_Drug_Histories

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight)

RIMUS23_Drug_Histories_NEW_short <- RIMUS23_Drug_Histories %>% left_join(RIMUS23_Drug_Histories_NEW_short)

sum(is.na(RIMUS23_Drug_Histories_NEW_short))

RIMUS23_Drug_Histories_NEW_short[is.na(RIMUS23_Drug_Histories_NEW_short)] <- "-"

sum(as.numeric(RIMUS23_Drug_Histories_NEW_short$weight)) # 21179255

fwrite(RIMUS23_Drug_Histories_NEW_short, "RIMUS23_Drug_Histories_NEW_short.txt")





Drugs_lookup <- fread("Drugs_lookup.csv")
Drugs_lookup <- Drugs_lookup %>% filter(drug_group!="Preventive" | drug_id==131 | drug_id==91 | drug_id==94 | drug_id== 101 | drug_id== 105) 


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.numeric(drug_id)), by=c("Treat"="drug_id"))
unique(RIMUS23_Drug_Histories$drug_group)

Advanced_Rx_pats <- RIMUS23_Drug_Histories %>% 
  filter(drug_group=="Triptans"|drug_group=="CGRP Injectable"|drug_group=="CGRP Oral") %>%
  select(patient) %>% distinct()

Prev_Sympt_pats <- RIMUS23_Drug_Histories %>% select(patient, generic, drug_class, drug_group) %>% distinct() %>% anti_join(Advanced_Rx_pats)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, migraine_last_dx) %>% mutate(migraine_last_dx=as.Date(migraine_last_dx))
RIMUS23_Demographics <- RIMUS23_Demographics %>% drop_na() 
range(RIMUS23_Demographics$migraine_last_dx)
RIMUS23_Demographics <- RIMUS23_Demographics %>% filter(migraine_last_dx>"2022-05-15") %>% select(patid)
Dx_LY_pats <- RIMUS23_Demographics
names(Dx_LY_pats)[1] <- "patient"

ModHigh_Severe <- Dx_LY_pats %>% full_join(Advanced_Rx_pats)

Prev_Sympt_pats <- Prev_Sympt_pats %>% anti_join(Dx_LY_pats)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, CV, psychiatric, epileptic)
names(RIMUS23_Demographics)[1] <- "patient"

Prev_Sympt_pats <- Prev_Sympt_pats %>% left_join(RIMUS23_Demographics)
unique(Prev_Sympt_pats$drug_class)

Prev_Sympt_pats <- Prev_Sympt_pats %>% mutate(to_maintain=ifelse(drug_group=="Symptomatic", 1, 0))

Prev_Sympt_pats <- Prev_Sympt_pats %>% mutate(to_maintain=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,to_maintain)))))))))

Prev_Sympt_pats <- Prev_Sympt_pats %>% anti_join(Prev_Sympt_pats %>% filter(to_maintain==0) %>% select(patient) %>% distinct()) 

Mild_pats <- Prev_Sympt_pats %>% select(patient) %>% distinct()


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
Naive_pats <- RIMUS23_Drug_Histories %>% anti_join(RIMUS23_Drug_Histories %>% filter(Treat != "-") %>% select(patient) %>% distinct())
unique(Naive_pats$Treat) 
Mild_pats <- Naive_pats %>% select(patient) %>% distinct() %>% full_join(Mild_pats)


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- RIMUS23_Drug_Histories %>% select(patient, weight)
sum(as.numeric(All_pats$weight))

All_pats <- All_pats %>% left_join(Mild_pats %>% mutate(group="Mild"))
All_pats %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))



All_pats <- All_pats %>% mutate(group=ifelse(is.na(group), "ModSev", group))


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, X60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(X60 != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, X60, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group) %>% mutate(drug_id=as.numeric(drug_id)), by=c("X60"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, drug_group) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)

RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Box=ifelse(`CGRP Oral`==1, "O",
                                             ifelse(`CGRP Injectable`==1, "I",
                                                    ifelse(Preventive==1&Triptans==1, "K",
                                                           ifelse(Preventive==1&Symptomatic==1, "Y",
                                                                  ifelse(Preventive==1, "P",
                                                                         ifelse(Triptans==1, "T",
                                                                                ifelse(Symptomatic==1, "S", "X"))))))))


All_pats %>% left_join(RIMUS23_Drug_Histories) %>%
  group_by(group, Box) %>% summarise(n=sum(as.numeric(weight)))



All_pats <- All_pats %>% left_join(RIMUS23_Drug_Histories) %>% select(patient, weight, group, Box)





RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()

All_pats %>% left_join(RIMUS23_Drug_Histories) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  group_by(group, Box) %>% summarise(mean=weighted.mean(n, as.numeric(weight), na.rm=T))

All_pats <- All_pats %>% select(-Box)




RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month)

Drugs_lookup <- fread("Drugs_lookup.csv")
Drugs_lookup <- Drugs_lookup %>% filter(drug_group!="Preventive" | drug_id==131 | drug_id==91 | drug_id==94 | drug_id== 101 | drug_id== 105) 

string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Box=ifelse(grepl(string_Orals, Treat), "O",
                                      ifelse(grepl(string_Mabs, Treat), "I",
                                             ifelse(grepl(string_Preventive, Treat) & grepl(string_Acute, Treat), "K",
                                                    ifelse(grepl(string_Preventive, Treat) & grepl(string_Symptomatic, Treat), "Y",
                                                           ifelse(grepl(string_Preventive, Treat), "P",
                                                                  ifelse(grepl(string_Acute, Treat), "T",
                                                                         ifelse(grepl(string_Symptomatic, Treat), "S", "X"))))))))


RIMUS23_Drug_Histories %>% filter(Month>=49) %>%
  group_by(patient) %>% filter(!grepl("136", Treat)&grepl("136", lead(Treat))) %>% group_by(Box) %>% summarise(n=sum(as.numeric(weight)))



# --------------------------------


# Exclude migraine preventive drugs associated with comorbidity -------------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id"))

unique(RIMUS23_Drug_Histories$drug_class)


RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, CV, psychiatric, epileptic)
names(RIMUS23_Demographics)[1] <- "patient"

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(RIMUS23_Demographics)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(to_maintain=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(to_maintain==0|is.na(to_maintain))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, Treat) %>% arrange(patient, weight, Month, Treat)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, Month ) %>% mutate(Treat=paste0(Treat, collapse = ","))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
# RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Month=paste0("month", Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=Month, value=Treat)

RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- "-"

RIMUS23_Drug_Histories_NEW_short_2 <- RIMUS23_Drug_Histories


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight)

RIMUS23_Drug_Histories_NEW_short_2 <- RIMUS23_Drug_Histories %>% left_join(RIMUS23_Drug_Histories_NEW_short_2)

sum(is.na(RIMUS23_Drug_Histories_NEW_short_2))

RIMUS23_Drug_Histories_NEW_short_2[is.na(RIMUS23_Drug_Histories_NEW_short_2)] <- "-"

sum(as.numeric(RIMUS23_Drug_Histories_NEW_short_2$weight)) # 21179255

fwrite(RIMUS23_Drug_Histories_NEW_short_2, "RIMUS23_Drug_Histories_NEW_short_2.txt")








Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.numeric(drug_id)), by=c("Treat"="drug_id"))
unique(RIMUS23_Drug_Histories$drug_group)

Advanced_Rx_pats <- RIMUS23_Drug_Histories %>% 
  filter(drug_group=="Triptans"|drug_group=="CGRP Injectable"|drug_group=="CGRP Oral") %>%
  select(patient) %>% distinct()

Prev_Sympt_pats <- RIMUS23_Drug_Histories %>% select(patient, generic, drug_class, drug_group) %>% distinct() %>% anti_join(Advanced_Rx_pats)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, migraine_last_dx) %>% mutate(migraine_last_dx=as.Date(migraine_last_dx))
RIMUS23_Demographics <- RIMUS23_Demographics %>% drop_na() 
range(RIMUS23_Demographics$migraine_last_dx)
RIMUS23_Demographics <- RIMUS23_Demographics %>% filter(migraine_last_dx>"2022-05-15") %>% select(patid)
Dx_LY_pats <- RIMUS23_Demographics
names(Dx_LY_pats)[1] <- "patient"

ModHigh_Severe <- Dx_LY_pats %>% full_join(Advanced_Rx_pats)

Prev_Sympt_pats <- Prev_Sympt_pats %>% anti_join(Dx_LY_pats)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, CV, psychiatric, epileptic)
names(RIMUS23_Demographics)[1] <- "patient"

Prev_Sympt_pats <- Prev_Sympt_pats %>% left_join(RIMUS23_Demographics)
unique(Prev_Sympt_pats$drug_class)

Prev_Sympt_pats <- Prev_Sympt_pats %>% mutate(to_maintain=ifelse(drug_group=="Symptomatic", 1, 0))

Prev_Sympt_pats <- Prev_Sympt_pats %>% mutate(to_maintain=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,to_maintain)))))))))

Prev_Sympt_pats <- Prev_Sympt_pats %>% anti_join(Prev_Sympt_pats %>% filter(to_maintain==0) %>% select(patient) %>% distinct()) 

Mild_pats <- Prev_Sympt_pats %>% select(patient) %>% distinct()


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
Naive_pats <- RIMUS23_Drug_Histories %>% anti_join(RIMUS23_Drug_Histories %>% filter(Treat != "-") %>% select(patient) %>% distinct())
unique(Naive_pats$Treat) 
Mild_pats <- Naive_pats %>% select(patient) %>% distinct() %>% full_join(Mild_pats)


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- RIMUS23_Drug_Histories %>% select(patient, weight)
sum(as.numeric(All_pats$weight))

All_pats <- All_pats %>% left_join(Mild_pats %>% mutate(group="Mild"))
All_pats %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))


All_pats <- All_pats %>% mutate(group=ifelse(is.na(group), "ModSev", group))


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, X60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(X60 != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, X60, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group) %>% mutate(drug_id=as.numeric(drug_id)), by=c("X60"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, drug_group) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)

RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Box=ifelse(`CGRP Oral`==1, "O",
                                             ifelse(`CGRP Injectable`==1, "I",
                                                    ifelse(Preventive==1&Triptans==1, "K",
                                                           ifelse(Preventive==1&Symptomatic==1, "Y",
                                                                  ifelse(Preventive==1, "P",
                                                                         ifelse(Triptans==1, "T",
                                                                                ifelse(Symptomatic==1, "S", "X"))))))))


All_pats %>% left_join(RIMUS23_Drug_Histories) %>%
  group_by(group, Box) %>% summarise(n=sum(as.numeric(weight)))



All_pats <- All_pats %>% left_join(RIMUS23_Drug_Histories) %>% select(patient, weight, group, Box)





RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()

All_pats %>% left_join(RIMUS23_Drug_Histories) %>% mutate(n=ifelse(is.na(n),0,n)) %>%
  group_by(group, Box) %>% summarise(mean=weighted.mean(n, as.numeric(weight), na.rm=T))

All_pats <- All_pats %>% select(-Box)

All_pats %>% group_by(group) %>% summarise(n=sum(as.numeric(weight)))

fwrite(All_pats, "ModSev_Pats_V3.txt")



RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month)

Drugs_lookup <- fread("Drugs_lookup.csv")

string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Box=ifelse(grepl(string_Orals, Treat), "O",
                                      ifelse(grepl(string_Mabs, Treat), "I",
                                             ifelse(grepl(string_Preventive, Treat) & grepl(string_Acute, Treat), "K",
                                                    ifelse(grepl(string_Preventive, Treat) & grepl(string_Symptomatic, Treat), "Y",
                                                           ifelse(grepl(string_Preventive, Treat), "P",
                                                                  ifelse(grepl(string_Acute, Treat), "T",
                                                                         ifelse(grepl(string_Symptomatic, Treat), "S", "X"))))))))


RIMUS23_Drug_Histories %>% filter(Month>=49) %>%
  group_by(patient) %>% filter(!grepl("136", Treat)&grepl("136", lead(Treat))) %>% group_by(Box) %>% summarise(n=sum(as.numeric(weight))/343932)

RIMUS23_Drug_Histories %>% select(patient, weight, group) %>% distinct() %>% group_by()

# ----------------
# Penetrance vs duration V3 vector -------------
All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)



Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()

data.frame(RIMUS23_Drug_Histories %>% inner_join(All_pats) %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 11490468)*100)) %>% arrange(-sum_weights_percent)



RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_class) %>% distinct()


data.frame(All_pats %>% mutate(link=1) %>%
  full_join(Drugs_lookup %>% select(drug_class) %>% distinct() %>% mutate(link=1)) %>%
  select(-link) %>%
  left_join(RIMUS23_Drug_Histories %>% inner_join(All_pats) %>% group_by(patient, drug_class) %>% count()) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n)))


data.frame(RIMUS23_Drug_Histories %>% inner_join(All_pats) %>% group_by(patient, drug_class) %>% count() %>%
  ungroup() %>%
  group_by(drug_class) %>% summarise(mean=mean(n)))



# Penetration
Penetrance_vs_Duration <- read.csv("Penetrance_vs_Duration_V3.csv")
names(Penetrance_vs_Duration) <- c("drug_class", "penetrance", "duration")

library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(Penetrance_vs_Duration, aes(x=penetrance, y=duration, size = penetrance, fill=penetrance, colour=penetrance)) +
  geom_point(alpha=0.7)+
  geom_text_repel(aes(label = drug_class), 
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
  xlab("\n12-month Penetrance (% Population)")+
  ylab("Weighted Average Duration (months)\n")+
  ylim(0,11)+ xlim(0,50)



# ----------------------
# Number of migraine Dxs distribution -------------------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Comorbidities_Extended_Dxs <- read.table("RIMUS23 Comorbidities Extended Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Comorbidities_Extended_Dxs)[1] <- "patient"

RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)|grepl("G44", ICD10_diag)|grepl("R51", ICD10_diag))
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patient, date) %>% distinct() 
range(as.Date(RIMUS23_Comorbidities_Extended_Dxs$date))

RIMUS23_Comorbidities_Extended_Dxs$date <- as.Date(RIMUS23_Comorbidities_Extended_Dxs$date)

ModSev_vector %>%
  left_join(RIMUS23_Comorbidities_Extended_Dxs %>%  filter(date>="2022-06-16") %>% group_by(patient) %>% count()) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
 # filter(n!=0) %>%
  group_by(group) %>% summarise(mean=sum(as.numeric(weight)))

ModSev_vector %>%
  left_join(RIMUS23_Comorbidities_Extended_Dxs %>%  filter(date>="2022-06-16") %>% group_by(patient) %>% count()) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  ggplot(aes(n, colour=group, fill=group)) +
  geom_histogram(bins=21, alpha=0.5) +
  xlim(0,20) +
  theme_minimal() + xlab("\n Number of distinct Migraine diagnosis dates") + ylab("Patient Count \n") +
  facet_wrap(~group, scales="free_y") +
  scale_color_manual(values=c("deepskyblue3", "firebrick")) + 
  scale_fill_manual(values=c("deepskyblue3", "firebrick"))

# -----------------------
# Pathways to Rimegepant -------------------
All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
Rimegepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month, weight) %>% distinct()
names(Rimegepant_Pats)[2] <- "First_Rimegepant"


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
Ubrogepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("137", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month, weight) %>% distinct()
names(Ubrogepant_Pats)[2] <- "First_Ubrogepant"

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- Ubrogepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Ubrogepant)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month, Treat)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct() 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight) %>% mutate(drug_group=paste0(drug_group, collapse = "->")) %>% distinct()

data.frame(RIMUS23_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% arrange(-n))



RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

sum(as.numeric(Ubrogepant_Pats$weight))
RIMUS23_Drug_Histories <- Ubrogepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Ubrogepant)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Treat) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct() 

data.frame(RIMUS23_Drug_Histories %>% group_by(drug_class) %>% summarise(n=sum(as.numeric(weight))/532916.4) %>% arrange(-n))





RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)

RIMUS23_Drug_Histories <- Ubrogepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Ubrogepant)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month, Treat)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct() 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight) %>% mutate(drug_group=paste0(drug_group, collapse = "->")) %>% distinct()

data.frame(RIMUS23_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% arrange(-n))


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)

sum(as.numeric(Ubrogepant_Pats$weight))
RIMUS23_Drug_Histories <- Ubrogepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Ubrogepant)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Treat) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct() 

data.frame(RIMUS23_Drug_Histories %>% group_by(drug_class) %>% summarise(n=sum(as.numeric(weight))/532916.4) %>% arrange(-n))



# -----------------------
# Pathways to Rimegepant 12m prior rank -------------------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
Rimegepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month, weight) %>% distinct()
names(Rimegepant_Pats)[2] <- "First_Rimegepant"


RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
Ubrogepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("137", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month, weight) %>% distinct()
names(Ubrogepant_Pats)[2] <- "First_Ubrogepant"




RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)

RIMUS23_Drug_Histories <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Rimegepant)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(lapsed=First_Rimegepant-Month) %>% filter(lapsed<=12)


flMIG <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- flMIG %>% select(patient, p2, flow)

Flows <- RIMUS23_Drug_Histories %>% left_join(flMIG) %>% filter(p2==Month) %>% group_by(patient) %>% summarise(flow=sum(as.numeric(flow))) %>%
  filter(flow!=0) %>% select(patient) %>% mutate(flow=1)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Flows)  %>% select(patient, weight, Treat, flow)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group, flow) %>% distinct() 
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
names(RIMUS23_Drug_Histories)[9] <- "Lapsed"

sum(as.numeric(RIMUS23_Drug_Histories$weight))

RIMUS23_Drug_Histories %>% mutate(group=ifelse(`CGRP Oral`==1, "Oral",
                                               ifelse(`CGRP Injectable`==1, "Inj",
                                                      ifelse(flow==1&Preventive==1, "FlowPrev",
                                                             ifelse(flow==1&Preventive==0,"FlowNoPrev",
                                                                    ifelse(flow==0&Preventive==1&Triptans==1, "Prev+Trip",
                                                                           ifelse(flow==0&Preventive==1&Symptomatic==1, "Prev+Sympt",
                                                                                  ifelse(flow==0&Preventive==1, "Prev",
                                                                                         ifelse(flow==0&Lapsed==1, "Lapsed", "Other"))))))))) %>%
  group_by(group) %>% summarise(n=sum(as.numeric(weight)/433803.9))







RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)

RIMUS23_Drug_Histories <- Ubrogepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Ubrogepant)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(lapsed=First_Ubrogepant-Month) %>% filter(lapsed<=12)


flMIG <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- flMIG %>% select(patient, p2, flow)

Flows <- RIMUS23_Drug_Histories %>% left_join(flMIG) %>% filter(p2==Month) %>% group_by(patient) %>% summarise(flow=sum(as.numeric(flow))) %>%
  filter(flow!=0) %>% select(patient) %>% mutate(flow=1)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Flows)  %>% select(patient, weight, Treat, flow)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group, flow) %>% distinct() 
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
names(RIMUS23_Drug_Histories)[9] <- "Lapsed"

sum(as.numeric(RIMUS23_Drug_Histories$weight))

RIMUS23_Drug_Histories %>% mutate(group=ifelse(`CGRP Oral`==1, "Oral",
                                               ifelse(`CGRP Injectable`==1, "Inj",
                                                      ifelse(flow==1&Preventive==1, "FlowPrev",
                                                             ifelse(flow==1&Preventive==0,"FlowNoPrev",
                                                                    ifelse(flow==0&Preventive==1&Triptans==1, "Prev+Trip",
                                                                           ifelse(flow==0&Preventive==1&Symptomatic==1, "Prev+Sympt",
                                                                                  ifelse(flow==0&Preventive==1, "Prev",
                                                                                         ifelse(flow==0&Lapsed==1, "Lapsed", "Other"))))))))) %>%
  group_by(group) %>% summarise(n=sum(as.numeric(weight)/532916.4))





# -----------------------
# Marimekko acute vs prev , patients & volume V3 ---------------------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories





RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses %>% select(patid) %>% group_by(patid) %>% count() %>% filter(n>1)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[2] <- "OralAcute"
names(RIMUS23_Doses)[3] <- "OralPrev"


All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)
Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories) 
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
Experience <- RIMUS23_Drug_Histories

Experience <- Experience %>% left_join(RIMUS23_Doses, by=c("patient"="patid"))

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 

Rimegepan_patst <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% select(patient) %>% distinct()
Rimegepan_patst <- Rimegepan_patst %>% mutate(Rimegepant=1)

Experience <- Experience %>% left_join(Rimegepan_patst) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))

Experience$OralAcute <- as.character(Experience$OralAcute)
Experience$OralPrev <- as.character(Experience$OralPrev)

Experience[is.na(Experience)] <- "-"

Experience %>% group_by(OralAcute, OralPrev) %>% summarise(n=sum(as.numeric(weight)))
Experience  <- Experience %>% mutate(OralAcute=ifelse(OralPrev==1&OralAcute==1,0,OralAcute))
Experience <- Experience %>% mutate(OralAcute=ifelse(OralAcute=="-"&`CGRP Oral`==1, "1", OralAcute))

sum(as.numeric(Experience$weight))



data.frame(Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  group_by(Type, Rimegepant, `CGRP Oral`, `CGRP Injectable`, ) %>%
   # group_by(Type) %>%
  summarise(n=sum(as.numeric(weight))))


Experience <- Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  select(patient, weight, Type)


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-05-15"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, generic, drug_class, drug_group)

RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(class=ifelse(generic=="Rimegepant", "Rimegepant",
                                      ifelse(drug_class=="CGRP Oral", "CGRP Oral",
                                             ifelse(drug_class=="CGRP Injectable", "CGRP Injectable", drug_group))))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, class)
names(RIMUS23_Doses)[1] <- "patient"
unique(RIMUS23_Doses$class)



Experience <- Experience %>% left_join(RIMUS23_Doses) %>% drop_na() %>%
  filter(Type=="Acute_only") %>% mutate(Volume=as.numeric(weight)*as.numeric(days_sup)) %>%
  select(-c(weight, days_sup))

Experience <- Experience %>% select(-Type)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(generic=ifelse(generic=="Rimegepant", generic, "CGRP Oral")) %>% select(patid, generic, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[3] <- "OralAcute"
names(RIMUS23_Doses)[4] <- "OralPrev"
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(OralAcute=ifelse(OralPrev==1,0,OralAcute))
names(RIMUS23_Doses)[2] <- "class"
names(RIMUS23_Doses)[1] <- "patient"

Experience <- Experience %>% left_join(RIMUS23_Doses)
Experience[is.na(Experience)] <- 0

Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
  group_by(group, class) %>%  summarise(total=sum(as.numeric(Volume)))


# --------------------------------------------

# Acute vs prev , patients & volume per specialty V3 --------------------------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories





RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses %>% select(patid) %>% group_by(patid) %>% count() %>% filter(n>1)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[2] <- "OralAcute"
names(RIMUS23_Doses)[3] <- "OralPrev"


All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)
Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories) 
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
Experience <- RIMUS23_Drug_Histories

Experience <- Experience %>% left_join(RIMUS23_Doses, by=c("patient"="patid"))

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 

Rimegepan_patst <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% select(patient) %>% distinct()
Rimegepan_patst <- Rimegepan_patst %>% mutate(Rimegepant=1)

Experience <- Experience %>% left_join(Rimegepan_patst) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))

Experience$OralAcute <- as.character(Experience$OralAcute)
Experience$OralPrev <- as.character(Experience$OralPrev)

Experience[is.na(Experience)] <- "-"

Experience %>% group_by(OralAcute, OralPrev) %>% summarise(n=sum(as.numeric(weight)))
Experience  <- Experience %>% mutate(OralAcute=ifelse(OralPrev==1&OralAcute==1,0,OralAcute))
Experience <- Experience %>% mutate(OralAcute=ifelse(OralAcute=="-"&`CGRP Oral`==1, "1", OralAcute))

sum(as.numeric(Experience$weight))



data.frame(Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  group_by(Type, Rimegepant, `CGRP Oral`, `CGRP Injectable`, ) %>%
   # group_by(Type) %>%
  summarise(n=sum(as.numeric(weight))))



Experience <- Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  select(patient, weight, Type)


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt > "2022-05-15"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, generic, drug_class, drug_group,  provider, provcat)

RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(class=ifelse(generic=="Rimegepant", "Rimegepant",
                                      ifelse(drug_class=="CGRP Oral", "CGRP Oral",
                                             ifelse(drug_class=="CGRP Injectable", "CGRP Injectable", drug_group))))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, class, provider, provcat)
names(RIMUS23_Doses)[1] <- "patient"
unique(RIMUS23_Doses$class)




Experience <- Experience %>% left_join(RIMUS23_Doses) %>% drop_na() %>%
  #filter(Type=="Prev") %>%
  mutate(Volume=as.numeric(weight)*as.numeric(days_sup)) %>%
  select(-c(weight, days_sup))

# Experience <- Experience %>% select(-Type)

unique(Experience$class)



Unique_provcats <- fread("Unique_provcats.csv")

Experience <- Experience %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na()

unique(Experience$TYPE)

Experience <- Experience %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                ifelse(TYPE=="PRIMARY CARE"|TYPE=="INTERNAL MEDICINE"|TYPE=="INTERAL MEDICINE", "PCP", "OTHER")))


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 


RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(generic=ifelse(generic=="Rimegepant", generic, "CGRP Oral")) %>% select(patid, generic, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[3] <- "OralAcute"
names(RIMUS23_Doses)[4] <- "OralPrev"
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(OralAcute=ifelse(OralPrev==1,0,OralAcute))
names(RIMUS23_Doses)[2] <- "class"
names(RIMUS23_Doses)[1] <- "patient"

Experience <- Experience %>% left_join(RIMUS23_Doses)
Experience[is.na(Experience)] <- 0

data.frame(Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
    mutate(class=ifelse(class=="Rimegepant", "Rimegepant_Acute", class)) %>%
  group_by(Type, TYPE, group, class) %>%  summarise(total=sum(as.numeric(Volume)))
  )




# --------------------------------------------------
# Waterfall stocks and experience V3 -----------------
All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, X60)

string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Box=ifelse(grepl(string_Orals, X60), "O",
                                      ifelse(grepl(string_Mabs, X60), "I",
                                             ifelse(grepl(string_Preventive, X60) & grepl(string_Acute, X60), "K",
                                                    ifelse(grepl(string_Preventive, X60) & grepl(string_Symptomatic, X60), "Y",
                                                           ifelse(grepl(string_Preventive, X60), "P",
                                                                  ifelse(grepl(string_Acute, X60), "T",
                                                                         ifelse(grepl(string_Symptomatic, X60), "S", "X"))))))))


All_pats %>% left_join(RIMUS23_Drug_Histories) %>% group_by(group, Box) %>% summarise(n=sum(as.numeric(weight)))




RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories %>% filter(Month>=49 & grepl("136", Treat)) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
Lines <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()

All_pats %>% left_join(Lines) %>% mutate(n=ifelse(is.na(n),0,n)) %>% summarise(n=weighted.mean(n, as.numeric(weight)))
All_pats %>% left_join(Lines) %>% mutate(n=ifelse(is.na(n),0,n)) %>% group_by(group) %>% summarise(n=weighted.mean(n, as.numeric(weight)))


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, X60)

string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Box=ifelse(grepl(string_Orals, X60), "O",
                                      ifelse(grepl(string_Mabs, X60), "I",
                                             ifelse(grepl(string_Preventive, X60) & grepl(string_Acute, X60), "K",
                                                    ifelse(grepl(string_Preventive, X60) & grepl(string_Symptomatic, X60), "Y",
                                                           ifelse(grepl(string_Preventive, X60), "P",
                                                                  ifelse(grepl(string_Acute, X60), "T",
                                                                         ifelse(grepl(string_Symptomatic, X60), "S", "X"))))))))


All_pats %>% left_join(RIMUS23_Drug_Histories) %>% left_join(Lines) %>% mutate(n=ifelse(is.na(n),0,n))  %>% 
  filter(group=="ModSev") %>% group_by(Box  ) %>% summarise(n=weighted.mean(n, as.numeric(weight)))

Boxes <- RIMUS23_Drug_Histories

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month==60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
Mols <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()



Boxes %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% summarise(n=weighted.mean(n, as.numeric(weight)))
All_pats %>% left_join(Boxes) %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% group_by(group) %>% summarise(n=weighted.mean(n, as.numeric(weight)))
All_pats %>% left_join(Boxes) %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% filter(group=="ModSev") %>% group_by(Box) %>% summarise(n=weighted.mean(n, as.numeric(weight)))



All_pats %>% left_join(Boxes) %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n=ifelse(n<2,0,1)) %>% group_by(n) %>% summarise(pats=sum(as.numeric(weight)))
All_pats %>% left_join(Boxes) %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n=ifelse(n<2,0,1)) %>% group_by(group, n) %>% summarise(pats=sum(as.numeric(weight)))
All_pats %>% left_join(Boxes) %>% left_join(Mols) %>% mutate(n=ifelse(is.na(n),0,n)) %>% mutate(n=ifelse(n<2,0,1)) %>% filter(group=="ModSev") %>% group_by(Box, n) %>% summarise(pats=sum(as.numeric(weight)))



RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month==60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c( Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
Drugs_lookup <- fread("Drugs_lookup.csv")
unique(Drugs_lookup$drug_group)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup , by=c("Treat"="drug_id")) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()

All_pats %>% left_join(Boxes) %>% left_join(RIMUS23_Drug_Histories) %>%
  group_by(drug_group) %>% summarise(n=sum(as.numeric(weight)))


All_pats %>% left_join(Boxes)  %>% left_join(RIMUS23_Drug_Histories) %>%
  group_by(group, drug_group) %>% summarise(n=sum(as.numeric(weight))) %>% filter(group=="Mild")


All_pats %>% left_join(Boxes) %>% left_join(RIMUS23_Drug_Histories) %>%
  filter(group=="ModSev") %>%
  group_by(Box, drug_group) %>% summarise(n=sum(as.numeric(weight)))  %>%
  spread(key=Box  , value=n)

# --------------------------
# Rimegepant patients waterfall ON May 2023 V3  --------------
Drugs_lookup <- fread("Drugs_lookup.csv")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, X60) 

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(grepl("136", X60))

sum(as.numeric(RIMUS23_Drug_Histories$weight))

RIMUS23_Drug_Histories$Rimegepant <- 1

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Combo=ifelse(grepl(",", X60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Mabs=ifelse(grepl(string_Mabs, X60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Acute=ifelse(grepl(string_Acute, X60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Symptomatic=ifelse(grepl(string_Symptomatic, X60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Preventive=ifelse(grepl(string_Preventive, X60),1,0))

RIMUS23_Drug_Histories %>% 
  mutate(AcuteSympt=ifelse(Acute==1|Symptomatic==1,1,0)) %>%
  group_by(Rimegepant, Combo, Mabs,  Preventive, AcuteSympt) %>%
  summarise(n=sum(as.numeric(weight)))


# ---------------------------
# Sources of Inflows / Outflows V3 ----------------------------
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month)

Drugs_lookup <- fread("Drugs_lookup.csv")

string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Box=ifelse(grepl(string_Orals, Treat), "O",
                                      ifelse(grepl(string_Mabs, Treat), "I",
                                             ifelse(grepl(string_Preventive, Treat) & grepl(string_Acute, Treat), "K",
                                                    ifelse(grepl(string_Preventive, Treat) & grepl(string_Symptomatic, Treat), "Y",
                                                           ifelse(grepl(string_Preventive, Treat), "P",
                                                                  ifelse(grepl(string_Acute, Treat), "T",
                                                                         ifelse(grepl(string_Symptomatic, Treat), "S", "X"))))))))


Boxes_V3 <- RIMUS23_Drug_Histories

fwrite(Boxes_V3, "Boxes_V3.txt")


data.frame(RIMUS23_Drug_Histories %>%
  group_by(patient) %>% filter(!grepl("136", Treat)&grepl("136", lead(Treat))) %>% ungroup() %>%
    group_by(Month, Box)  %>% summarise(n=sum(as.numeric(weight))) %>%
    spread(key=Box, value=n)
  )


# ------------
# Rimegepant patients waterfall ON May 2023 V3  Acute vs Preventive Rimegepant Pats --------------


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant") %>% select(patid, weight, mean)
names(RIMUS23_Doses)[1] <- "patient"


Drugs_lookup <- fread("Drugs_lookup.csv")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, X60) 

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(grepl("136", X60))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(RIMUS23_Doses %>% filter(mean=="Prev") %>% select(patient))

sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 51916.7

RIMUS23_Drug_Histories$Rimegepant <- 1

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Combo=ifelse(grepl(",", X60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Mabs=ifelse(grepl(string_Mabs, X60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Acute=ifelse(grepl(string_Acute, X60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Symptomatic=ifelse(grepl(string_Symptomatic, X60),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Preventive=ifelse(grepl(string_Preventive, X60),1,0))

RIMUS23_Drug_Histories %>% 
  mutate(AcuteSympt=ifelse(Acute==1|Symptomatic==1,1,0)) %>%
  group_by(Rimegepant, Combo, Mabs,  Preventive, AcuteSympt) %>%
  summarise(n=sum(as.numeric(weight)))


# ------------------------------
# Segmentation source of business V3 ------------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
Rimegepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month) %>% distinct()
names(Rimegepant_Pats)[2] <- "First_Rimegepant"

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Rimegepant_Pats %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(drug_group=ifelse(is.na(drug_group), "Lapsed", drug_group)) %>%
  mutate(exp=1) %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

RIMUS23_Drug_Histories <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month) %>%
  group_by(patient) %>% mutate(CumCGRP=cumsum(`CGRP Injectable`==1|`CGRP Oral`==1)) %>%
  mutate(CumCGRP=ifelse(CumCGRP==0,0,1))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  group_by(patient) %>% mutate(CumPrev=cumsum(Preventive ==1)) %>%
  mutate(CumPrev=ifelse(CumPrev==0,0,1))

RIMUS23_Drug_Histories %>% filter(First_Rimegepant>=49 & First_Rimegepant<=60 ) %>% select(patient, weight) %>% distinct() %>% 
  ungroup() %>% summarise(n=sum(as.numeric(weight))) # 274795 # 106942 # 50921


RIMUS23_Drug_Histories %>% filter(First_Rimegepant>=49 & First_Rimegepant<=60 & Month==(First_Rimegepant-1) & CumCGRP==1) %>%
  select(patient, weight)  %>% distinct() %>% 
  ungroup() %>% summarise(n=sum(as.numeric(weight)))  # 94595 # 46501 # 23812
 
CGRP_exp <- RIMUS23_Drug_Histories %>% filter(First_Rimegepant>=49 & First_Rimegepant<=60 & Month==(First_Rimegepant-1) & CumCGRP==1) %>%
  select(patient, weight)  %>% distinct() 

RIMUS23_Drug_Histories %>%  filter(First_Rimegepant>=49 & First_Rimegepant<=60   & Month==(First_Rimegepant-1) & CumPrev==1) %>% anti_join(CGRP_exp) %>%
  select(patient, weight, Lapsed)  %>% distinct() %>% ungroup() %>%
  group_by(Lapsed) %>%
  summarise(n=sum(as.numeric(weight)))


Prev_exp <- RIMUS23_Drug_Histories %>% filter(First_Rimegepant>=49 & First_Rimegepant<=60  & Month==(First_Rimegepant-1) & CumPrev==1) %>% anti_join(CGRP_exp) %>%
  select(patient, weight) %>% distinct()

RIMUS23_Drug_Histories %>% anti_join(Prev_exp) %>% filter(First_Rimegepant>=49 & First_Rimegepant<=60 & Month==(First_Rimegepant-1)) %>%  anti_join(CGRP_exp) %>%
  select(patient, weight, Lapsed)  %>% distinct() %>% ungroup() %>%
  group_by(Lapsed) %>%
  summarise(n=sum(as.numeric(weight)))


# --------------------

# New architecture / flows / stocks ----------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")

string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% filter(group=="ModSev") %>% select(patient) %>%  left_join(RIMUS23_Drug_Histories)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Preventive=ifelse(grepl(string_Preventive, Treat),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(AcuteSympt=ifelse(grepl(string_Acute, Treat)|grepl(string_Symptomatic, Treat),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Rimeg=ifelse(grepl("136", Treat),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(OralCGRP=ifelse(grepl(string_Orals, Treat),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(InjCGRP=ifelse(grepl(string_Mabs, Treat),1,0))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  group_by(patient) %>% mutate(CumOralCGRP=cumsum(OralCGRP ==1)) %>%
  mutate(CumOralCGRP=ifelse(CumOralCGRP==0,0,1)) %>% ungroup()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  group_by(patient) %>% mutate(CumInjCGRP=cumsum(InjCGRP ==1)) %>%
  mutate(CumInjCGRP=ifelse(CumInjCGRP==0,0,1))  %>% ungroup()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Stock=ifelse(Rimeg==1, "Rime",
                                               ifelse(OralCGRP==1, "Orals",
                                                      ifelse(InjCGRP==1, "Injs",
                                                             ifelse(CumOralCGRP==1, "OralExp",
                                                                    ifelse(CumInjCGRP==1,"InjExp",
                                                                           ifelse(Preventive==1, "Prevs",
                                                                                  ifelse(AcuteSympt==1, "Acutes", "Lapsed"))))))))

RIMUS23_Drug_Histories %>% filter(Month==60) %>% group_by(Stock) %>% summarise(n=sum(as.numeric(weight))) 

RIMUS23_Drug_Histories %>% filter(Month==48) %>% group_by(Stock) %>% summarise(n=sum(as.numeric(weight))) 



Stocks <- RIMUS23_Drug_Histories %>% select(patient, weight, Treat, Month, Stock)

Stocks <- Stocks %>% left_join(Stocks %>% mutate(Month=Month-1) %>% rename("Stock2"="Stock") %>% rename("Treat2"="Treat"))
Stocks <- Stocks %>% select(patient, weight, Month, Treat, Treat2, Stock, Stock2)
Stocks <- Stocks %>% filter(Month!=60) %>% mutate(Month=Month+1)


Stocks %>% filter(Month>=36&Month<48) %>% filter(Treat!=Treat2)  %>% group_by(Stock, Stock2) %>% 
  summarise(n=sum(as.numeric(weight))) %>% spread(key=Stock2, value=n)


# -------------------

# Physician profiles V3 --------------------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories



Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))

RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider , provcat, drug_group) %>% distinct()
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)


RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date   =as.Date(date)) %>% filter(date>="2022-06-01"&date<="2023-05-31")
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(provider, provcat) %>% distinct()
RIMUS23_Migraine_Dxs$Dx <- 1


RIMUS23_Migraine_Dxs %>% group_by(provcat) %>% count() %>% arrange(-n)


All_provs <- RIMUS23_Doses %>% select(provider, provcat) %>% full_join(RIMUS23_Migraine_Dxs %>% select(provider, provcat)) %>% distinct() %>%
  left_join(RIMUS23_Migraine_Dxs %>% select(-provcat)) %>% left_join(RIMUS23_Doses %>% select(-provcat))

All_provs[is.na(All_provs)] <- 0

All_provs <- All_provs %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

All_provs <- All_provs %>% filter(TYPE!="EXCLUDE") %>% drop_na()


All_provs <- All_provs %>% mutate(Physician_Profile=ifelse(Dx==1&`CGRP Injectable`==0&`CGRP Oral`==0&Preventive==0&Symptomatic==0&Triptans==0, "Dx_Only",
                                              ifelse(`CGRP Injectable`==1|`CGRP Oral`==1, "Rx_CGRP",
                                                     ifelse(Preventive==1, "Rx_Preventive", 
                                                            ifelse(Triptans==1, "Rx_Triptan",
                                                                   ifelse(Symptomatic==1, "Rx_Sympt","none"))))))


All_provs %>% group_by(Physician_Profile) %>% count() %>% mutate(n=n/278197)

All_provs %>% group_by(TYPE) %>% count()

All_provs %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) %>%
  group_by( TYPE, Physician_Profile) %>% count() %>% 
  mutate(n=ifelse(TYPE=="INTERNAL MEDICINE", n/40003,
                  ifelse(TYPE=="NEUROLOGY", n/17142,
                         ifelse(TYPE=="OBG", n/5653,
                                ifelse(TYPE=="OTHER HCP", n/46735,
                                       ifelse(TYPE=="OTHER PHYSICIAN", n/91860,
                                              ifelse(TYPE=="PRIMARY CARE", n/70993,
                                                     ifelse(TYPE=="PSYCHIATRY",n/5811,NA)))))))) %>%
  spread(key=TYPE, value=n)




All_provs <- All_provs %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) 


All_provs


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")


N_pats <- RIMUS23_Doses %>% select(provider, patid) %>% distinct() %>% group_by(provider) %>% count() %>% rename("N_pats"="n")
N_Scripts <- RIMUS23_Doses %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_Scripts"="n")
N_Prev <- RIMUS23_Doses %>% filter(drug_group=="Preventive") %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_Prev"="n")
N_Triptan <- RIMUS23_Doses %>% filter(drug_group=="Triptans") %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_Triptan"="n")


All_provs_2 <- All_provs %>% left_join(N_pats) %>% left_join(N_Scripts) %>% left_join(N_Prev) %>% left_join(N_Triptan)

All_provs_2[is.na(All_provs_2)] <- 0




All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_pats))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_pats))



All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Scripts))



All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Prev/N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Prev/N_Scripts))



All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Triptan/N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Triptan/N_Scripts))


All_provs_2


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant") %>% select(provider) %>% distinct()


RIMUS23_Doses$Rimegepant <- 1


All_provs_2 <- All_provs_2 %>% left_join(RIMUS23_Doses) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))

All_provs_2 <- All_provs_2 %>% mutate(CGRP=ifelse(`CGRP Injectable`==1|`CGRP Oral`==1, 1,0))


All_provs_2 %>% group_by(Rimegepant, CGRP) %>% count()


All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_pats))



All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Scripts))




All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Scripts/N_pats, na.rm=T))





All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Prev))



All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Triptan))



All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=factor(group, levels=c("Other Rx", "Other CGRP", "Rimegepant"))) %>%
  ggplot(aes(N_Scripts, colour=group, fill=group)) +
  xlim(0, 250) +
  geom_density(alpha=0.8, adjust=8) +
  theme_minimal() +
  scale_colour_manual(values=c("#B2D1DC", "#FFEFCA", "#FFBFBF")) +
  scale_fill_manual(values=c("#B2D1DC", "#FFEFCA", "#FFBFBF")) +
  xlab("\n Number of Scripts per physician") + ylab("Physician density \n (kernel smoothing)\n")



All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant")))  %>%
  group_by(group) %>% count()

All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant"))) %>%
  ggplot(aes(N_Scripts/N_pats, colour=group, fill=group)) +
   xlim(0, 30) +
  geom_density(alpha=0.6, adjust=8) +
  theme_minimal() +
  scale_colour_manual(values=c("#B2D1DC", "#448196", "#FFEFCA", "#FFBFBF")) +
  scale_fill_manual(values=c("#B2D1DC", "#448196", "#FFEFCA", "#FFBFBF")) +
  xlab("\n Number of Scripts/Patient per physician") + ylab("Physician density \n (kernel smoothing)\n")




All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant")))  %>%
  filter(group %in% c("Other Rx", "High Vol Other Rx")) %>%
  select(provider, group, N_Triptan, N_Scripts, N_Prev) %>%
  mutate(N_Triptan=N_Triptan/N_Scripts) %>% mutate(N_Prev=N_Prev/N_Scripts) %>%
  group_by(group) %>% summarise(meanP=mean(N_Prev))






All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant")))  %>%
  filter(group %in% c("Other Rx", "High Vol Other Rx")) %>%
  select(provider, group, N_Triptan, N_Scripts, N_Prev) %>%
  mutate(N_Triptan=N_Triptan/N_Scripts) %>% mutate(N_Triptan=N_Triptan/N_Scripts) %>%
  ggplot(aes(N_Scripts, N_Triptan)) +
  geom_smooth()



All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant")))  %>%
  filter(group %in% c("Other Rx", "High Vol Other Rx")) %>%
  select(provider, group, TYPE) %>%
  group_by(group, TYPE) %>% count() %>%
  spread(key=TYPE , value=n)







All_provs_2 %>% group_by(Rimegepant, CGRP, TYPE) %>% count() %>%
  spread(key=TYPE, value=n)



All_provs_2 %>% group_by(Rimegepant, CGRP, TYPE) %>% summarise(n=sum(N_Scripts)) %>%
  spread(key=TYPE, value=n)



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")

CGRP_scripts <- RIMUS23_Doses %>% filter(grepl("CGRP", drug_group)) %>% select(provider, generic) %>%  group_by(provider, generic) %>% count()


temp <- All_provs_2 %>% filter(Rimegepant==0 & CGRP==1) %>%
  select(provider) %>%
  left_join(CGRP_scripts) %>% ungroup() %>% distinct() %>%
  spread(key=generic, value=n)

temp[is.na(temp)] <- 0

temp <- gather(temp, generic, n, Atogepant:Ubrogepant) 


temp %>% group_by(generic) %>% summarise(mean=mean(n))



temp %>%
  mutate(generic=factor(generic, levels=c("Atogepant", "Ubrogepant", "Eptinezumab", "Fremanezumab", "Galcanezumab", "Erenumab"))) %>%
  ggplot(aes(n, colour=generic, fill=generic)) +
  geom_density(alpha=0.5, adjust=2) +
  xlim(0,5) +
  xlab("\n Number of Scripts per Physician \n (Other CGRP Physicians/No Rimegepant)") +
  ylab("Physician density \n (Kernel smoothin) \n") +
  theme_minimal() +
  facet_wrap(~generic, ncol = 1, scales="free_y") +
  ggsci::scale_fill_jama() +
  ggsci::scale_colour_jama()


# ----------------
# 2 box model Rimegepant flows---------------------
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI, generic, drug_id, drug_class, drug_group))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2021-06-16"&from_dt <= "2023-05-15") %>% filter(days_sup!="")


RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, from_dt) 

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()

data.frame(RIMUS23_Doses) 

# RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  #filter(from_dt<="2022-06-16") %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>%  ungroup() %>%
  mutate(diff=as.numeric(as.Date("2023-05-15")-from_dt)) %>% 
  filter(diff<=92) %>%
  full_join(pats_m48) %>%  rename("m60"="rate") %>%
  group_by(m48, m60) %>% summarise(n=sum(as.numeric(weight)))
   

pats_m48 <- RIMUS23_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  filter(from_dt<="2022-06-16") %>%
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>%  ungroup() %>%
  mutate(diff=as.numeric(as.Date("2022-06-16")-from_dt)) %>% 
  filter(diff<=92) %>% select(patid, weight, rate) %>% rename("m48"="rate")


RIMUS23_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  #group_by(patid) %>%  filter(from_dt==max(from_dt)) %>% filter(qty==max(qty)) %>% slice(1) %>% ungroup() %>%
  #mutate(diff=as.numeric(as.Date("2023-05-15")-from_dt)) %>% 
  #filter(diff<=122) %>%
  group_by(patid, weight, rate) %>% count() %>%
  spread(key=rate, value=n) %>%
  mutate(Acute=ifelse(is.na(Acute),0,Acute)) %>%
  mutate(Prev=ifelse(is.na(Prev),0,Prev)) %>%
  ggplot(aes(Acute, Prev)) +
  geom_jitter(alpha=0.5)
  
   


RIMUS23_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>%
  group_by(patid) %>% filter(rate=="Prev"&lag(rate)=="Acute") %>% ungroup() %>%
  summarise(n=sum(as.numeric(weight)))



RIMUS23_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  group_by(patid) %>% filter(from_dt==max(from_dt)) %>%  ungroup() %>%
  mutate(diff=as.numeric(as.Date("2023-05-15")-from_dt)) %>% 
  filter(diff<=92) %>%
  group_by(rate) %>% summarise(n=sum(as.numeric(weight)))
   




RIMUS23_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  group_by(patid) %>%  filter(from_dt==max(from_dt) ) %>% filter(from_dt>="2022-06-16") %>% 
  select(patid, weight, rate) %>% rename("Final"="rate") %>%
  full_join(
    RIMUS23_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
  group_by(patid) %>%  filter(from_dt==min(from_dt)) %>% filter(from_dt>="2022-06-16") %>% 
  select(patid, weight, rate) %>% rename("First"="rate")
  ) %>%
  group_by(First, Final) %>% summarise(n=sum(as.numeric(weight)))




RIMUS23_Doses %>% mutate(rate=ifelse(rate>=13, "Prev", "Acute")) %>% 
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

# Time to CGRP ~ Migraine Onset -------------

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, weight, migraine_onset) %>% mutate(migraine_onset=as.Date(migraine_onset))
RIMUS23_Demographics <- RIMUS23_Demographics %>% drop_na()

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(grepl("CGRP", drug_group)) %>% select(patid, weight, generic, drug_group, from_dt) %>% distinct() %>% mutate(from_dt=as.Date(from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, weight, generic, drug_group) %>% filter(from_dt==min(from_dt)) %>% slice(1) %>% ungroup()
RIMUS23_Demographics <- RIMUS23_Demographics %>% left_join(RIMUS23_Doses) %>% drop_na()

RIMUS23_Demographics <- RIMUS23_Demographics %>% mutate(elapsed=as.numeric(from_dt-migraine_onset))

RIMUS23_Demographics %>%
  mutate(elapsed=ifelse(elapsed<0,0, elapsed)) %>%
 filter(drug_group=="CGRP Injectable") %>%
  ggplot(aes(migraine_onset, elapsed/30.5, colour=generic, fill=generic)) +
  geom_smooth() +
 # facet_wrap(~drug_group) +
  theme_minimal() +
  xlim=c("2020-01-01", "2022-12-31") +
  xlab("\n Exact Migraine Onset Date") + 
  ylab("Number of Elapsed Months \n from Migraine Onset to CGRP Initiation \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() 
  


All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, weight, migraine_onset) %>% mutate(migraine_onset=as.Date(migraine_onset))
RIMUS23_Demographics <- RIMUS23_Demographics %>% drop_na()
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(grepl("CGRP", drug_group)) %>% select(patid, weight, generic, drug_group) %>% distinct() 
RIMUS23_Doses <- RIMUS23_Doses %>% select(-drug_group) %>% mutate(exp=1) %>% spread(key=generic, value=exp)
RIMUS23_Demographics <- RIMUS23_Demographics %>% left_join(RIMUS23_Doses) 
RIMUS23_Demographics[is.na(RIMUS23_Demographics)] <- 0

RIMUS23_Demographics %>% filter(grepl("2017", as.character(migraine_onset))) %>% 
    inner_join(All_pats %>% filter(group=="ModSev") %>% select(patient), by=c("patid"="patient")) %>%
  summarise(n=mean(Rimegepant))

RIMUS23_Demographics %>% filter(grepl("2023", as.character(migraine_onset))) %>% 
      inner_join(All_pats %>% filter(group=="ModSev") %>% select(patient), by=c("patid"="patient")) %>%
  summarise(n=mean(Rimegepant))

names(RIMUS23_Demographics)

RIMUS23_Demographics %>%
  inner_join(All_pats %>% filter(group=="ModSev") %>% select(patient), by=c("patid"="patient")) %>%
  gather( generic, Treat, Atogepant:Ubrogepant, factor_key=TRUE) %>%
  mutate(drug_group=ifelse(grepl("ant", generic), "CGRP Oral", "CGRP Injectable")) %>%
  filter(drug_group=="CGRP Injectable") %>%
  ggplot(aes(migraine_onset, Treat, colour=generic, fill=generic)) +
  stat_smooth(method="glm",  se=TRUE, method.args = list(family=binomial)) +
  theme_minimal() +
  xlab("\n Exact Migraine Onset Date") + 
  ylab("Probability of having been started ON a CGRP \n") +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() 


# ---------------
  

# Average supply days relative to CGRP initiation ---------------------------------------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(grepl("CGRP", drug_group)) %>% select(patid, weight, generic, drug_group, from_dt) %>% distinct() %>% mutate(from_dt=as.Date(from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, weight, generic, drug_group) %>% filter(from_dt==min(from_dt)) %>% slice(1) %>% ungroup()
FirstCGRPs <- RIMUS23_Doses
names(FirstCGRPs)[3] <- "CGRP"
names(FirstCGRPs)[4] <- "drug_class"
names(FirstCGRPs)[5] <- "FirstCGRP"

FirstCGRPs <- FirstCGRPs %>% select(-drug_class)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, generic, drug_class, from_dt, days_sup)
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, weight, generic, drug_class, from_dt) %>% summarise(days_sup=sum(as.numeric(days_sup))) %>% ungroup()


FirstCGRPs %>% left_join(RIMUS23_Doses) %>% 
  filter(CGRP=="Ubrogepant") %>%
  mutate(elapsed=as.numeric(as.Date(from_dt)-as.Date(FirstCGRP))) %>%
  ggplot(aes(elapsed/30.5, days_sup, colour=drug_class, fill=drug_class)) +
  xlim(-30,30) + ylim(0,60) +
  geom_smooth(show.legend = FALSE) +
  facet_wrap(~drug_class, scales="free_y") +
  theme_minimal() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  xlab("\n Number of Elapsed Months \n from Ubrogepant Initiation") +
  ylab("Supply Days \n")
  


# V2 
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(grepl("CGRP", drug_group)) %>% filter(generic=="Rimegepant") %>% select(patid, weight, from_dt) %>% distinct() %>% mutate(from_dt=as.Date(from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, weight) %>% filter(from_dt==min(from_dt)) %>% slice(1) %>% ungroup()
FirstCGRPs <- RIMUS23_Doses
names(FirstCGRPs)[3] <- "FirstCGRP"

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, drug_class, from_dt, days_sup)


data.frame(FirstCGRPs %>% left_join(RIMUS23_Doses) %>% mutate(from_dt=as.Date(from_dt)) %>%
  mutate(diff=as.numeric(from_dt-FirstCGRP)) %>%
  filter(diff>=(-183) & diff<=183) %>%
  mutate(diff=ifelse(diff<0, "Before", "After")) %>%
    group_by(drug_class, diff) %>% summarise(n=sum(as.numeric(weight)*as.numeric(days_sup))) %>%
  spread(key=diff, value=n)) %>%
  mutate(diff=After-Before)






# ------------------
# Rimegepant Physicians   ---------------------------------------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")  %>% mutate(from_dt=as.Date(from_dt)) %>% 
  filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% select(provider) %>% distinct() %>% left_join(RIMUS23_Doses)

RIMUS23_Doses <- RIMUS23_Doses  %>% mutate(from_dt=as.Date(from_dt)) %>%  filter(from_dt>="2022-06-16"&from_dt<="2023-05-15")

unique(RIMUS23_Doses$drug_class)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="Triptan"|drug_class=="Ergot"|drug_class=="Ditan"|grepl("CGRP", drug_class))

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") 

RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE")  %>% 
  select(provider, patid) %>% distinct() %>% group_by(provider) %>% count() %>% ungroup() %>% summarise(mean=mean(n)) # 4.21 # 3.43

RIMUS23_Doses%>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>%
  select(provider) %>%  group_by(provider) %>% count() %>% ungroup() %>% summarise(mean=mean(n)) # 31.8 # 28.2



# ------------------
# Characterise rimegepant source of flows ------------------------------

Boxes_V3 <- fread("Boxes_V3.txt")

Inflow_pats <- data.frame(Boxes_V3 %>%
  group_by(patient) %>% filter(!grepl("136", Treat)&grepl("136", lead(Treat))) %>% ungroup() %>%
    select(patient, weight, Box) %>% distinct())



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() %>% select(patid, mean)

Inflow_pats %>% mutate(patient=as.numeric(patient)) %>% 
  inner_join(RIMUS23_Doses %>% mutate(patid=as.numeric(patid)), by=c("patient"="patid")) %>%
  group_by(Box, mean) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=mean, value=n) %>% mutate(perc=Acute/(Acute+Prev))


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") 
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% summarise(n=sum(as.numeric(days_sup)))



Inflow_pats %>% mutate(patient=as.numeric(patient)) %>% 
  inner_join(RIMUS23_Doses %>% mutate(patid=as.numeric(patid)), by=c("patient"="patid")) %>%
  group_by(Box) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))



RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49 & Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()


Inflow_pats %>% mutate(patient=as.numeric(patient)) %>% 
  inner_join(RIMUS23_Drug_Histories %>% mutate(patient=as.numeric(patient))) %>%
  group_by(Box) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))



Unique_provcats <- fread("Unique_provcats.csv")
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, provider , provcat) %>% distinct()
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(TYPE=="NEUROLOGY") %>% select(patid) %>% distinct() %>% mutate(NERUO=1)


Inflow_pats %>% mutate(patient=as.numeric(patient)) %>% 
  left_join(RIMUS23_Doses %>% mutate(patid=as.numeric(patid)), by=c("patient"="patid")) %>%
  group_by(Box, NERUO) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=NERUO, value=n) %>%
  mutate(perc=`1`/(`1`+`<NA>`))
  




# ---------------------
# Generate long flows table  V3 --------------


Boxes_V3 <- fread("Boxes_V3.txt")
Boxes_V3 <- Boxes_V3 %>% select(-Treat) %>% spread(key=Month, value=Box)

names(Boxes_V3)[3:62] <- paste0("month", names(Boxes_V3)[3:62])

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>%  spread(key=Month, value=Treat)
names(RIMUS23_Drug_Histories)[3:62] <- paste0("month", names(RIMUS23_Drug_Histories)[3:62])


MIG_Drug_Histories <- RIMUS23_Drug_Histories
MIG_Box_Histories <- Boxes_V3

setDT(MIG_Drug_Histories)
setDT(MIG_Box_Histories)

# Flows table in long format
flMIG <- MIG_Drug_Histories
#flMIG <- flMIG[,disease := NULL]

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
flMIG <- flMIG[, disease := "MIG US"]
flMIG <- flMIG[,c(12,1:11)]

# Bring Therapy classes (Stocks) to the table***********************************
#MIG_Box_Histories <- MIG_Box_Histories[,disease := NULL]
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

MIG_Box_Histories$patient <- as.numeric(MIG_Box_Histories$patient)
flMIG$patient <- as.numeric(flMIG$patient)

flMIG <- MIG_Box_Histories[,.(patient,p,s)][flMIG, on = .(patient, p = p1)]
names(flMIG)[c(2,3)] <- c("p1","s1")
flMIG <- MIG_Box_Histories[,.(patient,p,s)][flMIG, on = .(patient, p = p2)]
names(flMIG)[c(2,3)] <- c("p2","s2")

flMIG <- flMIG[,.(disease, patient, weight, p1, p2, v1, v2, s1, s2, p1_RxExp, flow, stops, starts, re_starts)]
names(flMIG)[c(6,7)] <- c("d1","d2")

fwrite(flMIG,"MIG_Flows_Aux._Long_v3.txt")

flMIG %>% filter(p1>=49) %>% filter(grepl("136", d1) & !grepl("136", d2)) %>%
  summarise(n=sum(as.numeric(weight)))


# ---------------------------
# Sources of Inflows V3 ---------------
All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

flMIG <- fread("MIG_Flows_Aux._Long_v3.txt")


flMIG %>% filter(as.numeric(p2)>=49) %>% filter(flow==1) %>% filter(s1!=s2) %>%
  group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=s2, value=n)

# -----------------
# Pathways to Rimegepant 12m prior rank V3 -------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
Rimegepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month, weight) %>% distinct()
names(Rimegepant_Pats)[2] <- "First_Rimegepant"


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
Ubrogepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("137", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month, weight) %>% distinct()
names(Ubrogepant_Pats)[2] <- "First_Ubrogepant"

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Rimegepant)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(lapsed=First_Rimegepant-Month) %>% filter(lapsed<=12)


flMIG <- fread("MIG_Flows_Aux._Long_v3.txt", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- flMIG %>% select(patient, p2, flow)

Flows <- RIMUS23_Drug_Histories %>% left_join(flMIG) %>% filter(p2==Month) %>% group_by(patient) %>% summarise(flow=sum(as.numeric(flow))) %>%
  filter(flow!=0) %>% select(patient) %>% mutate(flow=1)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Flows)  %>% select(patient, weight, Treat, flow)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group, flow) %>% distinct() 
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
names(RIMUS23_Drug_Histories)[9] <- "Lapsed"

sum(as.numeric(RIMUS23_Drug_Histories$weight))

RIMUS23_Drug_Histories %>% mutate(group=ifelse(`CGRP Oral`==1, "Oral",
                                               ifelse(`CGRP Injectable`==1, "Inj",
                                                      ifelse(flow==1&Preventive==1, "FlowPrev",
                                                             ifelse(flow==1&Preventive==0,"FlowNoPrev",
                                                                    ifelse(flow==0&Preventive==1&Triptans==1, "Prev+Trip",
                                                                           ifelse(flow==0&Preventive==1&Symptomatic==1, "Prev+Sympt",
                                                                                  ifelse(flow==0&Preventive==1, "Prev",
                                                                                         ifelse(flow==0&Lapsed==1, "Lapsed", "Other"))))))))) %>%
  group_by(group) %>% summarise(n=sum(as.numeric(weight)/433803.9))






RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))


RIMUS23_Drug_Histories <- Ubrogepant_Pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month<First_Ubrogepant)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(lapsed=First_Ubrogepant-Month) %>% filter(lapsed<=12)


flMIG <- fread("MIG_Flows_Aux._Long_v3.txt", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- flMIG %>% select(patient, p2, flow)

Flows <- RIMUS23_Drug_Histories %>% left_join(flMIG) %>% filter(p2==Month) %>% group_by(patient) %>% summarise(flow=sum(as.numeric(flow))) %>%
  filter(flow!=0) %>% select(patient) %>% mutate(flow=1)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Flows)  %>% select(patient, weight, Treat, flow)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group, flow) %>% distinct() 
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
names(RIMUS23_Drug_Histories)[9] <- "Lapsed"

sum(as.numeric(RIMUS23_Drug_Histories$weight))

RIMUS23_Drug_Histories %>% mutate(group=ifelse(`CGRP Oral`==1, "Oral",
                                               ifelse(`CGRP Injectable`==1, "Inj",
                                                      ifelse(flow==1&Preventive==1, "FlowPrev",
                                                             ifelse(flow==1&Preventive==0,"FlowNoPrev",
                                                                    ifelse(flow==0&Preventive==1&Triptans==1, "Prev+Trip",
                                                                           ifelse(flow==0&Preventive==1&Symptomatic==1, "Prev+Sympt",
                                                                                  ifelse(flow==0&Preventive==1, "Prev",
                                                                                         ifelse(flow==0&Lapsed==1, "Lapsed", "Other"))))))))) %>%
  group_by(group) %>% summarise(n=sum(as.numeric(weight)/532916.4))






# -----------------
# Class Penetrance before and after Oral CGRP Start V3 ------------------------------------------------------------------------------
flMIG <- fread("MIG_Flows_Aux._Long_v3.txt")
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with ORAL CGRP after m48
MIG_CGRP_ORAL_After_48 <- flMIG %>% 
  filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2)) %>% select(patient) %>% distinct()

#pats with ORAL CGRP before m48
MIG_CGRP_ORAL_Before_48 <- flMIG %>% 
  filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2) | grepl("135",d1) | grepl("136",d1) | grepl("137",d1)) %>% select(patient) %>% distinct()

#pats with ORAL CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% anti_join(MIG_CGRP_ORAL_Before_48)
rm(MIG_CGRP_ORAL_Before_48)

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join(flMIG)

# first occurence of ORAL CGRP, month before and after as well, remove those who started on m60
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient) %>% filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1) %>% mutate(after=p2+1) %>% filter(after <= 60)


# collpase month to single column
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% arrange(patient, n)

# add drugs
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((flMIG %>% select(patient, p2, s2, d2)), by=c("patient"="patient", "n"="p2"))

# to long, each molecule
MIG_CGRP_ORAL_After_48 <- separate_rows(MIG_CGRP_ORAL_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_ORAL_After_48)[4] <- "drug_id"


# drugs look up
Drugs_lookup <- fread("Drugs_lookup.csv")
Drugs_lookup <- Drugs_lookup %>% select(drug_id, drug_class) %>% mutate(drug_id=as.character(drug_id))

# add drug classes
MIG_CGRP_ORAL_After_48<- MIG_CGRP_ORAL_After_48 %>% left_join(Drugs_lookup) %>% arrange(patient)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% mutate(n=ifelse(n==min(n), "Before", 
                                                                     ifelse(n==max(n),"After", "Current")))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((flMIG %>% select(patient, weight) %>% distinct()))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% select(patient, n, s2, drug_class, weight) %>% distinct()
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% mutate(drug_class = ifelse(is.na(drug_class), "Lapsed", drug_class))

# Total weights = 179529
MIG_CGRP_ORAL_After_48 %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight))

MIG_CGRP_ORAL_After_48 %>% select(patient, weight,n) %>% distinct() %>% group_by(patient) %>% count() %>%
  filter(n==3)


# 399053

before_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before") %>% group_by(n, drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange(-penetrance) %>%
             mutate(drug_class= as.factor(drug_class)))

current_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Current") %>% group_by(n,  drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange(-penetrance) %>%
             mutate(drug_class= as.factor(drug_class))) 

after_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After") %>% group_by(n,  drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange( -penetrance) %>%
             mutate(drug_class= as.factor(drug_class))) 


before_penetrance %>% select(-n) %>% rename("before"="penetrance") %>%
  full_join(current_penetrance %>% select(-n) %>% rename("current"="penetrance")) %>%
  full_join(after_penetrance %>% select(-n) %>% rename("after"="penetrance")) 

# ----------------------
# Physician profiles V3 --------------------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories



Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))

RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider , provcat, drug_group) %>% distinct()
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)


RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date   =as.Date(date)) %>% filter(date>="2022-06-01"&date<="2023-05-31")
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(provider, provcat) %>% distinct()
RIMUS23_Migraine_Dxs$Dx <- 1


RIMUS23_Migraine_Dxs %>% group_by(provcat) %>% count() %>% arrange(-n)


All_provs <- RIMUS23_Doses %>% select(provider, provcat) %>% full_join(RIMUS23_Migraine_Dxs %>% select(provider, provcat)) %>% distinct() %>%
  left_join(RIMUS23_Migraine_Dxs %>% select(-provcat)) %>% left_join(RIMUS23_Doses %>% select(-provcat))

All_provs[is.na(All_provs)] <- 0

All_provs <- All_provs %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

All_provs <- All_provs %>% filter(TYPE!="EXCLUDE") %>% drop_na()


All_provs <- All_provs %>% mutate(Physician_Profile=ifelse(Dx==1&`CGRP Injectable`==0&`CGRP Oral`==0&Preventive==0&Symptomatic==0&Triptans==0, "Dx_Only",
                                              ifelse(`CGRP Injectable`==1|`CGRP Oral`==1, "Rx_CGRP",
                                                     ifelse(Preventive==1, "Rx_Preventive", 
                                                            ifelse(Triptans==1, "Rx_Triptan",
                                                                   ifelse(Symptomatic==1, "Rx_Sympt","none"))))))


All_provs %>% group_by(Physician_Profile) %>% count() %>% mutate(n=n/278197)

All_provs %>% group_by(TYPE) %>% count()

All_provs %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) %>%
  group_by( TYPE, Physician_Profile) %>% count() %>% 
  mutate(n=ifelse(TYPE=="INTERNAL MEDICINE", n/40003,
                  ifelse(TYPE=="NEUROLOGY", n/17142,
                         ifelse(TYPE=="OBG", n/5653,
                                ifelse(TYPE=="OTHER HCP", n/46735,
                                       ifelse(TYPE=="OTHER PHYSICIAN", n/91860,
                                              ifelse(TYPE=="PRIMARY CARE", n/70993,
                                                     ifelse(TYPE=="PSYCHIATRY",n/5811,NA)))))))) %>%
  spread(key=TYPE, value=n)




All_provs <- All_provs %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) 


All_provs


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")


N_pats <- RIMUS23_Doses %>% select(provider, patid) %>% distinct() %>% group_by(provider) %>% count() %>% rename("N_pats"="n")
N_Scripts <- RIMUS23_Doses %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_Scripts"="n")
N_Prev <- RIMUS23_Doses %>% filter(drug_group=="Preventive") %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_Prev"="n")
N_Triptan <- RIMUS23_Doses %>% filter(drug_group=="Triptans") %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_Triptan"="n")
N_CGRPs <- RIMUS23_Doses %>% filter(grepl("CGRP",drug_group)) %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_CGRP"="n")


All_provs_2 <- All_provs %>% left_join(N_pats) %>% left_join(N_Scripts) %>% left_join(N_Prev) %>% left_join(N_Triptan) %>% left_join(N_CGRPs)



All_provs_2[is.na(All_provs_2)] <- 0




All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_pats))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_pats))



All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Scripts))



All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Prev/N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Prev/N_Scripts))



All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Triptan/N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  summarise(n=mean(N_Triptan/N_Scripts))


All_provs_2


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant") %>% select(provider) %>% distinct()


RIMUS23_Doses$Rimegepant <- 1


All_provs_2 <- All_provs_2 %>% left_join(RIMUS23_Doses) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))

All_provs_2 <- All_provs_2 %>% mutate(CGRP=ifelse(`CGRP Injectable`==1|`CGRP Oral`==1, 1,0))


All_provs_2 %>% group_by(Rimegepant, CGRP) %>% count()

All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_pats))

All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Scripts))



All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Scripts/N_pats, na.rm=T))



All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Prev))


All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Triptan))


All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=factor(group, levels=c("Other Rx", "Other CGRP", "Rimegepant"))) %>%
  ggplot(aes(N_Scripts, colour=group, fill=group)) +
  xlim(0, 250) +
  geom_density(alpha=0.8, adjust=8) +
  theme_minimal() +
  scale_colour_manual(values=c("#B2D1DC", "#FFEFCA", "#FFBFBF")) +
  scale_fill_manual(values=c("#B2D1DC", "#FFEFCA", "#FFBFBF")) +
  xlab("\n Number of Scripts per physician") + ylab("Physician density \n (kernel smoothing)\n")



All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant")))  %>%
  group_by(group) %>% count()

All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant"))) %>%
  ggplot(aes(N_Scripts/N_pats, colour=group, fill=group)) +
   xlim(0, 30) +
  geom_density(alpha=0.6, adjust=8) +
  theme_minimal() +
  scale_colour_manual(values=c("#B2D1DC", "#448196", "#FFEFCA", "#FFBFBF")) +
  scale_fill_manual(values=c("#B2D1DC", "#448196", "#FFEFCA", "#FFBFBF")) +
  xlab("\n Number of Scripts/Patient per physician") + ylab("Physician density \n (kernel smoothing)\n")




All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant")))  %>%
  filter(group %in% c("Other Rx", "High Vol Other Rx")) %>%
  select(provider, group, N_Triptan, N_Scripts, N_Prev) %>%
  mutate(N_Triptan=N_Triptan/N_Scripts) %>% mutate(N_Prev=N_Prev/N_Scripts) %>%
  group_by(group) %>% summarise(meanP=mean(N_Prev))






All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant")))  %>%
  filter(group %in% c("Other Rx", "High Vol Other Rx")) %>%
  select(provider, group, N_Triptan, N_Scripts, N_Prev) %>%
  mutate(N_Triptan=N_Triptan/N_Scripts) %>% mutate(N_Triptan=N_Triptan/N_Scripts) %>%
  ggplot(aes(N_Scripts, N_Triptan)) +
  geom_smooth()



All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx"))) %>%
  mutate(group=ifelse(group=="Other Rx"&N_Scripts/N_pats>5, "High Vol Other Rx", group)) %>%
  mutate(group=factor(group, levels=c("Other Rx", "High Vol Other Rx", "Other CGRP", "Rimegepant")))  %>%
  filter(group %in% c("Other Rx", "High Vol Other Rx")) %>%
  select(provider, group, TYPE) %>%
  group_by(group, TYPE) %>% count() %>%
  spread(key=TYPE , value=n)







All_provs_2 %>% group_by(Rimegepant, CGRP, TYPE) %>% count() %>%
  spread(key=TYPE, value=n)



All_provs_2 %>% group_by(Rimegepant, CGRP, TYPE) %>% summarise(n=sum(N_Scripts)) %>%
  spread(key=TYPE, value=n)



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")

CGRP_scripts <- RIMUS23_Doses %>% filter(grepl("CGRP", drug_group)) %>% select(provider, generic) %>%  group_by(provider, generic) %>% count()


temp <- All_provs_2 %>% filter(Rimegepant==0 & CGRP==1) %>%
  select(provider) %>%
  left_join(CGRP_scripts) %>% ungroup() %>% distinct() %>%
  spread(key=generic, value=n)

temp[is.na(temp)] <- 0

temp <- gather(temp, generic, n, Atogepant:Ubrogepant) 


temp %>% group_by(generic) %>% summarise(mean=mean(n))



temp %>%
  mutate(generic=factor(generic, levels=c("Atogepant", "Ubrogepant", "Eptinezumab", "Fremanezumab", "Galcanezumab", "Erenumab"))) %>%
  ggplot(aes(n, colour=generic, fill=generic)) +
  geom_density(alpha=0.5, adjust=2) +
  xlim(0,5) +
  xlab("\n Number of Scripts per Physician \n (Other CGRP Physicians/No Rimegepant)") +
  ylab("Physician density \n (Kernel smoothin) \n") +
  theme_minimal() +
  facet_wrap(~generic, ncol = 1, scales="free_y") +
  ggsci::scale_fill_jama() +
  ggsci::scale_colour_jama()

# --------------------

# More10_months ---------
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31")
RIMUS23_Doses$from_dt <- substr(as.character(RIMUS23_Doses$from_dt), 1, 7)
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider, provcat, from_dt) %>% distinct() 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(provider, provcat) %>% count() %>% ungroup() %>% filter(n>=10) %>% select(provider, provcat) %>% distinct()

More10_months <- RIMUS23_Doses


fwrite(More10_months, "More10_months.csv")

unique(All_provs_2$TYPE)

All_provs_2 %>% filter(TYPE=="PRIMARY CARE"|TYPE=="INTERNAL MEDICINE") %>% group_by(Physician_Profile) %>%
 # inner_join(RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat))) %>%
  summarise(n=mean(N_Scripts))

mean(All_provs_2$N_CGRP)

All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(Physician_Profile) %>%
  inner_join(RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat))) %>%
  summarise(n=mean(N_CGRP/N_Scripts, na.rm=T))



All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx")))  %>%
  filter(TYPE=="PRIMARY CARE"|TYPE=="INTERNAL MEDICINE") %>%
  mutate(vol=ifelse(N_Scripts>15, "High", "Low")) %>% mutate(vol=ifelse(is.na(vol), "Low", vol)) %>%
  mutate(group=paste(group, vol)) %>%
 inner_join(RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat))) %>%
   group_by(group) %>% summarise(n=mean(N_Scripts, na.rm=T))



All_provs_2 %>% mutate(group=ifelse(Rimegepant==1, "Rimegepant",
                                    ifelse(CGRP==1, "Other CGRP", "Other Rx")))  %>%
  filter(TYPE=="NEUROLOGY") %>%
  mutate(vol=ifelse(N_Scripts>15, "High", "Low")) %>% mutate(vol=ifelse(is.na(vol), "Low", vol)) %>%
  mutate(group=paste(group, vol)) %>%
 inner_join(RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat))) %>%
   group_by(group) %>% summarise(n=mean(N_Triptan/N_Scripts, na.rm=T))

# ---------------
# % Class Share month-over-month per physician specialty ----------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(All_pats)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) 

RIMUS23_Doses <- RIMUS23_Doses %>% select(weight, from_dt , provider , provcat, drug_class, drug_group) %>% distinct()

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>% 
  mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) %>%
  filter(TYPE=="NEUROLOGY"|TYPE=="INTERNAL MEDICINE")

RIMUS23_Doses$from_dt <- substr(as.character(RIMUS23_Doses$from_dt), 1, 7)


temp <- RIMUS23_Doses %>% group_by(from_dt, TYPE, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=from_dt, value=n)

fwrite(temp, "temp.csv")

# ---------------
# Remained vs droped out of Riemgepant -------------------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
Rimegepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month) %>% distinct()
names(Rimegepant_Pats)[2] <- "First_Rimegepant"

Rimegepant_Pats <- Rimegepant_Pats %>% filter(First_Rimegepant>=42&First_Rimegepant<=54)

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Rimegepant_Pats %>% select(patient))
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% mutate(Treat=as.numeric(Treat)) %>% left_join(Drugs_lookup %>% select(drug_id, drug_class ), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, Month, drug_class) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Rimegepant_Pats) %>% filter(Month<First_Rimegepant) %>% 
  select(patient, drug_class) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_class, value=exp)

Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
Rimegepant_Pats[is.na(Rimegepant_Pats)] <- 0



RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Rimegepant_Pats %>% select(patient))
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-") %>% filter(grepl("136", Treat))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Rimegepant_Pats %>% select(patient, First_Rimegepant)) %>% filter(Month>=First_Rimegepant) %>%
  select(patient, weight) %>% group_by(patient) %>% count() %>% rename("ON_Rime"="n")

Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
Rimegepant_Pats$Elapsed <- 61 - Rimegepant_Pats$First_Rimegepant

Rimegepant_Pats %>% select(patient, First_Rimegepant, ON_Rime, Elapsed) %>% filter(First_Rimegepant==54)


RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Demographics)[1] <- "patient"
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patient, GENDER, AGE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% mutate(GENDER=ifelse(GENDER=="M",1,0))
Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Demographics)

Rimegepant_Pats <- Rimegepant_Pats %>% select(patient, First_Rimegepant, Elapsed, ON_Rime, GENDER, AGE, Analgesic:`Weak Opioid`)


flMIG <- fread("MIG_Flows_Aux._Long_v3.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- flMIG %>% inner_join(Rimegepant_Pats %>% select(patient)) %>% group_by(patient) %>% summarise(flows=sum(as.numeric(flow)))

Rimegepant_Pats <- Rimegepant_Pats %>% left_join(flMIG)


RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Demographics)[1] <- "patient"

RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patient, migraine_onset) %>% mutate(migraine_onset=as.Date(migraine_onset)) %>% mutate(disease_duration=as.numeric(as.Date("2023-05-15")-migraine_onset)/30.5) 
range(RIMUS23_Demographics$disease_duration, na.rm=T)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patient, disease_duration)
RIMUS23_Demographics[is.na(RIMUS23_Demographics)] <- 0

Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Demographics)

RIMUS23_Demographics[is.na(RIMUS23_Demographics)] <- 0



library(randomForest)
library(xgboost)
library(caret)


temp <- Rimegepant_Pats %>% ungroup() %>% select(-patient)
temp <- temp %>% mutate(perc=ON_Rime/Elapsed) %>% select(-c(First_Rimegepant, Elapsed, ON_Rime))

temp <- temp %>% mutate(perc=ifelse(perc==1, 1,
                            ifelse(perc<0.25,0,NA))) %>% drop_na() %>%
  mutate(AGE=ifelse(AGE>55,1,0)) %>% mutate(flows=ifelse(flows>20,1,0)) %>% 
  select(-disease_duration) %>%
  # mutate(disease_duration=ifelse(disease_duration>30,1,0)) %>%
  mutate(perc=as.factor(perc))

names(temp) <- str_replace_all(names(temp), " ", "_")

modelAll_1_randomForest <- randomForest(perc ~ ., data = temp)
 
summary(modelAll_1_randomForest)
 
data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)

unique(temp$perc)


temp %>% group_by(perc) %>% summarise(n=mean(AGE))



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


names(temp)


model_hd = xgboost(data = as.matrix(temp[,-26]),
                   nround = 500,
                   objective = "binary:logistic",
                   label=as.matrix(temp[26]))  



shap_result = shap.score.rank(xgb_model = model_hd, 
                              X_train = as.matrix(temp[,-26]),
                              shap_approx = F)


var_importance(shap_result, top_n=25)


shap_long_hd = shap.prep(X_train = as.matrix(temp[,-26]) , top_n = 25)

plot.shap.summary(data_long = shap_long_hd)

shap_long_hd %>% select(variable) %>% distinct()

# -------------------------------
# Remained vs droped out of Riemgepant -------------------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
Rimegepant_Pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% select(patient, Month) %>% distinct()
names(Rimegepant_Pats)[2] <- "First_Rimegepant"

Rimegepant_Pats <- Rimegepant_Pats %>% filter(First_Rimegepant>=42&First_Rimegepant<=54)

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Rimegepant_Pats %>% select(patient))
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% mutate(Treat=as.numeric(Treat)) %>% left_join(Drugs_lookup %>% select(drug_id, drug_class ), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, Month, drug_class) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Rimegepant_Pats) %>% filter(Month<First_Rimegepant) %>% 
  select(patient, drug_class) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_class, value=exp)

Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
Rimegepant_Pats[is.na(Rimegepant_Pats)] <- 0



RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Rimegepant_Pats %>% select(patient))
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-") %>% filter(grepl("136", Treat))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Rimegepant_Pats %>% select(patient, First_Rimegepant)) %>% filter(Month>=First_Rimegepant) %>%
  select(patient, weight) %>% group_by(patient) %>% count() %>% rename("ON_Rime"="n")

Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Drug_Histories)
Rimegepant_Pats$Elapsed <- 61 - Rimegepant_Pats$First_Rimegepant

Rimegepant_Pats %>% select(patient, First_Rimegepant, ON_Rime, Elapsed) %>% filter(First_Rimegepant==54)


RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Demographics)[1] <- "patient"
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patient, GENDER, AGE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% mutate(GENDER=ifelse(GENDER=="M",1,0))
Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Demographics)

Rimegepant_Pats <- Rimegepant_Pats %>% select(patient, First_Rimegepant, Elapsed, ON_Rime, GENDER, AGE, Analgesic:`Weak Opioid`)


flMIG <- fread("MIG_Flows_Aux._Long_v3.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- flMIG %>% inner_join(Rimegepant_Pats %>% select(patient)) %>% group_by(patient) %>% summarise(flows=sum(as.numeric(flow)))

Rimegepant_Pats <- Rimegepant_Pats %>% left_join(flMIG)


RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Demographics)[1] <- "patient"

RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patient, migraine_onset) %>% mutate(migraine_onset=as.Date(migraine_onset)) %>% mutate(disease_duration=as.numeric(as.Date("2023-05-15")-migraine_onset)/30.5) 
range(RIMUS23_Demographics$disease_duration, na.rm=T)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patient, disease_duration)
RIMUS23_Demographics[is.na(RIMUS23_Demographics)] <- 0

Rimegepant_Pats <- Rimegepant_Pats %>% left_join(RIMUS23_Demographics)

RIMUS23_Demographics[is.na(RIMUS23_Demographics)] <- 0



library(randomForest)
library(xgboost)
library(caret)


temp <- Rimegepant_Pats %>% ungroup() %>% select(-patient)
temp <- temp %>% mutate(perc=ON_Rime/Elapsed) %>% select(-c(First_Rimegepant, Elapsed, ON_Rime))

temp <- temp %>% mutate(perc=ifelse(perc==1, 1,
                            ifelse(perc<0.25,0,NA))) %>% drop_na() %>%
  mutate(AGE=ifelse(AGE>55,1,0)) %>% mutate(flows=ifelse(flows>20,1,0)) %>% 
  select(-disease_duration) %>%
  # mutate(disease_duration=ifelse(disease_duration>30,1,0)) %>%
  mutate(perc=as.factor(perc))

names(temp) <- str_replace_all(names(temp), " ", "_")

modelAll_1_randomForest <- randomForest(perc ~ ., data = temp)
 
summary(modelAll_1_randomForest)
 
data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)

unique(temp$perc)


temp %>% group_by(perc) %>% summarise(n=mean(AGE))



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


names(temp)


model_hd = xgboost(data = as.matrix(temp[,-26]),
                   nround = 500,
                   objective = "binary:logistic",
                   label=as.matrix(temp[26]))  



shap_result = shap.score.rank(xgb_model = model_hd, 
                              X_train = as.matrix(temp[,-26]),
                              shap_approx = F)


var_importance(shap_result, top_n=25)


shap_long_hd = shap.prep(X_train = as.matrix(temp[,-26]) , top_n = 25)

plot.shap.summary(data_long = shap_long_hd)

shap_long_hd %>% select(variable) %>% distinct()

# -------------------------------
# Physician based on MIgraine-sepcifc drugs --------------------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Drugs_lookup %>%
                                                                 filter(drug_group=="CGRP Injectable"|drug_group=="CGRP Oral"|drug_group=="Triptans") %>%
                                                                 select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15")
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider , provcat, drug_group) %>% distinct()
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(exp=1) %>% spread(key=drug_group, value=exp)

# RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
# RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date=as.Date(date)) %>% filter(date>="2022-06-01"&date<="2023-05-31")
# RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(provider, provcat) %>% distinct()
# RIMUS23_Migraine_Dxs$Dx <- 1
# RIMUS23_Migraine_Dxs %>% group_by(provcat) %>% count() %>% arrange(-n)

# All_provs <- RIMUS23_Doses %>% select(provider, provcat) %>% full_join(RIMUS23_Migraine_Dxs %>% select(provider, provcat)) %>% distinct() %>%
 # left_join(RIMUS23_Migraine_Dxs %>% select(-provcat)) %>% left_join(RIMUS23_Doses %>% select(-provcat))

All_provs <- RIMUS23_Doses 

All_provs[is.na(All_provs)] <- 0

All_provs <- All_provs %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

All_provs <- All_provs %>% filter(TYPE!="EXCLUDE") %>% drop_na()

All_provs <- All_provs %>% mutate(Physician_Profile=ifelse(`CGRP Oral`==1, "Rx_CGRP_Oral",
                                                           ifelse(`CGRP Injectable`==1, "Rx_CGRP_Injectable",
                                                            ifelse(Triptans==1, "Rx_Triptan","none"))))


All_provs %>% group_by(Physician_Profile) %>% count() %>% mutate(n=n/dim(All_provs)[1])

All_provs %>% group_by(TYPE) %>% count() %>% mutate(n=n/dim(All_provs)[1])


All_provs %>% group_by(TYPE) %>% count() 


All_provs %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) %>%
  group_by( TYPE, Physician_Profile) %>% count() %>% 
  mutate(n=ifelse(TYPE=="INTERNAL MEDICINE", n/(6614+462),
                  ifelse(TYPE=="NEUROLOGY", n/4858,
                         ifelse(TYPE=="OBG", n/654,
                                ifelse(TYPE=="OTHER HCP", n/5064,
                                       ifelse(TYPE=="OTHER PHYSICIAN", n/3088,
                                              ifelse(TYPE=="PRIMARY CARE", n/13953,
                                                     ifelse(TYPE=="PSYCHIATRY",n/258,NA)))))))) %>%
  spread(key=TYPE, value=n)


All_provs <- All_provs %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) 

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15")

N_pats <- RIMUS23_Doses %>% select(provider, patid) %>% distinct() %>% group_by(provider) %>% count() %>% rename("N_pats"="n")
N_Scripts <- RIMUS23_Doses %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_Scripts"="n")
N_Triptan <- RIMUS23_Doses %>% filter(drug_group=="Triptans") %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_Triptan"="n")
N_CGRP_Oral <- RIMUS23_Doses %>% filter(grepl("CGRP Oral",drug_group)) %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_CGRP_Oral"="n")
N_CGRP_Inj <- RIMUS23_Doses %>% filter(grepl("CGRP Injectable",drug_group)) %>% select(provider) %>%  group_by(provider) %>% count() %>% rename("N_CGRP_Inj"="n")

All_provs_2 <- All_provs %>% left_join(N_pats) %>% left_join(N_Scripts)%>% left_join(N_Triptan) %>% left_join(N_CGRP_Oral)  %>% left_join(N_CGRP_Inj) 

All_provs_2[is.na(All_provs_2)] <- 0


All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_pats))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_pats))

All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_Scripts))

All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_CGRP_Oral/N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_CGRP_Oral/N_Scripts))

All_provs_2 %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>%  # group_by(Physician_Profile) %>%
  summarise(n=mean(N_Triptan/N_Scripts))
All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_Triptan/N_Scripts))



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant") %>% select(provider) %>% distinct()

RIMUS23_Doses$Rimegepant <- 1

All_provs_2 <- All_provs_2 %>% left_join(RIMUS23_Doses) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))

All_provs_2 <- All_provs_2 %>% mutate(CGRP=ifelse(`CGRP Injectable`==1|`CGRP Oral`==1, 1,0))

All_provs_2 %>% group_by(Rimegepant, CGRP) %>% count()

All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_pats))

All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Scripts))

All_provs_2 %>% group_by(Rimegepant, CGRP) %>% summarise(pats=mean(N_Scripts/N_pats, na.rm=T))


All_provs_2 %>% filter(TYPE=="NEUROLOGY") %>% group_by(`CGRP Oral`) %>% count()




RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15")
RIMUS23_Doses$from_dt <- substr(as.character(RIMUS23_Doses$from_dt), 1, 7)
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider, provcat, from_dt) %>% distinct() 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(provider, provcat) %>% count() %>% ungroup() %>% filter(n>=10) %>% select(provider, provcat) %>% distinct()




RIMUS23_Doses




All_provs_2 %>% inner_join(RIMUS23_Doses %>% select(provider)) %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_pats))
All_provs_2 %>% inner_join(RIMUS23_Doses %>% select(provider)) %>% filter(TYPE=="NEUROLOGY") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_pats))

All_provs_2 %>% inner_join(RIMUS23_Doses%>% select(provider)) %>% filter(TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_Scripts))
All_provs_2 %>% inner_join(RIMUS23_Doses%>% select(provider)) %>% filter(TYPE=="NEUROLOGY") %>% # group_by(Physician_Profile) %>%
  summarise(n=mean(N_Scripts))


# ------------



# # Acute vs prev , patients & volume per specialty V3 Month>10 per physician  -----------------
All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15")
RIMUS23_Doses$from_dt <- substr(as.character(RIMUS23_Doses$from_dt), 1, 7)
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider, provcat, from_dt) %>% distinct() 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(provider, provcat) %>% count() %>% ungroup() %>% filter(n>=10) %>% select(provider, provcat) %>% distinct()
More10_months <- RIMUS23_Doses




RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>% mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>% ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses %>% select(patid) %>% group_by(patid) %>% count() %>% filter(n>1)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[2] <- "OralAcute"
names(RIMUS23_Doses)[3] <- "OralPrev"


All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories) 
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
Experience <- RIMUS23_Drug_Histories

Experience <- Experience %>% left_join(RIMUS23_Doses, by=c("patient"="patid"))

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 

Rimegepan_pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% select(patient) %>% distinct()
Rimegepan_pats <- Rimegepan_pats %>% mutate(Rimegepant=1)

Experience <- Experience %>% left_join(Rimegepan_pats) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))

Experience$OralAcute <- as.character(Experience$OralAcute)
Experience$OralPrev <- as.character(Experience$OralPrev)

Experience[is.na(Experience)] <- "-"

Experience %>% group_by(OralAcute, OralPrev) %>% summarise(n=sum(as.numeric(weight)))
Experience  <- Experience %>% mutate(OralAcute=ifelse(OralPrev==1&OralAcute==1,0,OralAcute))
Experience <- Experience %>% mutate(OralAcute=ifelse(OralAcute=="-"&`CGRP Oral`==1, "1", OralAcute))

sum(as.numeric(Experience$weight))

data.frame(Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  group_by(Type, Rimegepant, `CGRP Oral`, `CGRP Injectable`, ) %>%
   # group_by(Type) %>%
  summarise(n=sum(as.numeric(weight))))

Experience <- Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  select(patient, weight, Type)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, generic, drug_class, drug_group,  provider, provcat)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(class=ifelse(generic=="Rimegepant", "Rimegepant",
                                      ifelse(drug_class=="CGRP Oral", "CGRP Oral",
                                             ifelse(drug_class=="CGRP Injectable", "CGRP Injectable", drug_group))))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, class, provider, provcat)
names(RIMUS23_Doses)[1] <- "patient"
unique(RIMUS23_Doses$class)

Experience <- Experience %>% left_join(RIMUS23_Doses) %>% drop_na() %>%
  #filter(Type=="Prev") %>%
  mutate(Volume=as.numeric(weight)*as.numeric(days_sup)) %>%
  select(-c(weight, days_sup))

# Experience <- Experience %>% select(-Type)

unique(Experience$class)

Unique_provcats <- fread("Unique_provcats.csv")

Experience <- Experience %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na()

unique(Experience$TYPE)

Experience <- Experience %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                ifelse(TYPE=="PRIMARY CARE"|TYPE=="INTERNAL MEDICINE"|TYPE=="INTERAL MEDICINE", "PCP", "OTHER")))





RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 


RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(generic=ifelse(generic=="Rimegepant", generic, "CGRP Oral")) %>% select(patid, generic, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[3] <- "OralAcute"
names(RIMUS23_Doses)[4] <- "OralPrev"
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(OralAcute=ifelse(OralPrev==1,0,OralAcute))
names(RIMUS23_Doses)[2] <- "class"
names(RIMUS23_Doses)[1] <- "patient"

Experience <- Experience %>% left_join(RIMUS23_Doses)
Experience[is.na(Experience)] <- 0


Experience <- Experience %>% inner_join(More10_months %>% mutate(provcat=as.numeric(provcat)))


unique(Experience$class)

Experience %>% filter(TYPE=="NEUROLOGY") %>%
  group_by(Type, class) %>%  count() 


Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
    mutate(class=ifelse(class=="Rimegepant", "Rimegepant_Acute", class)) %>%
  filter(grepl("Rimegepant", class)) %>%
  select(provider, provcat, TYPE, Type, group) %>% distinct() %>%
  group_by(TYPE, Type, group) %>% count()
    


data.frame(Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
    mutate(class=ifelse(class=="Rimegepant", "Rimegepant_Acute", class)) %>%
  group_by(Type, TYPE, group, class) %>%  summarise(total=sum(as.numeric(Volume)))
  )

# ---------------------------------
# Number of Months Currently Lased vs Therapy lines tried ----------------------

All_pats <- fread("ModSev_Pats_V3.txt")
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

flMIG <- fread("MIG_Flows_Aux._Long_v3.txt")
flMIG <- flMIG %>% inner_join(All_pats)

Naive_Pats <- flMIG %>% filter(p2==60 & s2=="X" ) %>% select(patient)

flMIG <- Naive_Pats %>% left_join(flMIG)
flMIG <- flMIG %>% arrange(patient, -p2, -p1)

flMIG <- flMIG %>% mutate(patient=as.character(patient)) %>%
  group_by(patient, weight) %>% 
  slice(if(any(grepl("-",d2))) which.max(grepl("-",d2)):which.max(!grepl("-",d2))-1 else NA) 

Naive_pats <- flMIG %>% group_by(patient, weight) %>% count() 

Naive_pats %>% ungroup() %>% group_by(n) %>% summarise(pats=sum(as.numeric(weight)))

Naive_pats %>% ungroup() %>%
  ggplot(aes(x=n)) + 
  geom_density()


flMIG <- fread("MIG_Flows_Aux._Long_v3.txt")
flMIG <- flMIG %>% inner_join(All_pats)
flMIG <- flMIG %>% mutate(patient=as.character(patient)) %>%
  inner_join(Naive_pats %>% select(patient, weight))
flMIG <- flMIG %>% select(patient, weight, d2) %>% filter(d2!="-") %>% distinct()
flMIG <- flMIG %>% group_by(patient, weight) %>% count() %>% rename("lines"="n")


data.frame(flMIG %>% ungroup() %>% mutate(lines=ifelse(lines>=24,24,lines)) %>% 
             group_by(lines) %>% summarise(pats=sum(as.numeric(weight))))


Naive_pats %>% inner_join(flMIG) %>%
  ungroup %>%
  ggplot(aes(n, lines)) + 
  geom_smooth() +
  coord_cartesian(
  xlim = c(0,11),
  ylim = c(0,9)) +
  xlab("\n Number of Months \n Currently Lapsed period") +
  ylab("Number of Therapy Lines \n Up to Now \n") +
  theme_minimal()


# ---------------

# Acute vs preventive Rimegepant patients ----------------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid,  from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight) %>% summarise(mean=mean(rate)) %>%
 # mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

temp <- RIMUS23_Doses

flMIG <- fread("MIG_Flows_Aux._Long_v3.txt", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- temp %>% select(patid) %>% left_join(flMIG, by=c("patid"="patient"))
temp <- temp %>% left_join(flMIG %>% filter(as.numeric(p2)>=49) %>% group_by(patid) %>% summarise(flows=sum(as.numeric(flow)))) 

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% inner_join(temp %>% select(patid)) 
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, GENDER, AGE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% mutate(GENDER=ifelse(GENDER=="M",1,0))
temp <- temp %>% left_join(RIMUS23_Demographics)
temp[is.na(temp)] <- 0


Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- temp %>% select(patid) %>% left_join(RIMUS23_Drug_Histories, by=c("patid"="patient"))
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class ), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patid, drug_class) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories  %>% mutate(exp=1) %>% spread(key=drug_class, value=exp)

temp <- temp %>% left_join(RIMUS23_Drug_Histories)

names(temp) <- str_replace_all(names(temp), " ", "_")

temp[is.na(temp)] <- 0

temp %>% group_by(mean) %>% summarise(n=mean(flows))
temp %>% group_by(mean) %>% summarise(n=mean(GENDER))
temp %>% group_by(mean) %>% summarise(n=mean(as.numeric(AGE)))
temp %>% group_by(mean) %>% summarise(n=mean(as.numeric(CGRP_Injectable)))
temp %>% group_by(mean) %>% summarise(n=mean(as.numeric(CGRP_Oral)))
temp %>% group_by(mean) %>% summarise(n=mean(as.numeric(Triptan)))
temp %>% group_by(mean) %>% summarise(n=mean(as.numeric(Antiemetic)))
temp %>% group_by(mean) %>% summarise(n=mean(as.numeric(Antiepileptic)))

temp %>% group_by(mean) %>% summarise(across(everything(), mean)) 


temp %>%
  filter(mean<=10  | mean >=20) %>%
  mutate(mean=ifelse(mean<=10, "Acute", "Prev")) %>%
  mutate(AGE=as.numeric(AGE)) %>%
  group_by(mean) %>% summarise(across(everything(), mean))
  

temp %>%
  filter(mean<=30) %>%
  filter(mean<=10  | mean >=20) %>%
  ggplot(aes(mean, as.numeric(Antipsychotic))) + 
  geom_point() +
  geom_smooth()
  


# ------------
# Months vs Supply days per month ---------------------------

All_pats <- fread("ModSev_Pats_V3.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% filter(Month>=49) %>% select(patient, weight, Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight) %>% count()
names(RIMUS23_Drug_Histories)[3] <- "MONTHS"
names(RIMUS23_Drug_Histories)[2] <- "weight"

temp <- RIMUS23_Drug_Histories

Drugs_lookup <- fread("Drugs_lookup.csv")








RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories








RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- temp %>% select(patient) %>% distinct() %>% ungroup() %>% left_join(RIMUS23_Doses, by=c("patient"="patid")) %>% inner_join(Unique_pats_drugs)
RIMUS23_Doses <- RIMUS23_Doses %>%  filter(status != "G") %>% mutate(from_dt = as.Date(from_dt))  %>% 
  filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

unique(RIMUS23_Doses$drug_group)

# RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_group=="Preventive"|grepl("CGRP",drug_group))


RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patient) %>% summarise(n=sum(as.numeric(days_sup)))
names(RIMUS23_Doses)[2] <- "SUPDAYS"

temp <- temp %>% left_join(RIMUS23_Doses) %>% drop_na()

temp <- temp %>% mutate(SUPPERC=(SUPDAYS/MONTHS))




temp %>%
  ggplot(aes(MONTHS, SUPPERC)) +
  geom_jitter(alpha=0.5, size=0.5) +
  theme_minimal() +
  geom_smooth() +
  ylim(0,300) +
  xlab("\n Number of Months ON Therapy") +
  ylab("Nmber of Supply Days per Month \n")



marimekko <- temp %>% mutate(SUPPERC=ifelse(SUPPERC<=25, "000-025",
                               ifelse(SUPPERC<=50,"025-050",
                                          ifelse(SUPPERC<=75, "050-075",
                                                 ifelse(SUPPERC<=100, "075-100",
                                                        ifelse(SUPPERC<=125, "100-125",
                                                               ifelse(SUPPERC<=150, "125-150",
                                                                      ifelse(SUPPERC<=175, "150-175",
                                                                             ifelse(SUPPERC<=200, "175-200", "200Plus"))))))))) %>%
  group_by(MONTHS, SUPPERC) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=SUPPERC, value=n)

marimekko[is.na(marimekko)] <- 0




RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid,  from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight) %>% summarise(mean=mean(rate)) %>%
 # mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

library(ggExtra)

temp <- temp %>% inner_join(RIMUS23_Doses, by=c("patient"="patid", "weight"="weight"))

temp



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- temp %>% select(patient) %>% distinct() %>% ungroup() %>% left_join(RIMUS23_Doses, by=c("patient"="patid")) %>% inner_join(Unique_pats_drugs)
RIMUS23_Doses <- RIMUS23_Doses %>%  filter(status != "G") %>% mutate(from_dt = as.Date(from_dt))  %>% 
  filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patient) %>% summarise(n=sum(as.numeric(days_sup)))

RIMUS23_Doses <- RIMUS23_Doses %>% rename("RIME_SUPDARS"="n")




temp %>% inner_join(RIMUS23_Doses) %>% group_by(MONTHS) %>% count() %>% mutate(n=n/(8+18+34+55+59+75+91+97+111+131+1479))


temp %>% mutate(group=ifelse(mean<13, "RIME Acute", " RIME Prev")) %>% group_by(group) %>% count()

temp %>% inner_join(RIMUS23_Doses) %>% mutate(perc=RIME_SUPDARS/SUPDAYS) %>%
  mutate(perc=ifelse(perc<=0.25, "0-25",
                     ifelse(perc<=0.5, "25-50",
                            ifelse(perc<=0.75,"50-75", "75-100")))) %>%
  group_by(perc) %>% count() %>% mutate(n=n/(1133+507+213+305))
  



p <- temp %>% inner_join(RIMUS23_Doses) %>%
  mutate(group=ifelse(mean<13, "RIME Acute", " RIME Prev")) %>%
  ggplot(aes(RIME_SUPDARS/SUPDAYS  , mean, colour=group, fill=group)) +
   geom_jitter(alpha=0.5, size=1) +
  theme_minimal() +
  ylim(2.5,22.5) +
  xlab("\n Rimegepant Share of Supply Days\n") +
  ylab("Rimegepant Rate Usage \n (Pills per month) \n") +
  ggsci::scale_fill_nejm() +
  ggsci::scale_colour_nejm() 



ggMarginal(p, type="density", fill = "deepskyblue4" , alpha=0.6, groupColour = T, groupFill = T)


# --------------------------------

# Patiens seen Neurology vs PCP -------------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15")

RIMUS23_Doses %>% select(patid, provider, provcat) %>% distinct() %>%
  group_by(patid) %>% count() %>% ungroup() %>% # summarise(mean=mean(n)) # 3
  ggplot(aes(n)) +
  geom_density() +
  xlim(0,25) +
  theme_minimal() +
  xlab("\n Number of unique physicians seen last year") + ylab("Patient density \n")


unique(Unique_provcats$TYPE)

RIMUS23_Doses %>% select(patid, provider, provcat) %>% distinct()  %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na() %>%
  select(patid, TYPE) %>% distinct() %>%
  filter(TYPE!="NEUROLOGY"&TYPE!="PRIMARY CARE"&TYPE!="INTERAL MEDICINE"&TYPE!="INTERAL MEDICINE") %>%
  select(patid) %>% distinct() # 127081 # 85830 # 0.675396




Unique_provcats <- Unique_provcats %>% filter(TYPE=="NEUROLOGY"|TYPE=="PRIMARY CARE"|TYPE=="INTERAL MEDICINE"|TYPE=="INTERAL MEDICINE")

RIMUS23_Doses %>% select(patid, provider, provcat) %>% distinct()  %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na() %>%
  select(patid, TYPE) %>% distinct() %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY", "OTHER")) %>%
  filter(TYPE=="NEUROLOGY") %>% select(patid) %>% distinct() %>% # 22499
  left_join(
    RIMUS23_Doses %>% select(patid, provider, provcat) %>% distinct()  %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na() %>%
  select(patid, TYPE) %>% distinct() %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY", "OTHER")) %>% distinct()
  ) %>% group_by(TYPE) %>% count()



RIMUS23_Doses %>% select(patid, provider, provcat) %>% distinct()  %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na() %>%
  select(patid, TYPE) %>% distinct() %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY", "OTHER")) %>%
  mutate(exp=1) %>% ungroup() %>% distinct() %>% spread(key=TYPE, value=exp) %>%
  group_by(NEUROLOGY, OTHER) %>% count() %>% mutate(n=n/(10880+11619+74439))
















All_pats <- fread("ModSev_Pats_V3.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% filter(Month>=49) %>% select(patient, weight, Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight) %>% count()
names(RIMUS23_Drug_Histories)[3] <- "MONTHS"
names(RIMUS23_Drug_Histories)[2] <- "weight"

temp <- RIMUS23_Drug_Histories

Drugs_lookup <- fread("Drugs_lookup.csv")


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- temp %>% select(patient) %>% distinct() %>% ungroup() %>% left_join(RIMUS23_Doses, by=c("patient"="patid")) %>% inner_join(Unique_pats_drugs)
RIMUS23_Doses <- RIMUS23_Doses %>%  filter(status != "G") %>% mutate(from_dt = as.Date(from_dt))  %>% 
  filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

Unique_provcats <- fread("Unique_provcats.csv")

Unique_provcats <- Unique_provcats %>% filter(TYPE=="NEUROLOGY"|TYPE=="PRIMARY CARE"|TYPE=="INTERAL MEDICINE"|TYPE=="INTERAL MEDICINE")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patient, provider, provcat) %>% distinct()  %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na() %>% select(patient, TYPE) %>% distinct()


RIMUS23_Doses <- RIMUS23_Doses  %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY", "PCP")) %>% distinct()

temp %>% left_join(RIMUS23_Doses) %>% mutate(exp=1) %>% spread(key=TYPE, value=exp) %>% ungroup() %>%
  group_by(MONTHS, PCP   ) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key= PCP, value=n) %>% mutate(perc=`1`/(`1`+`<NA>`))

# --------------------------
 
# Number of  Months ON therapy vs volume by patient type ---------------------------

All_pats <- fread("ModSev_Pats_V3.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% filter(Month>=49) %>% select(patient, weight, Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight) %>% count()
names(RIMUS23_Drug_Histories)[3] <- "MONTHS"
names(RIMUS23_Drug_Histories)[2] <- "weight"

temp <- RIMUS23_Drug_Histories












All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15")
RIMUS23_Doses$from_dt <- substr(as.character(RIMUS23_Doses$from_dt), 1, 7)
RIMUS23_Doses <- RIMUS23_Doses %>% select(provider, provcat, from_dt) %>% distinct() 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(provider, provcat) %>% count() %>% ungroup() %>% filter(n>=10) %>% select(provider, provcat) %>% distinct()
More10_months <- RIMUS23_Doses




RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>% mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>% ungroup() 

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses %>% select(patid) %>% group_by(patid) %>% count() %>% filter(n>1)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[2] <- "OralAcute"
names(RIMUS23_Doses)[3] <- "OralPrev"


All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories) 
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
Experience <- RIMUS23_Drug_Histories

Experience <- Experience %>% left_join(RIMUS23_Doses, by=c("patient"="patid"))

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 

Rimegepan_pats <- RIMUS23_Drug_Histories %>% filter(grepl("136", Treat)) %>% select(patient) %>% distinct()
Rimegepan_pats <- Rimegepan_pats %>% mutate(Rimegepant=1)

Experience <- Experience %>% left_join(Rimegepan_pats) %>% mutate(Rimegepant=ifelse(is.na(Rimegepant),0,Rimegepant))

Experience$OralAcute <- as.character(Experience$OralAcute)
Experience$OralPrev <- as.character(Experience$OralPrev)

Experience[is.na(Experience)] <- "-"

Experience %>% group_by(OralAcute, OralPrev) %>% summarise(n=sum(as.numeric(weight)))
Experience  <- Experience %>% mutate(OralAcute=ifelse(OralPrev==1&OralAcute==1,0,OralAcute))
Experience <- Experience %>% mutate(OralAcute=ifelse(OralAcute=="-"&`CGRP Oral`==1, "1", OralAcute))

sum(as.numeric(Experience$weight))

data.frame(Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  group_by(Type, Rimegepant, `CGRP Oral`, `CGRP Injectable`, ) %>%
   # group_by(Type) %>%
  summarise(n=sum(as.numeric(weight))))

Experience <- Experience %>%
  mutate(Type=ifelse(`CGRP Injectable`==0&Preventive==0&OralPrev!="1", "Acute_only", "Prev")) %>%
  select(patient, weight, Type)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, generic, drug_class, drug_group,  provider, provcat)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(class=ifelse(generic=="Rimegepant", "Rimegepant",
                                      ifelse(drug_class=="CGRP Oral", "CGRP Oral",
                                             ifelse(drug_class=="CGRP Injectable", "CGRP Injectable", drug_group))))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, days_sup, class, provider, provcat)
names(RIMUS23_Doses)[1] <- "patient"
unique(RIMUS23_Doses$class)

Experience <- Experience %>% left_join(RIMUS23_Doses) %>% drop_na() %>%
  #filter(Type=="Prev") %>%
  mutate(Volume=as.numeric(weight)*as.numeric(days_sup)) %>%
  select(-c(weight, days_sup))

# Experience <- Experience %>% select(-Type)

unique(Experience$class)

Unique_provcats <- fread("Unique_provcats.csv")

Experience <- Experience %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na()

unique(Experience$TYPE)

Experience <- Experience %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                ifelse(TYPE=="PRIMARY CARE"|TYPE=="INTERNAL MEDICINE"|TYPE=="INTERAL MEDICINE", "PCP", "OTHER")))





RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 


RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, generic, mean) %>%  mutate(mean=ifelse(is.na(mean), "Acute", mean)) 
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(generic=ifelse(generic=="Rimegepant", generic, "CGRP Oral")) %>% select(patid, generic, mean) %>% distinct() %>% mutate(exp=1) %>% spread(key=mean, value=exp)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0 
names(RIMUS23_Doses)[3] <- "OralAcute"
names(RIMUS23_Doses)[4] <- "OralPrev"
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(OralAcute=ifelse(OralPrev==1,0,OralAcute))
names(RIMUS23_Doses)[2] <- "class"
names(RIMUS23_Doses)[1] <- "patient"

Experience <- Experience %>% left_join(RIMUS23_Doses)
Experience[is.na(Experience)] <- 0


Experience <- Experience %>% inner_join(More10_months %>% mutate(provcat=as.numeric(provcat)))


unique(Experience$class)

Experience %>% filter(TYPE=="NEUROLOGY") %>%
  group_by(Type, class) %>%  count() 


Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
    mutate(class=ifelse(class=="Rimegepant", "Rimegepant_Acute", class)) %>%
  filter(grepl("Rimegepant", class)) %>%
  select(provider, provcat, TYPE, Type, group) %>% distinct() %>%
  group_by(TYPE, Type, group) %>% count()
    


  Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
    mutate(class=ifelse(class=="Rimegepant", "Rimegepant_Acute", class)) %>%
    inner_join(temp) %>%
    mutate(MONTHS=ifelse(MONTHS<=5, 5,
                         ifelse(MONTHS<=8, 8,
                                ifelse(MONTHS<=11, 11,12)))) %>%
    group_by(MONTHS, Type, group) %>% summarise(n=sum(as.numeric(Volume))) %>%
    spre
    



  data.frame(
  Experience %>% mutate(class=ifelse(class=="Rimegepant"&OralAcute==1, "Rimegepant_Acute",
                                   ifelse(class=="Rimegepant"&OralPrev==1, "Rimegepant_Prev",
                                          ifelse(class=="CGRP Oral"&OralAcute==1, "CGRP Oral Acute",
                                                 ifelse(class=="CGRP Oral"&OralPrev==1, "CGRP Oral Prev", class))))) %>%
  mutate(group=ifelse(class=="Preventive"|class=="CGRP Injectable"|class=="Rimegepant_Prev"|class=="CGRP Oral Prev", "Prev", "Acute")) %>%
  ungroup() %>%
    mutate(class=ifelse(class=="Rimegepant", "Rimegepant_Acute", class)) %>%
    inner_join(temp) %>%
  group_by(MONTHS) %>%  summarise(total=sum(as.numeric(Volume))/1983387175)
  ) 



# --------------------
# % of comorbid rimegepant patients already treated for it -----------
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, CV, psychiatric, epileptic)
RIMUS23_Doses <- RIMUS23_Doses %>% left_join(RIMUS23_Demographics)
names(RIMUS23_Doses)[1] <- "patient"


Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup, by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, drug_class) %>% distinct()

CVs <- RIMUS23_Drug_Histories %>% filter(drug_class=="Beta Blocker"|drug_class=="Cardiovascular"|drug_class=="Calcium Blocker") %>% 
  select(patient) %>% distinct() %>% mutate(CVs=1)

Epileptics <- RIMUS23_Drug_Histories %>% filter(drug_class=="Antiepileptic") %>% 
  select(patient) %>% distinct() %>% mutate(Epileptics=1)

Psychiatrics <- RIMUS23_Drug_Histories %>% filter(drug_class=="SSRI"|drug_class=="SNRI"|drug_class=="Antipsychotic"|drug_class=="Tricyclic") %>% 
  select(patient) %>% distinct() %>% mutate(Psychiatrics=1)

RIMUS23_Doses <- RIMUS23_Doses %>% left_join(CVs) %>% left_join(Epileptics)  %>% left_join(Psychiatrics)
RIMUS23_Doses[is.na(RIMUS23_Doses)] <- 0


data.frame(RIMUS23_Doses %>% group_by(mean, CV, psychiatric, epileptic, CVs, Epileptics, Psychiatrics) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=mean, value=n))


# --------------

# Rimegepant Pills per year ---------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

Means <- RIMUS23_Doses 

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Means %>% select(patid) ) %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(patid, from_dt, qty)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") 

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% summarise(qty=sum(as.numeric(qty)))

Means %>% inner_join(RIMUS23_Doses) %>% group_by(mean) %>% summarise(qty=mean(qty))


Means %>% inner_join(RIMUS23_Doses) %>% mutate(qty=as.numeric(qty)) %>%
  rename("RIME_group"="mean") %>%
  ggplot(aes(qty, colour=RIME_group, fill=RIME_group)) +
  geom_density(alpha=.5) +
  theme_minimal() +
  xlab("\n Number of Pills Lats Year") + ylab("Patient density \n") +
  scale_fill_manual(values=c("midnightblue", "firebrick")) +
  scale_colour_manual(values=c("midnightblue", "firebrick"))





RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Means %>% select(patid) ) %>% filter(generic=="Sumatriptan")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(patid, from_dt, qty)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") 

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% summarise(qty=sum(as.numeric(qty)))

RIMUS23_Doses %>% drop_na() %>% summarise(qty=mean(qty)) # 52.7
 
#   mean    qty # 52.7


RIMUS23_Doses %>% drop_na() %>% mutate(qty=as.numeric(qty)) %>%
  ggplot(aes(qty)) +
  geom_density(alpha=.5, fill="deepskyblue4", colour="deepskyblue4") +
  theme_minimal() +
  xlim(0,300) +
  xlab("\n Number of Pills Lats Year") + ylab("Patient density \n")


# ------------------
# Volume distribution per class - Neurologists with Riemgepant Last year -------


All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)
Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()
Unique_pats_drugs <- RIMUS23_Drug_Histories



Unique_provcats <- fread("Unique_provcats.csv")
Unique_provcats <- Unique_provcats %>% filter(TYPE=="NEUROLOGY") %>% select(PROVCAT) %>% distinct()

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>% inner_join(Unique_provcats %>% select(PROVCAT), by=c("provcat"="PROVCAT"))
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant") 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(provider, from_dt)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") 
Providers <-  RIMUS23_Doses %>% select(provider) %>% distinct()

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Providers)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") 

RIMUS23_Doses <- RIMUS23_Doses %>% select(provider, generic, drug_class, drug_group, weight, days_sup)

RIMUS23_Doses %>% mutate(class=ifelse(generic=="Rimegepant", "Rimegepant",
                                      ifelse(drug_class=="CGRP Oral", "CGRP Oral",
                                             ifelse(drug_class=="CGRP Injectable", "CGRP Injectable",
                                                    ifelse(drug_group=="Triptans", "Triptans",
                                                           ifelse(drug_group=="Symptomatic", "Other Acute",
                                                                  ifelse(drug_group=="Preventive", "Preventive", NA))))))) %>%
  group_by(class) %>% summarise(tot=sum(as.numeric(weight)*(as.numeric(days_sup))))


# ------------
# Share of Neurology Volume - Acute vs Preventive --------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(group=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 


Means <- RIMUS23_Doses 

Means %>% group_by(patid) %>% mutate(group = paste (generic, group, sep = " - ")) %>%
  filter(group!="Atogepant - Acute") %>%
  ggplot(aes(mean, colour=group, fill=group)) +
  geom_density(alpha=0.7) +
  theme_minimal() + 
  xlim(0,50) +
  xlab("\n Average Treatment Rate (pills per month)") + ylab("Patient density \n") +
  scale_fill_manual(values=c("#ee931b", "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  scale_colour_manual(values=c("#ee931b", "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) 





All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(All_pats)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories

Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt)) 


RIMUS23_Doses <- RIMUS23_Doses %>% left_join(Means) %>% filter( !(drug_class=="CGRP Oral"&is.na(mean) )) 




RIMUS23_Doses <- RIMUS23_Doses %>% select(weight, from_dt , provider , provcat, drug_class, drug_group, mean) %>% distinct()



RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>% 
  mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) %>%
  filter(TYPE=="NEUROLOGY"|TYPE=="INTERNAL MEDICINE")

RIMUS23_Doses$from_dt <- substr(as.character(RIMUS23_Doses$from_dt), 1, 7)


temp <- RIMUS23_Doses %>% group_by(from_dt, TYPE, drug_class, mean) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=from_dt, value=n)

temp <- temp %>% filter(TYPE=="NEUROLOGY")

fwrite(temp, "temp.csv")

# -------------


# Reasons for being ModSev vs Mild -----------------------

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
length(unique(RIMUS23_Demographics$patid)) # 263848

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
length(unique(All_pats$patient)) # 250119
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)
length(unique(All_pats$patient)) # 135660 # 0.5141597

RIMUS23_Demographics %>% select(patid,migraine_last_dx) %>% mutate(migraine_last_dx=as.Date(migraine_last_dx)) %>%
  filter(migraine_last_dx>="2021-06-16") %>% select(patid) %>% distinct() # 132484 # 0.5021224

RIMUS23_Demographics %>% filter(CV==1|psychiatric==1|epileptic==1) %>% select(patid) %>% distinct() # 226725 # 0.8593016


All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
length(unique(All_pats$patient)) # 250119
All_pats <- All_pats %>% filter(group=="Mild") %>% select(patient)
length(unique(All_pats$patient)) # 114459

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)

Always_Lapsed <- RIMUS23_Drug_Histories %>% filter(Treat == "-") %>% group_by(patient) %>% count() %>% filter(n==12) # 45871 # 0.4007636

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class, drug_group) %>% 
                                                                 mutate(drug_id=as.character(drug_id)), by=c("Treat"="drug_id"))
Sympt_only <- RIMUS23_Drug_Histories %>% filter(drug_group!="Triptans"&drug_group!="Preventive"&!grepl("CGRP", drug_group)) %>% select(patient) %>% distinct()
# 46920  0.4099284

# ----------------
# Continuously enrolled 5 years vs Open dataset 2 years --------

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)


RIMUS23_Demographics %>% mutate(migraine_onset=as.Date(migraine_onset)) %>%
  filter(migraine_onset>="2021-06-16")

RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, weight, GENDER, AGE)

RIMUS23_Demographics %>% group_by(GENDER) %>% count()


mean(as.numeric(RIMUS23_Demographics$AGE)) # 60

RIMUS23_Box_Histories <- read.table("RIMUS23 Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)
RIMUS23_Box_Histories %>% inner_join(All_pats) %>% select(patient, month60) %>% group_by(month60) %>% count() %>% mutate(n=n/135660)



RIMUS23_NOTCNT_Demographics <- read.table("RIMUS23 NOTCNT Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_NOTCNT_Demographics <- RIMUS23_NOTCNT_Demographics %>% select(patid, GENDER, AGE)

RIMUS23_NOTCNT_Demographics %>% group_by(GENDER) %>% count()


mean(as.numeric(RIMUS23_NOTCNT_Demographics$AGE)) # 53

RIMUS23_NOTCNT_Histories <- read.table("RIMUS23 NOTCNT Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
migpts_mild_modsev_classif <- fread("migpts_mild_modsev_classif.txt", colClasses = "character", stringsAsFactors = FALSE)
migpts_mild_modsev_classif <- migpts_mild_modsev_classif %>% filter(modsev_pat==1) %>% select(patient)
RIMUS23_NOTCNT_Histories %>% inner_join(migpts_mild_modsev_classif) %>% select(patient, month24) %>% group_by(month24) %>% count() %>% mutate(n=n/668649)



RIMUS23_NOTCNT_Drug_Histories <- read.table("RIMUS23 NOTCNT Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_NOTCNT_Drug_Histories <- gather(RIMUS23_NOTCNT_Drug_Histories, Month, Treat, month1:month24, factor_key=TRUE)
RIMUS23_NOTCNT_Drug_Histories$Month <- parse_number(as.character(RIMUS23_NOTCNT_Drug_Histories$Month))
RIMUS23_NOTCNT_Drug_Histories <- RIMUS23_NOTCNT_Drug_Histories %>% select(-disease)
RIMUS23_NOTCNT_Drug_Histories <- RIMUS23_NOTCNT_Drug_Histories %>% filter(Month>=13) 


RIMUS23_NOTCNT_Box_Histories <- read.table("RIMUS23 NOTCNT Box Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_NOTCNT_Box_Histories <- gather(RIMUS23_NOTCNT_Box_Histories, Month, Treat, month1:month24, factor_key=TRUE)
RIMUS23_NOTCNT_Box_Histories$Month <- parse_number(as.character(RIMUS23_NOTCNT_Box_Histories$Month))
RIMUS23_NOTCNT_Box_Histories <- RIMUS23_NOTCNT_Box_Histories %>% select(-disease)
RIMUS23_NOTCNT_Box_Histories <- RIMUS23_NOTCNT_Box_Histories %>% filter(Month>=13) 


RIMUS23_NOTCNT_Drug_Histories %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl("136", Treat)&grepl("136", lead(Treat))) %>%
  select(patient, Month, ) %>% distinct() %>%
  left_join(RIMUS23_NOTCNT_Box_Histories %>% select(patient, Month, Treat)) %>%
  group_by(Treat) %>% count()


# -----------------------
# Patiens seen Neurology vs PCP -  Rimegepant source of inflows -------------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Drugs_lookup %>% select(drug_id, generic), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, generic) %>% distinct()

Unique_pats_drugs <- RIMUS23_Drug_Histories

Unique_pats_drugs <- Unique_pats_drugs %>% filter(grepl("Rimegepant", generic)) %>% select(patient)  %>% distinct() %>% left_join(Unique_pats_drugs)


Unique_provcats <- fread("Unique_provcats.csv")

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(Unique_pats_drugs, by=c("patid"="patient", "weight"="weight", "generic"="generic"))

Unique_provcats <- Unique_provcats %>% filter(TYPE=="NEUROLOGY"|TYPE=="PRIMARY CARE"|TYPE=="INTERAL MEDICINE"|TYPE=="INTERAL MEDICINE")


RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, provider, provcat) %>% distinct()  %>% mutate(provcat=as.numeric(provcat)) %>%
  left_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT")) %>%
  filter(TYPE!="EXCLUDE") %>% drop_na() %>%
  select(patid, TYPE) %>% distinct() %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY", "OTHER")) %>%
  mutate(exp=1) %>% ungroup() %>% distinct() %>% spread(key=TYPE, value=exp)


RIMUS23_Doses <- RIMUS23_Doses %>% filter( (NEUROLOGY==1&is.na(OTHER)) | OTHER==1&is.na(NEUROLOGY))
names(RIMUS23_Doses)[1] <- "patient"

flMIG <- fread("MIG_Flows_Aux._Long_v3.txt",  header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- flMIG %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
flMIG <- flMIG %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

flows <- flMIG %>% filter(!grepl("136",d1) & grepl("136", d2)) %>% select(patient, weight, s1)

flows %>% inner_join(RIMUS23_Doses) %>% group_by(NEUROLOGY, s1) %>% summarise(n=sum(as.numeric(weight)))

Physician <- RIMUS23_Doses %>% mutate(Physician=ifelse(NEUROLOGY==1, 1,0)) %>% mutate(Physician=ifelse(is.na(Physician),0,Physician)) %>% select(-c(NEUROLOGY, OTHER))




Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Physician %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class ), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, drug_class) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories  %>% mutate(exp=1) %>% spread(key=drug_class, value=exp)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Demographics)[1] <- "patient"
RIMUS23_Demographics <- RIMUS23_Demographics %>% inner_join(Physician %>% select(patient)) 
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patient, GENDER, AGE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% mutate(GENDER=ifelse(GENDER=="M",1,0))
df <- RIMUS23_Demographics %>% left_join(RIMUS23_Drug_Histories)
df[is.na(df)] <- 0


flMIG <- fread("MIG_Flows_Aux._Long_v3.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- df %>% select(patient) %>% left_join(flMIG)
df <- df %>% left_join(flMIG %>% group_by(patient) %>% summarise(flows=sum(as.numeric(flow)))) 
df <- df %>% left_join(flMIG %>% group_by(patient) %>% summarise(starts=sum(as.numeric(starts)))) 
df <- df %>% left_join(flMIG %>% group_by(patient) %>% summarise(stops=sum(as.numeric(stops)))) 


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- df %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")   %>% distinct() %>% group_by(patient) %>% count() %>% rename("N_Months_Treat"="n")

df <- df %>% left_join(RIMUS23_Drug_Histories)

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- df %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% count()
df <- df %>% left_join(RIMUS23_Drug_Histories %>% rename("lines"="n"))



RIMUS23_Comorbidities_Extended_Dxs <- read.table("RIMUS23 Comorbidities Extended Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
length(unique(RIMUS23_Comorbidities_Extended_Dxs$ICD10_diag))
names(RIMUS23_Comorbidities_Extended_Dxs)[1] <- "patient"
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patient, ICD10_diag) %>% distinct() %>% inner_join(df %>% select(patient))
RIMUS23_Comorbidities_Extended_Dxs$ICD10_diag <-  substr(as.character(RIMUS23_Comorbidities_Extended_Dxs$ICD10_diag), 1, 3)
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% distinct()
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs  %>% mutate(exp=1) %>% spread(key=ICD10_diag, value=exp)
RIMUS23_Comorbidities_Extended_Dxs[is.na(RIMUS23_Comorbidities_Extended_Dxs)] <- 0
df <- df %>% left_join(RIMUS23_Comorbidities_Extended_Dxs)

names(df) <- str_replace_all(names(df), " ", "_")

dim(df)

df <- Physician %>% inner_join(df)

df_2 <- df  %>% select(-patient)

df_2 %>% group_by(Physician) %>% count()

df_3 <- df_2 %>% group_by(Physician) %>% sample_n(448) %>% ungroup()

df_3 <- df_3 %>% mutate(AGE=as.numeric(AGE))

library(randomForest)
library(xgboost)
library(caret)


df_3[is.na(df_3)] <- 0

modelAll_1_randomForest <- randomForest(Physician ~ ., data = df_3)

summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity) %>% filter(IncNodePurity>0.5)

df_3 %>% group_by(Physician) %>% summarise(AGE=mean(AGE))
df_3 %>% group_by(Physician) %>% summarise(N_Months_Treat=mean(N_Months_Treat))
df_3 %>% group_by(Physician) %>% summarise(CGRP_Injectable     =mean(CGRP_Injectable     ))





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


names(df_3)

model_hd = xgboost(data = as.matrix(df_3[,-1]),
                   nround = 500,
                   objective = "binary:logistic",
                   label=as.matrix(df_3[,1]))  



shap_result = shap.score.rank(xgb_model = model_hd, 
                              X_train = as.matrix(df_3[,-1]),
                              shap_approx = F)


var_importance(shap_result, top_n=50)



shap_long_hd = shap.prep(X_train = as.matrix(df_3[,-1]) , top_n = 50)

plot.shap.summary(data_long = shap_long_hd)

shap_long_hd %>% select(variable) %>% distinct()


result <- data.frame(df_3 %>% group_by(Physician) %>% summarise_all(mean))

result <- data.frame(names(result)) %>% bind_cols(result %>% transpose())
 






# ----------




# Concomitant classes with Rimegepant V3----------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <-RIMUS23_Drug_Histories %>% select(patient, weight, X60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(X60 != "-")  %>% distinct() %>% filter(grepl("136", X60))
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, X60, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("X60"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()
RIMUS23_Drug_Histories$Exp <- 1

data.frame(RIMUS23_Drug_Histories %>% group_by(drug_class) %>% summarise(tot=sum(as.numeric(weight))/203969)) %>%
  arrange(-tot)

203968.9

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_class, value=Exp)

RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

sum(as.numeric(RIMUS23_Drug_Histories$weight))


# ----------------------
# Concomitant classes with Ubrogepant V3 ----------------

Drugs_lookup <- fread("Drugs_lookup.csv")


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <-RIMUS23_Drug_Histories %>% select(patient, weight, X60)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(X60 != "-")  %>% distinct() %>% filter(grepl("137", X60))
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, X60, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("X60"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct()
RIMUS23_Drug_Histories$Exp <- 1

data.frame(RIMUS23_Drug_Histories %>% group_by(drug_class) %>% summarise(tot=sum(as.numeric(weight))/209124)) %>%
  arrange(-tot)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_class, value=Exp)

RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 209123.5

# ------------------

# Time since onset and profile of Inj CGRp Oral CGRP Triptan -------------

Rimegepant_Pats <- fread("Rimegepant_Pats.txt", sep="\t")

Fst_Mig_Dx_date_PatVector <- fread("Fst_Mig_Dx_date_PatVector.txt", sep=",")
Fst_Mig_Dx_date_PatVector$FST_MIG_DX_DT <- as.Date(Fst_Mig_Dx_date_PatVector$FST_MIG_DX_DT)
sum(is.na(Fst_Mig_Dx_date_PatVector))

Fst_Mig_Dx_date_MigPts <- fread("Fst_Mig_Dx_date_MigPts.txt", sep=",")
Fst_Mig_Dx_date_MigPts$FST_MIG_DX_DT <- as.Date(Fst_Mig_Dx_date_MigPts$FST_MIG_DX_DT)
sum(is.na(Fst_Mig_Dx_date_MigPts))

Fst_Mig_Dx_date_MigPts <- Fst_Mig_Dx_date_MigPts %>% left_join(Fst_Mig_Dx_date_PatVector %>% mutate(group="RIME"))

Fst_Mig_Dx_date_MigPts %>% group_by(group) %>% summarise(median=median(FST_MIG_DX_DT))


min(Fst_Mig_Dx_date_PatVector$FST_MIG_DX_DT) # "2015-08-06"
median(Fst_Mig_Dx_date_PatVector$FST_MIG_DX_DT) # "2017-05-16"
mean(Fst_Mig_Dx_date_PatVector$FST_MIG_DX_DT) # "2018-03-31"
max(Fst_Mig_Dx_date_PatVector$FST_MIG_DX_DT) # "2023-09-28"

Fst_Mig_Dx_date_PatVector %>%
  ggplot(aes(FST_MIG_DX_DT)) +
  geom_density(colour="midnightblue", fill="midnightblue", alpha=0.7) +
  theme_minimal() +
  xlab("\n Migraine Onset Date") + ylab("Patient density \n")

range(Fst_Mig_Dx_date_MigPts$FST_MIG_DX_DT)

Fst_Mig_Dx_date_MigPts %>%
  mutate(group=ifelse(is.na(group), "Other", group)) %>%
  ggplot(aes(FST_MIG_DX_DT, colour=group, fill=group)) +
  geom_density(alpha=0.7) +
  theme_minimal() +
  scale_fill_manual(values=c("midnightblue", "firebrick")) +
  scale_colour_manual(values=c("midnightblue", "firebrick")) +
  xlab("\n Migraine Onset Date") + ylab("Patient density \n")







All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)
Drugs_lookup <- fread("Drugs_lookup.csv")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

groups <- RIMUS23_Drug_Histories %>% filter(grepl(string_Acute, Treat)) %>% select(patient) %>% distinct() %>% mutate(group="Triptan") %>%
  bind_rows(RIMUS23_Drug_Histories %>% filter(grepl(string_Orals, Treat)) %>% select(patient) %>% distinct() %>% mutate(group="Orals")) %>%
  bind_rows(RIMUS23_Drug_Histories %>% filter(grepl(string_Mabs, Treat)) %>% select(patient) %>% distinct() %>% mutate(group="MAbs")) 

groups <- groups %>% left_join(RIMUS23_Drug_Histories %>% group_by(patient) %>% count() %>% rename("N_treat_month"="n"))

groups <- groups %>% left_join(RIMUS23_Drug_Histories %>% select(patient, Treat) %>% distinct() %>% group_by(patient) %>% count() %>% rename("N_Lines"="n"))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-weight) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_class), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Treat) %>% distinct() %>% mutate(exp=1) %>% spread(key=drug_class, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0

groups <- groups %>% left_join(RIMUS23_Drug_Histories)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, AGE, GENDER, CV,  psychiatric, epileptic)
names(RIMUS23_Demographics)[1] <- "patient"
RIMUS23_Demographics <- RIMUS23_Demographics %>% mutate(GENDER=ifelse(GENDER=="M", 1, 0))
groups <- groups %>% left_join(RIMUS23_Demographics)

flMIG <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character", stringsAsFactors = FALSE)
flMIG <- flMIG %>% group_by(patient) %>% summarise(flows=sum(as.numeric(flow)))

groups <- groups %>% left_join(flMIG)

groups[is.na(groups)] <- 0

RIMUS23_Comorbidities_Extended_Dxs <- read.table("RIMUS23 Comorbidities Extended Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Comorbidities_Extended_Dxs)[1] <- "patient"
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patient, date, ICD10_diag) %>% distinct() %>% inner_join(groups %>% select(patient) %>% distinct())
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% filter(grepl("G43", ICD10_diag)) %>% select(patient, date) %>% distinct()
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% group_by(patient) %>% count()
groups <- groups %>% left_join(RIMUS23_Comorbidities_Extended_Dxs %>% rename("N_Dxs"="n"))

groups[is.na(groups)] <- 0
groups$AGE <- as.numeric(groups$AGE)
groups$CV <- as.numeric(groups$CV)
groups$psychiatric <- as.numeric(groups$psychiatric)
groups$epileptic <- as.numeric(groups$epileptic)

data.frame(names(groups))

groups %>% select(-patient) %>% group_by(group) %>% summarise(across(everything(), mean)) %>% transpose()


# ---------
# Pathways ------------

Rimegepant_Pats <- fread("Rimegepant_Pats.txt", sep="\t" , header = T, colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Rimegepant_Pats %>% select(-weight) %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% anti_join(RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct())

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, drug_group) %>% count()

length(unique(RIMUS23_Drug_Histories$patient))
length(unique(RIMUS23_Drug_Histories$patient[RIMUS23_Drug_Histories$drug_group=="CGRP Oral"]))

RIMUS23_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(mean=mean(n))


RIMUS23_Drug_Histories %>% ungroup() %>%
  mutate(drug_group=factor(drug_group, levels=c("Preventive", "Symptomatic", "Triptans", "CGRP Injectable", "CGRP Oral" ))) %>%
  ggplot(aes(n,colour=drug_group, fill=drug_group)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  xlab("\n Number of Months Until Rimegepant Initiation") + ylab("Patient density \n") +
  scale_fill_manual(values=c( "#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  scale_colour_manual(values=c("#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  facet_wrap(~drug_group)



data.frame(
  RIMUS23_Drug_Histories %>% ungroup() %>% select(-n) %>% distinct() %>%
    arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste0(drug_group, collapse="+")) %>% distinct() %>%
  ungroup() %>% group_by(drug_group) %>% summarise(n=sum(as.numeric(weight))/430444) %>% arrange(-n)
  )
  





Rimegepant_Pats <- fread("Rimegepant_Pats.txt", sep="\t" , header = T, colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Rimegepant_Pats %>% select(-weight) %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()

RIMUS23_Drug_Histories %>% 
  left_join(
    RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct() %>% rename("Max"="Month")
    ) %>%
  mutate(Month=Month-Max) %>% filter(Month!=0) %>%
  ungroup() %>% group_by(Month, drug_group) %>% summarise(pop=sum(as.numeric(weight))) %>%
  ggplot(aes(Month, 100*pop/430444, colour=drug_group, fill=drug_group)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Number of Months Until Rimegepant Initiation") + ylab("Population (%) \n") +
  scale_fill_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) +
  scale_colour_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) 



  
data.frame(drug_group=c("CGRP_Injectable", "CGRP_oral", "Preventive", "Symptomatic", "Tritpans"), 
           months=c(15.7, 6.4, 25.0, 20.7, 17.3)) %>%
  mutate(drug_group=factor(drug_group, levels=c("Preventive", "Symptomatic", "Tritpans", "CGRP_Injectable", "CGRP_oral" ))) %>%
  ggplot(aes(drug_group, months, colour=drug_group, fill=drug_group)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values=c( "#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  scale_colour_manual(values=c("#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  coord_flip()


# -----------------

# % PCPs with CGRPs that did a 1st CGRP script ---------------------

Unique_provcats <- fread("Unique_provcats.csv")
unique(Unique_provcats$TYPE)
Unique_provcats <- Unique_provcats %>% filter(TYPE=="INTERAL MEDICINE"|TYPE=="INTERNAL MEDICINE"|TYPE=="PRIMARY CARE"|TYPE=="NEUROLOGY") %>% select(PROVCAT, TYPE)


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, provcat, provider, drug_class, from_dt)



PCP_CGRP <- RIMUS23_Doses %>%
  #filter(drug_class=="CGRP Injectable"|drug_class=="CGRP Oral") %>%
  filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>%
  mutate(provcat=as.numeric(provcat)) %>% inner_join(Unique_provcats %>% filter(TYPE!="NEUROLOGY") %>% select(PROVCAT), by=c("provcat"="PROVCAT")) %>%
  select(provider) %>% distinct()

length(unique(PCP_CGRP$provider)) # 4043



Firsts <- RIMUS23_Doses %>%  filter(drug_class=="CGRP Injectable"|drug_class=="CGRP Oral") %>% mutate(from_dt==as.Date(from_dt)) %>%
  mutate(provcat=as.numeric(provcat)) %>% 
    filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>%
    group_by(patid) %>% filter(from_dt==min(from_dt)) %>% ungroup() %>%
    inner_join(Unique_provcats %>% filter(TYPE!="NEUROLOGY") %>%  select(PROVCAT), by=c("provcat"="PROVCAT")) %>%
    select(provider) %>% distinct()

PCP_CGRP %>% inner_join(Firsts) %>% select(provider) %>% distinct() # 1702

# --------------
# Target Extract volume PCP and Neuros --------------
Target_Extract_Summary_paulo <- fread("Target_Extract_Summary_paulo.csv", sep=",")

library(RColorBrewer)
library(hexbin)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)


Target_Extract_Summary_paulo %>%
  filter(specialty_group=="NEURO") %>%
  ggplot(aes(`acute+mab_trx`, cgrp_mab_trx)) +
  stat_bin2d(bins=100) +
  xlim(0,1000) + ylim(0,1000) +
  theme_minimal() +
  scale_fill_gradientn(colours=r, trans="log") +
  ylab("Volume CGRPs \n") + xlab("\n Total Volume (Acute+CGRPs)")


Target_Extract_Summary_paulo %>%
  filter(specialty_group=="PCP") %>%
  ggplot(aes(`acute+mab_trx`, cgrp_mab_trx)) +
   stat_bin2d(bins=100) +
 xlim(0,400) + ylim(0,400) +
  theme_minimal() +
   scale_fill_gradientn(colours=r, trans="log") +
  ylab("Volume CGRPs \n") + xlab("\n Total Volume (Acute+CGRPs)")

# ---------------------
# Pathways Acute vs Prev ------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(group=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 


Means <- RIMUS23_Doses 
Acutes <- Means %>% filter(group=="Acute") %>% select(patid) %>% rename("patient"="patid")
Prevs <- Means %>% filter(group=="Prev")  %>% select(patid) %>% rename("patient"="patid")


Rimegepant_Pats <- fread("Rimegepant_Pats.txt", sep="\t" , header = T, colClasses = "character", stringsAsFactors = FALSE)
Rimegepant_Pats <- Rimegepant_Pats %>% inner_join(Prevs)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Rimegepant_Pats %>% select(-weight) %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% anti_join(RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct())

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, drug_group) %>% count()

length(unique(RIMUS23_Drug_Histories$patient))
length(unique(RIMUS23_Drug_Histories$patient[RIMUS23_Drug_Histories$drug_group=="CGRP Oral"]))

RIMUS23_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(mean=mean(n))



RIMUS23_Drug_Histories %>% ungroup() %>%
  mutate(drug_group=factor(drug_group, levels=c("Preventive", "Symptomatic", "Triptans", "CGRP Injectable", "CGRP Oral" ))) %>%
  ggplot(aes(n,colour=drug_group, fill=drug_group)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  xlab("\n Number of Months Until Rimegepant Initiation") + ylab("Patient density \n") +
  scale_fill_manual(values=c( "#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  scale_colour_manual(values=c("#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  facet_wrap(~drug_group)


RIMUS23_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))


data.frame(
  RIMUS23_Drug_Histories %>% ungroup() %>% select(-n) %>% distinct() %>%
    arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste0(drug_group, collapse="+")) %>% distinct() %>%
  ungroup() %>% group_by(drug_group) %>% summarise(n=sum(as.numeric(weight))/63250) %>% arrange(-n)
  )
  





Rimegepant_Pats <- fread("Rimegepant_Pats.txt", sep="\t" , header = T, colClasses = "character", stringsAsFactors = FALSE)
Rimegepant_Pats <- Rimegepant_Pats %>% inner_join(Prevs)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Rimegepant_Pats %>% select(-weight) %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl("136",Treat))) 1:which.max(grepl("136",Treat)) else row_number())   

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()

RIMUS23_Drug_Histories %>% 
  left_join(
    RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct() %>% rename("Max"="Month")
    ) %>%
  mutate(Month=Month-Max) %>% filter(Month!=0) %>%
  ungroup() %>% group_by(Month, drug_group) %>% summarise(pop=sum(as.numeric(weight))) %>%
  ggplot(aes(Month, 100*pop/63250, colour=drug_group, fill=drug_group)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Number of Months Until Rimegepant Initiation") + ylab("Population (%) \n") +
  scale_fill_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) +
  scale_colour_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) 



  
data.frame(drug_group=c("CGRP_Injectable", "CGRP_oral", "Preventive", "Symptomatic", "Tritpans"), 
           months=c(15.7, 6.4, 25.0, 20.7, 17.3)) %>%
  mutate(drug_group=factor(drug_group, levels=c("Preventive", "Symptomatic", "Tritpans", "CGRP_Injectable", "CGRP_oral" ))) %>%
  ggplot(aes(drug_group, months, colour=drug_group, fill=drug_group)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values=c( "#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  scale_colour_manual(values=c("#ee931b",  "#1bb6ee", "#095d7b", "#f8a5de", "#6d084d")) +
  coord_flip()

# -----------------------------------------


# Pathways CGRP Inj ------------


Drugs_lookup <- fread("Drugs_lookup.csv")
string_CGRPInjectable <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, Treat) %>% distinct() %>% filter(grepl(string_CGRPInjectable, Treat)) %>%
  select(patient) %>% distinct() %>%
  left_join(RIMUS23_Drug_Histories)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl(string_CGRPInjectable,Treat))) 1:which.max(grepl(string_CGRPInjectable,Treat)) else row_number())   

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% anti_join(RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct())

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient, weight, drug_group) %>% count()

length(unique(RIMUS23_Drug_Histories$patient))
length(unique(RIMUS23_Drug_Histories$patient[RIMUS23_Drug_Histories$drug_group=="CGRP Injectable"]))

RIMUS23_Drug_Histories %>% ungroup() %>% group_by(drug_group) %>% summarise(mean=mean(n))




RIMUS23_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))


data.frame(
  RIMUS23_Drug_Histories %>% ungroup() %>% select(-n) %>% distinct() %>%
    arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste0(drug_group, collapse="+")) %>% distinct() %>%
  ungroup() %>% group_by(drug_group) %>% summarise(n=sum(as.numeric(weight))/1068660) %>% arrange(-n)
  )
  



Drugs_lookup <- fread("Drugs_lookup.csv")
string_CGRPInjectable <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")


RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, Treat) %>% distinct() %>% filter(grepl(string_CGRPInjectable, Treat)) %>%
  select(patient) %>% distinct() %>%
  left_join(RIMUS23_Drug_Histories)


RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl(string_CGRPInjectable,Treat))) 1:which.max(grepl(string_CGRPInjectable,Treat)) else row_number())   

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()

RIMUS23_Drug_Histories %>% 
  left_join(
    RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct() %>% rename("Max"="Month")
    ) %>%
  mutate(Month=Month-Max) %>% filter(Month!=0) %>%
  ungroup() %>% group_by(Month, drug_group) %>% summarise(pop=sum(as.numeric(weight))) %>%
  ggplot(aes(Month, 100*pop/1068660, colour=drug_group, fill=drug_group)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Number of Months Until Injectable CGRP Initiation") + ylab("Population (%) \n") +
  scale_fill_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) +
  scale_colour_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) 




# --------------
# lapsed to Rime --------

Rimegepant_Pats <- fread("Rimegepant_Pats.txt", sep="\t" , header = T, colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- Rimegepant_Pats %>% select(-weight) %>% inner_join(RIMUS23_Drug_Histories)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

Min <- RIMUS23_Drug_Histories %>% group_by(patient) %>% filter(grepl("136", Treat) & grepl("-", lag(Treat))) %>%
  group_by(patient) %>% filter(Month==min(Month)) %>% ungroup() %>% select(patient, Month) %>% rename("Min"="Month")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(Min) %>% filter(Month<Min) %>% arrange(patient, Month) %>% select(-Min)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup  %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()

RIMUS23_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))

RIMUS23_Drug_Histories %>% 
  left_join(
    RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct() %>% rename("Max"="Month")
    ) %>%
  mutate(Month=Month-Max) %>%   filter(Month!=0) %>%
  drop_na() %>%
  ungroup() %>% group_by(Month, drug_group) %>% summarise(pop=sum(as.numeric(weight))) %>%
  ggplot(aes(Month, 100*pop/102986, colour=drug_group, fill=drug_group)) +
  geom_smooth() +
  theme_minimal() +
   coord_cartesian(ylim=c(0, 50)) +
  xlab("\n Number of Months Until Rimegepant Initiation") + ylab("Population (%) \n") +
  scale_fill_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) +
  scale_colour_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) 


data.frame(
  RIMUS23_Drug_Histories %>% ungroup() %>% select(-Month) %>% distinct() %>%
    arrange(patient, weight, drug_group) %>%
  group_by(patient, weight) %>% mutate(drug_group=paste0(drug_group, collapse="+")) %>% distinct() %>%
  ungroup() %>% group_by(drug_group) %>% summarise(n=sum(as.numeric(weight))/102986) %>% arrange(-n)
  )
  
# -------

# patients ON Preventive Month 60 -------------------
Drugs_lookup <- fread("Drugs_lookup.csv")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month==60) %>%
  select(patient, Treat) %>% distinct() %>% filter(grepl(string_Preventive, Treat)) %>% 
  select(patient) %>% distinct() %>% left_join(RIMUS23_Drug_Histories)

RIMUS23_Drug_Histories %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))
# 4717304

RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup %>% mutate(drug_id=as.character(drug_id)) %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, drug_group) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% anti_join(RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct())


RIMUS23_Drug_Histories %>%
  left_join(
    RIMUS23_Drug_Histories %>% group_by(patient)%>% filter(Month==max(Month)) %>% select(patient, Month) %>% distinct() %>% rename("Max"="Month")
    ) %>%
  mutate(Month=Month-Max) %>% # filter(Month!=0) %>%
  drop_na() %>%
  ungroup() %>% group_by(Month, drug_group) %>% summarise(pop=sum(as.numeric(weight))) %>%
  ggplot(aes(Month, 100*pop/4717304, colour=drug_group, fill=drug_group)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Number of Months Until May 2023 \n (Patients ON Preventive in May 2023) \n") + ylab("Population (%) \n") +
  scale_fill_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b")) +
  scale_colour_manual(values=c("#f8a5de", "#6d084d", "#ee931b",  "#1bb6ee", "#095d7b"))

# ----------
# Split ALl Migraine flows as acute vs preventive ------

All_pats <- fread("ModSev_Pats_V3.txt")
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

flMIG <- fread("MIG_Flows_Aux._Long_v3.txt")
flMIG <- flMIG %>% inner_join(All_pats)

Drugs_lookup <- fread("Drugs_lookup.csv")
unique(Drugs_lookup$drug_group)


string_Prevs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "CGRP Injectable" | 
                                                            Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")



flMIG <- flMIG %>% filter(p2>=49) %>% filter(flow==1) 
flMIG <- flMIG %>% select(patient,weight,p1,p2,d1,d2,s1,s2,flow)

flMIG %>% group_by(s1, s2) %>% summarise(n=sum(weight)) %>% spread(key=s2, value=n)




# Triptan Therapy class - flags

flMIG <- flMIG[, Prevs_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Prevs), str_c(unlist(str_extract_all(x, string_Prevs)), collapse = ","),"")))]
flMIG <- flMIG[, Prevs_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Prevs), str_c(unlist(str_extract_all(x, string_Prevs)), collapse = ","),"")))]
flMIG <- flMIG[, nr_Prevs_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Prevs)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_Prevs_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Prevs)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_PrevsUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Prevs_d1","Prevs_d2")] 
flMIG <- flMIG[, Prevs_flow_type := ifelse(nr_Prevs_d2 < nr_Prevs_d1 & nr_PrevsUnq_d1d2 > nr_Prevs_d1, "D+S", 
                                           ifelse(nr_Prevs_d2 > nr_Prevs_d1 & nr_PrevsUnq_d1d2 > nr_Prevs_d2, "A+S",
                                                  ifelse(nr_Prevs_d2 < nr_Prevs_d1, "D", 
                                                         ifelse(nr_Prevs_d2 > nr_Prevs_d1, "A", 
                                                                ifelse(nr_Prevs_d2 == nr_Prevs_d1 & Prevs_d2 != Prevs_d1, "S","-")))))] 



flMIG %>% filter(Prevs_flow_type!="-") %>% group_by(s1, s2) %>% summarise(n=sum(weight)) %>% spread(key=s2, value=n)



# ---------
# % Acute vs Prev Volume month over month ---------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt >= "2022-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, generic, from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, generic) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(patid, weight, generic) %>% summarise(mean=mean(rate)) %>%
  mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

means <- RIMUS23_Doses

means <- means %>% select(patid, weight, mean)




RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c(provider, provcat, status, code, NPI))
RIMUS23_Doses$from_dt <- substr(as.character(RIMUS23_Doses$from_dt), 1, 7)

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid, from_dt) %>% summarise(n=sum(as.numeric(days_sup)))

data.frame(means %>% inner_join(RIMUS23_Doses) %>% group_by(mean , from_dt) %>% 
  summarise(n2=sum(as.numeric(weight)*n)) %>%
  spread(key=mean, value=n2))%>% mutate(percAcute=Acute/(Acute+Prev)) %>% mutate(percPrev=Prev/(Acute+Prev))
 
# -----------
# Switch matrix new class based on CGRP exp, break down Acute vs Prev -----------
All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)

Drugs_lookup <- fread("Drugs_lookup.csv")

string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% filter(group=="ModSev") %>% select(patient) %>%  left_join(RIMUS23_Drug_Histories)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Preventive=ifelse(grepl(string_Preventive, Treat),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(AcuteSympt=ifelse(grepl(string_Acute, Treat)|grepl(string_Symptomatic, Treat),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Rimeg=ifelse(grepl("136", Treat),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(OralCGRP=ifelse(grepl(string_Orals, Treat),1,0))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(InjCGRP=ifelse(grepl(string_Mabs, Treat),1,0))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  group_by(patient) %>% mutate(CumOralCGRP=cumsum(OralCGRP ==1)) %>%
  mutate(CumOralCGRP=ifelse(CumOralCGRP==0,0,1)) %>% ungroup()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  group_by(patient) %>% mutate(CumInjCGRP=cumsum(InjCGRP ==1)) %>%
  mutate(CumInjCGRP=ifelse(CumInjCGRP==0,0,1))  %>% ungroup()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Stock=ifelse(Rimeg==1, "Rime",
                                               ifelse(OralCGRP==1, "Orals",
                                                      ifelse(InjCGRP==1, "Injs",
                                                             ifelse(CumOralCGRP==1, "OralExp",
                                                                    ifelse(CumInjCGRP==1,"InjExp",
                                                                           ifelse(Preventive==1, "Prevs",
                                                                                  ifelse(AcuteSympt==1, "Acutes", "Lapsed"))))))))

RIMUS23_Drug_Histories %>% filter(Month==60) %>% group_by(Stock) %>% summarise(n=sum(as.numeric(weight))) 


RIMUS23_Drug_Histories %>% filter(Month==48) %>% group_by(Stock) %>% summarise(n=sum(as.numeric(weight))) 

Stocks <- RIMUS23_Drug_Histories %>% select(patient, weight, Treat, Month, Stock)

Stocks <- Stocks %>% left_join(Stocks %>% mutate(Month=Month-1) %>% rename("Stock2"="Stock") %>% rename("Treat2"="Treat"))
Stocks <- Stocks %>% select(patient, weight, Month, Treat, Treat2, Stock, Stock2)
Stocks <- Stocks %>% filter(Month!=60) %>% mutate(Month=Month+1)

unique(Stocks$Month)

unique(Stocks$Stock2)

Stocks %>% filter(Month>=48) %>% filter(Treat!=Treat2)  %>% group_by(Stock, Stock2) %>% 
  summarise(n=sum(as.numeric(weight))) %>% spread(key=Stock2, value=n)


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid,  from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(generic, patid, weight) %>% summarise(mean=mean(rate)) %>%
 mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses %>% group_by(generic, mean) %>% summarise(n=sum(as.numeric(weight))) 
names(RIMUS23_Doses)[2] <- "patient"
RIMUS23_Doses <- RIMUS23_Doses %>% select(-weight) 

RIMUS23_Doses %>% group_by(generic, mean) %>% count()


RIMUS23_Doses %>% mutate(generic=ifelse(generic=="Rimegepant", "Rime", "Orals")) %>% distinct() %>%
  group_by(patient) %>% mutate(mean2=paste(generic, mean, sep=",")) %>% ungroup() %>% select(mean2) %>% distinct()


Stocks %>%  filter(Month>=48) %>% filter(Treat!=Treat2)  %>%
  left_join(RIMUS23_Doses %>% mutate(generic=ifelse(generic=="Rimegepant", "Rime", "Orals")) %>% distinct()) 

RIMUS23_Doses %>% filter(generic!="Atogepant") %>% mutate(generic=ifelse(generic=="Rimegepant", "Rime", "Orals")) %>% distinct()


Stocks %>%  filter(Month>=48) %>% filter(Treat!=Treat2) %>% 
  filter(Stock2 %in% c("Rime", "Orals", "Injs")) %>% select(patient, weight, Stock) %>% distinct() %>%
    filter(Stock %in% c("Acutes", "Lapsed", "Prevs")) %>%
  inner_join(
    RIMUS23_Doses %>% filter(generic!="Atogepant") %>% mutate(generic=ifelse(generic=="Rimegepant", "Rime", "Orals"))
  ) %>%
  group_by(Stock, generic, mean) %>% count() %>%
  spread(key=mean, value=n) %>% mutate(AcutePerc=Acute/(Acute+Prev)) %>% mutate(PrevPerc=Prev/(Acute+Prev)) 


# ------------------------
# Class penetrance month over month for antiepileptics --------------------
Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
sum(as.numeric(RIMUS23_Drug_Histories$weight)) # 21179255

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% 
  inner_join(Drugs_lookup %>% filter(drug_class=="Antiepileptic") %>% select(drug_id, generic), by=c("Treat"="drug_id"))

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, Month, generic) %>% distinct()

df <- data.frame(RIMUS23_Drug_Histories %>% group_by(Month, generic) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=generic, value=n))


fwrite(df, "df.csv")

# --------------
# Rimegepant Scripts over time per specialty -----------------------------------

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt, provider, provcat)

setDT(RIMUS23_Doses)[, Month_Yr := format(as.Date(from_dt), "%Y-%m") ]
RIMUS23_Doses <- RIMUS23_Doses %>% select(-from_dt)

Unique_provcats <- fread("Unique_provcats.csv")


RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Doses <- RIMUS23_Doses %>% filter(TYPE!="EXCLUDE") %>% drop_na()

unique(RIMUS23_Doses$TYPE)

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(TYPE=ifelse(TYPE=="INTERAL MEDICINE", "INTERNAL MEDICINE", TYPE)) %>%
  mutate(TYPE=ifelse(TYPE=="OBG", "OTHER PHYSICIAN", TYPE)) %>% mutate(TYPE=ifelse(TYPE=="PSYCHIATRY", "OTHER PHYSICIAN", TYPE))

RIMUS23_Doses <- RIMUS23_Doses %>% group_by(Month_Yr, TYPE) %>% summarise(n=sum(as.numeric(weight)))  %>% spread(key=TYPE, value=n)

data.frame(RIMUS23_Doses)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(generic=="Rimegepant")

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, weight, from_dt)

setDT(RIMUS23_Doses)[, Month_Yr := format(as.Date(from_dt), "%Y-%m") ]
RIMUS23_Doses <- RIMUS23_Doses %>% select(-from_dt)

data.frame(RIMUS23_Doses %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))))

# --------------
# % Preventive volume removed due to comorbidity ------------------

Drugs_lookup <- fread("Drugs_lookup.csv")

RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct()

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(Drugs_lookup, by=c("Treat"="drug_id"))

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)

RIMUS23_Drug_Histories <- ModSev_vector %>% select(patient) %>% left_join(RIMUS23_Drug_Histories)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% drop_na()

RIMUS23_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))


RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(drug_group=="Preventive")

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, CV, psychiatric, epileptic)
names(RIMUS23_Demographics)[1] <- "patient"

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% left_join(RIMUS23_Demographics)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(to_flag=ifelse(CV==1&drug_class=="Beta Blocker", 1,
                                                                 ifelse(CV==1&drug_class=="Cardiovascular",1,
                                                                        ifelse(CV==1&drug_class=="Calcium Blocker",1,
                                                                               ifelse(epileptic==1&drug_class=="Antiepileptic",1,
                                                                                      ifelse(psychiatric==1&drug_class=="SSRI",1,
                                                                                             ifelse(psychiatric==1&drug_class=="SNRI",1,
                                                                                                    ifelse(psychiatric==1&drug_class=="Antipsychotic",1,
                                                                                                           ifelse(psychiatric==1&drug_class=="Tricyclic",1,0)))))))))


RIMUS23_Drug_Histories %>% select(patient, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))
RIMUS23_Drug_Histories %>%  filter(to_flag!=1) %>%  select(patient, weight, drug_class) %>% distinct() %>% group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))
data.frame(RIMUS23_Drug_Histories %>% filter(to_flag==1) %>%  group_by(drug_class, psychiatric, epileptic, CV) %>% summarise(n=sum(as.numeric(weight))))


# ------------

# Number of flows before vs after first triptan Mab Riemgepant -------------

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-")

Drugs_lookup <- fread("Drugs_lookup.csv")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")

First_Triptan <- RIMUS23_Drug_Histories %>% filter(grepl(string_Acute, Treat)) %>% group_by(patient) %>% filter(Month==min(Month))
First_Triptan <- First_Triptan %>% select(patient, Month) %>% ungroup() %>% filter(Month>12 & Month<49)

First_Mab <- RIMUS23_Drug_Histories %>% filter(grepl(string_Mabs, Treat)) %>% group_by(patient) %>% filter(Month==min(Month))
First_Mab <- First_Mab %>% select(patient, Month) %>% ungroup() %>% filter(Month>12 & Month<49)

First_Orals <- RIMUS23_Drug_Histories %>% filter(grepl(string_Orals, Treat)) %>% group_by(patient) %>% filter(Month==min(Month))
First_Orals <- First_Orals %>% select(patient, Month) %>% ungroup() %>% filter(Month>12 & Month<49)


flMIG <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character", stringsAsFactors = FALSE)

flMIG <- flMIG %>% select(patient, weight, p2, flow)

flMIG %>% mutate(p2=as.numeric(p2)) %>% inner_join(First_Triptan) %>% arrange(patient) %>%  
  filter(p2>=(Month-12) & p2<(Month+12) ) %>% 
  mutate(period=ifelse(p2<Month, "Before", "After")) %>%
  filter(flow==1) %>%  group_by(period, patient) %>% summarise(n=sum( as.numeric(flow) )) %>%
  spread(key=period, value=n) %>% mutate(Before=ifelse(is.na(Before),1,Before)) %>% mutate(After=ifelse(is.na(After),1,After)) %>%
  mutate(fold= (After-Before)/Before) %>% ungroup() %>%
  summarise(after=mean(After), before=mean(Before))


flMIG %>% mutate(p2=as.numeric(p2)) %>% inner_join(First_Mab) %>% arrange(patient) %>%  
  filter(p2>=(Month-12) & p2<(Month+12) ) %>% 
  mutate(period=ifelse(p2<Month, "Before", "After")) %>%
  filter(flow==1) %>%  group_by(period, patient) %>% summarise(n=sum( as.numeric(flow) )) %>%
  spread(key=period, value=n) %>% mutate(Before=ifelse(is.na(Before),1,Before)) %>% mutate(After=ifelse(is.na(After),1,After)) %>%
  mutate(fold= (After-Before)/Before) %>% ungroup() %>% 
    summarise(after=mean(After), before=mean(Before))


flMIG %>% mutate(p2=as.numeric(p2)) %>% inner_join(First_Orals) %>% arrange(patient) %>%  
  filter(p2>=(Month-12) & p2<(Month+12) ) %>% 
  mutate(period=ifelse(p2<Month, "Before", "After")) %>%
  filter(flow==1) %>%  group_by(period, patient) %>% summarise(n=sum( as.numeric(flow) )) %>%
  spread(key=period, value=n) %>% mutate(Before=ifelse(is.na(Before),1,Before)) %>% mutate(After=ifelse(is.na(After),1,After)) %>%
  mutate(fold= (After-Before)/Before) %>% ungroup() %>% 
    summarise(after=mean(After), before=mean(Before))


# ------------------
# What's the first Injectable CGRP molecule ? -------------

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat!="-")

Drugs_lookup <- fread("Drugs_lookup.csv")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")

First_Mab <- RIMUS23_Drug_Histories %>% filter(grepl(string_Mabs, Treat)) %>% group_by(patient) %>% filter(Month==min(Month))

First_Mab <- separate_rows(First_Mab, Treat, sep = ",", convert=T )
First_Mab <- First_Mab %>% select(patient, weight, Treat) %>% filter(grepl(string_Mabs, Treat))

First_Mab %>% ungroup() %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) 

First_Mab %>% left_join(Drugs_lookup %>% select(drug_id, generic) %>% mutate(drug_id=as.numeric(drug_id)), by=c("Treat"="drug_id")) %>%
  ungroup() %>% group_by(generic) %>% summarise(n= 100 * sum(as.numeric(weight) /  1087653))


# ------------------------
# Stock month60 if not considering muscle relaxants ---------------

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, Month)

Drugs_lookup <- fread("Drugs_lookup.csv")
unique(Drugs_lookup$drug_class)
string_Orals <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Mabs <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_Preventive <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Preventive" & Drugs_lookup$drug_class != "Muscle Relaxant"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Symptomatic"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month==60)

RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% mutate(Box=ifelse(grepl(string_Orals, Treat), "O",
                                      ifelse(grepl(string_Mabs, Treat), "I",
                                             ifelse(grepl(string_Preventive, Treat) & grepl(string_Acute, Treat), "K",
                                                    ifelse(grepl(string_Preventive, Treat) & grepl(string_Symptomatic, Treat), "Y",
                                                           ifelse(grepl(string_Preventive, Treat), "P",
                                                                  ifelse(grepl(string_Acute, Treat), "T",
                                                                         ifelse(grepl(string_Symptomatic, Treat), "S", "X"))))))))


Boxes_V3 <- RIMUS23_Drug_Histories

Boxes_V3 %>% group_by(Box) %>% summarise(n=sum(as.numeric(weight))) 

sum(as.numeric(Boxes_V3$weight))

# -----------
# Type of migraine flow - Acute vs Preventive - Add Delete Substitute ---------
Drugs_lookup <- fread("Drugs_lookup.csv")

string_Acute <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "Triptans"|Drugs_lookup$drug_group == "Symptomatic"|Drugs_lookup$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_group == "CGRP Injectable"|Drugs_lookup$drug_group == "Preventive"], collapse = "|"),")\\b")

All_pats <- fread("ModSev_Pats_V3.txt")
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)
flMIG <- fread("MIG_Flows_Aux._Long_v3.txt")
flMIG <- All_pats %>% left_join(flMIG)

flMIG <- flMIG %>% filter(p2>=49) %>% filter(flow==1)  
flMIG <- flMIG %>% select(patient,weight,d1,d2,s1,s2,flow, stops)

# Acute Flow 

flMIG <- flMIG[, Acute_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Acute), str_c(unlist(str_extract_all(x, string_Acute)), collapse = ","),"")))]
flMIG <- flMIG[, Acute_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Acute), str_c(unlist(str_extract_all(x, string_Acute)), collapse = ","),"")))]
flMIG <- flMIG[, nr_Acute_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Acute)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_Acute_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Acute)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_AcuteUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Acute_d1","Acute_d2")] 
flMIG <- flMIG[, Acute_flow_type := ifelse(nr_Acute_d2 < nr_Acute_d1 & nr_AcuteUnq_d1d2 > nr_Acute_d1, "D+S", 
                                           ifelse(nr_Acute_d2 > nr_Acute_d1 & nr_AcuteUnq_d1d2 > nr_Acute_d2, "A+S",
                                                  ifelse(nr_Acute_d2 < nr_Acute_d1, "D", 
                                                         ifelse(nr_Acute_d2 > nr_Acute_d1, "A", 
                                                                ifelse(nr_Acute_d2 == nr_Acute_d1 & Acute_d2 != Acute_d1, "S","-")))))] 



# Prev Flow 
flMIG <- flMIG[, Prev_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Prev), str_c(unlist(str_extract_all(x, string_Prev)), collapse = ","),"")))]
flMIG <- flMIG[, Prev_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Prev), str_c(unlist(str_extract_all(x, string_Prev)), collapse = ","),"")))]
flMIG <- flMIG[, nr_Prev_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Prev)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_Prev_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Prev)*1), str_split(x,","))))]
flMIG <- flMIG[, nr_PrevUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Prev_d1","Prev_d2")] 
flMIG <- flMIG[, Prev_flow_type := ifelse(nr_Prev_d2 < nr_Prev_d1 & nr_PrevUnq_d1d2 > nr_Prev_d1, "D+S", 
                                            ifelse(nr_Prev_d2 > nr_Prev_d1 & nr_PrevUnq_d1d2 > nr_Prev_d2, "A+S",
                                                   ifelse(nr_Prev_d2 < nr_Prev_d1, "D", 
                                                          ifelse(nr_Prev_d2 > nr_Prev_d1, "A", 
                                                                 ifelse(nr_Prev_d2 == nr_Prev_d1 & Prev_d2 != Prev_d1, "S","-")))))] 



flMIG <- flMIG %>% mutate(ACUTE_FLOW=ifelse(Acute_flow_type!="-",1,0)) %>% mutate(PREV_FLOW=ifelse(Prev_flow_type!="-",1,0)) 

flMIG %>% group_by(ACUTE_FLOW, PREV_FLOW) %>% summarise(n=sum(as.numeric(weight)))

data.frame(flMIG %>% filter(stops==0) %>% group_by(Acute_flow_type, Prev_flow_type ) %>% summarise(n=sum(as.numeric(weight))))

# ----------------
# Physicians with vs without CGRP -------------------------

Unique_provcats <- fread("Unique_provcats.csv")
RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status!="G") %>% mutate(from_dt=as.Date(from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>%  select(patid, weight, provider, provcat, from_dt, drug_class) 

Year5 <- RIMUS23_Doses %>% filter(grepl("CGRP",drug_class  )) %>% filter(from_dt>="2022-06-01"&from_dt<="2023-05-31") %>% select(provider, provcat) %>% distinct()
Before5 <- RIMUS23_Doses %>% filter(grepl("CGRP",drug_class  )) %>% filter(from_dt<="2022-05-31") %>% select(provider, provcat) %>% distinct()

Year5 <- Year5 %>% anti_join(Before5)

NO_CGRP <- RIMUS23_Doses %>% select(provider, provcat) %>% distinct()  %>%
  anti_join(Year5) %>% anti_join(Before5)

Year5 <- Year5 %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% filter(TYPE=="PRIMARY CARE") %>% select(PROVCAT), by=c("provcat"="PROVCAT"))

NO_CGRP <- NO_CGRP %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% filter(TYPE=="PRIMARY CARE") %>% select(PROVCAT), by=c("provcat"="PROVCAT"))

Plus5Pats <- RIMUS23_Doses %>% select(provider, patid) %>% distinct() %>% group_by(provider) %>% count() %>% filter(n>20)


Year5 <- Year5 %>% inner_join(Plus5Pats %>% select(provider))
NO_CGRP <- NO_CGRP %>% inner_join(Plus5Pats %>% select(provider))

Scripts_first4years <-  RIMUS23_Doses %>% filter(from_dt<="2022-05-31") %>%
   group_by(provider) %>% count()

Scripts_first4years %>% inner_join(Year5) %>% ungroup() %>% summarise(mean=mean(n)) # 165
Scripts_first4years %>% inner_join(NO_CGRP) %>% ungroup() %>% summarise(mean=mean(n)) # 90

ClassesVolume <- RIMUS23_Doses %>% filter(from_dt<="2022-05-31") %>%
   group_by(provider, drug_class) %>% count() 

ClassesVolume <- ClassesVolume %>% ungroup() %>% group_by(provider) %>% mutate(total=sum(n))
ClassesVolume <- ClassesVolume %>% mutate(perc=n/total)



Year5 %>% inner_join(ClassesVolume %>% select(provider, drug_class,n)) %>% ungroup() %>%
  group_by(drug_class) %>% count() %>% ungroup() %>% mutate(total=sum(n)) %>% mutate(perc=100*n/total) %>%
  select(drug_class, perc)


NO_CGRP %>% inner_join(ClassesVolume %>% select(provider, drug_class,n)) %>% ungroup() %>%
  group_by(drug_class) %>% count() %>% ungroup() %>% mutate(total=sum(n)) %>% mutate(perc=100*n/total) %>%
  select(drug_class, perc)


Pats_seen <- RIMUS23_Doses %>% filter(from_dt<="2022-05-31") %>%  select(provider, patid) %>% distinct()

flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")
flMIG <- flMIG %>% filter(p2>=36&p2<48)
flMIG <- flMIG %>% group_by(patient) %>% summarise(flows=sum(flow))

Year5 %>% inner_join(Pats_seen) %>% 
  inner_join(flMIG %>% mutate(patient=as.character(patient)), by=c("patid"="patient")) %>%
  summarise(mean=mean(flows)) # 18.29121
  
NO_CGRP %>% inner_join(Pats_seen) %>% 
  inner_join(flMIG %>% mutate(patient=as.character(patient)), by=c("patid"="patient")) %>%
  summarise(mean=mean(flows)) # 18.52801

# ----------
 
# Number of Dx patients vs Rx patients per physician -------------------


Unique_provcats <- fread("Unique_provcats.csv")
unique(Unique_provcats$TYPE)
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")
Unique_provcats <- Unique_provcats %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                          ifelse(TYPE=="PRIMARY CARE", "PCP", "OTHER")))


`%notin%` <- Negate(`%in%`)

RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) 
unique(RIMUS23_Doses$drug_class)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, drug_class, provider , provcat, drug_group) %>% distinct() %>%
  filter(drug_class %notin% c("Ditan", "Ergot", "Triptan", "CGRP Oral", "CGRP Injectable"))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

N_pats_Rx <- RIMUS23_Doses %>% select(provider, TYPE, patid) %>% distinct() %>% group_by(provider, TYPE) %>% count()
names(N_pats_Rx)[3] <- "N_Rx"


RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date  =as.Date(date))
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, provider, provcat) %>% distinct()

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

N_pats_Dx <- RIMUS23_Migraine_Dxs %>% select(provider, TYPE, patid) %>% distinct() %>% group_by(provider, TYPE) %>% count()
names(N_pats_Dx)[3] <- "N_Dx"

N_pats <- N_pats_Dx %>% full_join(N_pats_Rx)
sum(is.na(N_pats))

N_pats[is.na(N_pats)] <- 0


N_pats %>% ungroup() %>%
  filter(TYPE=="NEUROLOGY") %>%
  sample_n(25000) %>%
  ggplot(aes((N_Dx), (N_Rx) )) +
  xlim(-1,10) + ylim(-1,10) +
  geom_jitter(alpha=0.5, size=0.2, colour="#FFBFBF") + 
  theme_minimal() +
  xlab("\n Number of Dx Patients") + ylab("Number of Rx Patients \n")


N_pats %>% ungroup() %>%
  filter(TYPE=="PCP") %>%
  sample_n(25000) %>%
  ggplot(aes((N_Dx), (N_Rx) )) +
  xlim(-1,10) + ylim(-1,10) +
  geom_jitter(alpha=0.5, size=0.2, colour="#B2D1DC") + 
  theme_minimal() +
  xlab("\n Number of Dx Patients") + ylab("Number of Rx Patients \n")



N_pats %>% ungroup() %>%
  filter(TYPE=="OTHER") %>%
  sample_n(25000) %>%
  ggplot(aes((N_Dx), (N_Rx) )) +
  xlim(-1,20) + ylim(-1,20) +
  geom_jitter(alpha=0.5, size=0.2, colour="#FFEFCA") + 
  theme_minimal() +
  xlab("\n Number of Dx Patients") + ylab("Number of Rx Patients \n")



N_pats %>% ungroup() %>% mutate(Ratio=N_Rx/(N_Rx+N_Dx)) %>%
  filter(TYPE!="OTHER") %>%
  ggplot(aes((Ratio), colour=TYPE, fill=TYPE )) +
  geom_density(alpha=0.75) + 
  theme_minimal() +
  xlab("\n Ratio of Rx to Rx+Dx Patients") + ylab("Physician Density \n") +
  scale_colour_manual(values=c("#FFBFBF", "#B2D1DC")) +
  scale_fill_manual(values=c("#FFBFBF", "#B2D1DC")) 


# -------------------------------
# Month over Month source of Dx and Scripts ------------- 
Unique_provcats <- fread("Unique_provcats.csv")
unique(Unique_provcats$TYPE)
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")
Unique_provcats <- Unique_provcats %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                          ifelse(TYPE=="PRIMARY CARE", "PCP", "OTHER")))



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) 
unique(RIMUS23_Doses$drug_class)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, drug_class, provider , provcat, drug_group) %>% distinct() 
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, provider , provcat) %>% distinct() 

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))


RIMUS23_Doses$TYPE <- paste0("Rx_", RIMUS23_Doses$TYPE)

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, TYPE) %>% distinct()

names(RIMUS23_Doses)[2] <- "date"

length(unique(RIMUS23_Doses$patid))
RIMUS23_Doses %>% filter(grepl("NEURO", TYPE)) %>% select(patid) %>% distinct()


RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date  =as.Date(date))
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, provider, provcat) %>% distinct()

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, TYPE) %>% distinct() 

RIMUS23_Migraine_Dxs$TYPE <- paste0("Dx_", RIMUS23_Migraine_Dxs$TYPE)

All_over_time <- RIMUS23_Doses %>% bind_rows(RIMUS23_Migraine_Dxs)

All_over_time$Month_Yr <- format(as.Date(All_over_time$date), "%Y-%m")
#All_over_time <- All_over_time %>%  mutate(date=str_sub(as.character(date), 1L, 7L))

All_over_time <- All_over_time %>% select(-date) %>% distinct() 
All_over_time <- All_over_time %>% arrange(patid, Month_Yr, TYPE)
range(All_over_time$Month_Yr)
All_over_time$Exp <- 1
All_over_time_wide <- All_over_time %>% spread(key=Month_Yr, value=Exp)


All_over_time_all_months <- All_over_time %>% select(patid) %>% distinct() %>% mutate(link=1)  %>%
  left_join(All_over_time %>% select(Month_Yr) %>% distinct() %>% mutate(link=1)) %>%
  select(-link) %>% left_join(All_over_time) %>% select(-Exp)

All_over_time_all_months[is.na(All_over_time_all_months)] <- "0"

All_over_time_all_months <- All_over_time_all_months %>% mutate(Exp=1) %>% spread(key=TYPE, value=Exp)
All_over_time_all_months <- All_over_time_all_months %>% select(-`0`)

All_over_time_all_months[is.na(All_over_time_all_months)] <- 0

All_over_time_all_months <- All_over_time_all_months %>%
  group_by(patid) %>%
  mutate(CUM_Dx_NEUROLOGY=cumsum(Dx_NEUROLOGY)) %>% mutate(CUM_Dx_NEUROLOGY=ifelse(CUM_Dx_NEUROLOGY==0,0,1)) %>%
  mutate(CUM_Dx_OTHER =cumsum(Dx_OTHER )) %>% mutate(CUM_Dx_OTHER=ifelse(CUM_Dx_OTHER ==0,0,1)) %>%
  mutate(CUM_Dx_PCP =cumsum(Dx_PCP )) %>% mutate(CUM_Dx_PCP=ifelse(CUM_Dx_PCP==0,0,1)) %>%
  mutate(CUM_Rx_NEUROLOGY =cumsum(Rx_NEUROLOGY )) %>% mutate(CUM_Rx_NEUROLOGY=ifelse(CUM_Rx_NEUROLOGY==0,0,1)) %>%
  mutate(CUM_Rx_OTHER=cumsum(Rx_OTHER)) %>% mutate(CUM_Rx_OTHER=ifelse(CUM_Rx_OTHER==0,0,1)) %>%
  mutate(CUM_Rx_PCP=cumsum(Rx_PCP)) %>% mutate(CUM_Rx_PCP=ifelse(CUM_Rx_PCP==0,0,1))  %>% ungroup()

data.frame(All_over_time_all_months %>% filter(Month_Yr==max(Month_Yr)) %>%
  group_by(  CUM_Dx_PCP, CUM_Dx_NEUROLOGY, CUM_Rx_PCP, CUM_Rx_NEUROLOGY) %>%
  count() %>% arrange(-n) )


All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

 
data.frame(All_over_time_all_months %>%
             inner_join(All_pats, by=c("patid"="patient")) %>%
             filter(Month_Yr==max(Month_Yr)) %>%
  group_by(  CUM_Dx_PCP, CUM_Dx_NEUROLOGY, CUM_Rx_PCP, CUM_Rx_NEUROLOGY) %>%
  count() %>% arrange(-n) )



All_over_time_all_months %>%
  inner_join(All_pats, by=c("patid"="patient")) %>%
  filter(CUM_Rx_PCP==1 |CUM_Rx_NEUROLOGY==1) %>%
  group_by(patid) %>% filter(Month_Yr==min(Month_Yr)) %>% select(patid, CUM_Rx_NEUROLOGY, CUM_Rx_PCP) %>%
  left_join(
    All_over_time_all_months %>%
      inner_join(All_pats, by=c("patid"="patient")) %>%
                   filter(Month_Yr==max(Month_Yr)) %>% select(patid, CUM_Rx_NEUROLOGY, CUM_Rx_PCP) %>%
                   rename("FINAL_CUM_Rx_NEUROLOGY"="CUM_Rx_NEUROLOGY", "FINAL_CUM_Rx_PCP"="CUM_Rx_PCP")
  ) %>%
  group_by(CUM_Rx_NEUROLOGY, CUM_Rx_PCP, FINAL_CUM_Rx_NEUROLOGY,FINAL_CUM_Rx_PCP) %>% count() %>% arrange(-n)
  

# ------------


# First Specialty vs Subsequent ones ------------

# Using Dx only 
Unique_provcats <- fread("Unique_provcats.csv")
unique(Unique_provcats$TYPE)
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")
Unique_provcats <- Unique_provcats %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                          ifelse(TYPE=="PRIMARY CARE", "PCP", 
                                                                 ifelse(TYPE=="INTERNAL MEDICINE", "OTHER",
                                                                        ifelse(TYPE=="INTERAL MEDICINE", "OTHER",
                                                                               ifelse(TYPE=="OBG", "OTHER",
                                                                                      ifelse(TYPE=="PSYCHIATRY", "OTHER", "EXCLUDE")))))))
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")


RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date  =as.Date(date))
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, provider, provcat) %>% distinct()

range(RIMUS23_Migraine_Dxs$date)

# Jun 18 to May 23

# Remove those with Rx first 12m
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% anti_join(RIMUS23_Migraine_Dxs %>% filter(date<"2019-06-16") %>% select(patid) %>% distinct())

# Select thos ewith first on yera 2 or year 3
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% inner_join(RIMUS23_Migraine_Dxs %>% filter(date<"2021-06-16") %>% select(patid) %>% distinct())



RIMUS23_Doses %>% select(patid, from_dt) %>% distinct() %>% group_by(patid) %>% 
  count() %>% ungroup() %>% summarise(median=mean(n))


RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, TYPE) %>% distinct() 

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% left_join(
  RIMUS23_Migraine_Dxs %>% group_by(patid) %>% filter(date==min(date)) %>% filter(TYPE==min(TYPE)) %>% distinct() %>%
  select(patid, TYPE) %>% rename("First_Dx"="TYPE") 
  )

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% arrange(patid, date, TYPE)

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>%  group_by(patid) %>% mutate(grp = rle(TYPE)$lengths %>% {rep(seq(length(.)), .)}) 

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% left_join(
  RIMUS23_Migraine_Dxs %>% group_by(patid) %>% filter(grp==max(grp)) %>% distinct() %>%
  select(patid, grp) %>% rename("Lines"="grp") %>% distinct() 
  )


RIMUS23_Migraine_Dxs %>% select(patid, TYPE, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines==1) %>% select(patid, First_Dx) %>% distinct() %>% group_by(First_Dx) %>% count()


RIMUS23_Migraine_Dxs %>% select(patid, TYPE, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% select(patid, First_Dx, TYPE) %>% distinct() %>%
  arrange(patid, First_Dx, TYPE) %>% group_by(patid) %>% mutate(TYPE=paste0(TYPE, collapse=",")) %>% distinct() %>%
  group_by(First_Dx, TYPE) %>% count()

# Using MIG -spec Rx only


Unique_provcats <- fread("Unique_provcats.csv")
unique(Unique_provcats$TYPE)
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")
Unique_provcats <- Unique_provcats %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                          ifelse(TYPE=="PRIMARY CARE", "PCP", 
                                                                 ifelse(TYPE=="INTERNAL MEDICINE", "OTHER",
                                                                        ifelse(TYPE=="INTERAL MEDICINE", "OTHER",
                                                                               ifelse(TYPE=="OBG", "OTHER",
                                                                                      ifelse(TYPE=="PSYCHIATRY", "OTHER", "EXCLUDE")))))))
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) 
unique(RIMUS23_Doses$drug_class)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class %in% c("Ditan", "Ergot", "Triptan", "CGRP Oral", "CGRP Injectable"))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, provider , provcat) %>% distinct() 

# Jun 18 to May 23

# Remove those with Rx first 12m
RIMUS23_Doses <- RIMUS23_Doses %>% anti_join(RIMUS23_Doses %>% filter(from_dt<"2019-06-16") %>% select(patid) %>% distinct())

# Select thos ewith first on yera 2 or year 3
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(RIMUS23_Doses %>% filter(from_dt<"2021-06-16") %>% select(patid) %>% distinct())

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, TYPE) %>% distinct() 

RIMUS23_Doses <- RIMUS23_Doses %>% left_join(
  RIMUS23_Doses %>% group_by(patid) %>% filter(from_dt==min(from_dt)) %>% filter(TYPE==min(TYPE)) %>% distinct() %>%
  select(patid, TYPE) %>% rename("First_Rx"="TYPE") 
  )

RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, from_dt, TYPE)

RIMUS23_Doses <- RIMUS23_Doses %>%  group_by(patid) %>% mutate(grp = rle(TYPE)$lengths %>% {rep(seq(length(.)), .)}) 

RIMUS23_Doses <- RIMUS23_Doses %>% left_join(
  RIMUS23_Doses %>% group_by(patid) %>% filter(grp==max(grp)) %>% distinct() %>%
  select(patid, grp) %>% rename("Lines"="grp") %>% distinct() 
  )


RIMUS23_Doses %>% select(patid, TYPE, First_Rx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines==1) %>% select(patid, First_Rx) %>% distinct() %>% group_by(First_Rx) %>% count()


RIMUS23_Doses %>% select(patid, TYPE, First_Rx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% select(patid, First_Rx, TYPE) %>% distinct() %>%
  arrange(patid, First_Rx, TYPE) %>% group_by(patid) %>% mutate(TYPE=paste0(TYPE, collapse=",")) %>% distinct() %>%
  group_by(First_Rx, TYPE) %>% count()




# Using ANY  Rx

Unique_provcats <- fread("Unique_provcats.csv")
unique(Unique_provcats$TYPE)
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")
Unique_provcats <- Unique_provcats %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                          ifelse(TYPE=="PRIMARY CARE", "PCP", 
                                                                 ifelse(TYPE=="INTERNAL MEDICINE", "OTHER",
                                                                        ifelse(TYPE=="INTERAL MEDICINE", "OTHER",
                                                                               ifelse(TYPE=="OBG", "OTHER",
                                                                                      ifelse(TYPE=="PSYCHIATRY", "OTHER", "EXCLUDE")))))))
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) 
unique(RIMUS23_Doses$drug_class)
# RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class %in% c("Ditan", "Ergot", "Triptan", "CGRP Oral", "CGRP Injectable"))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, provider , provcat) %>% distinct() 

# Jun 18 to May 23

# Remove those with Rx first 12m
RIMUS23_Doses <- RIMUS23_Doses %>% anti_join(RIMUS23_Doses %>% filter(from_dt<"2019-06-16") %>% select(patid) %>% distinct())

# Select thos ewith first on yera 2 or year 3
RIMUS23_Doses <- RIMUS23_Doses %>% inner_join(RIMUS23_Doses %>% filter(from_dt<"2021-06-16") %>% select(patid) %>% distinct())

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, TYPE) %>% distinct() 

RIMUS23_Doses <- RIMUS23_Doses %>% left_join(
  RIMUS23_Doses %>% group_by(patid) %>% filter(from_dt==min(from_dt)) %>% filter(TYPE==min(TYPE)) %>% distinct() %>%
  select(patid, TYPE) %>% rename("First_Rx"="TYPE") 
  )

RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid, from_dt, TYPE)

RIMUS23_Doses <- RIMUS23_Doses %>%  group_by(patid) %>% mutate(grp = rle(TYPE)$lengths %>% {rep(seq(length(.)), .)}) 

RIMUS23_Doses <- RIMUS23_Doses %>% left_join(
  RIMUS23_Doses %>% group_by(patid) %>% filter(grp==max(grp)) %>% distinct() %>%
  select(patid, grp) %>% rename("Lines"="grp") %>% distinct() 
  )


RIMUS23_Doses %>% select(patid, TYPE, First_Rx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines==1) %>% select(patid, First_Rx) %>% distinct() %>% group_by(First_Rx) %>% count()


RIMUS23_Doses %>% select(patid, TYPE, First_Rx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% select(patid, First_Rx, TYPE) %>% distinct() %>%
  arrange(patid, First_Rx, TYPE) %>% group_by(patid) %>% mutate(TYPE=paste0(TYPE, collapse=",")) %>% distinct() %>%
  group_by(First_Rx, TYPE) %>% count()



# ALl Dx and Rx

RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date  =as.Date(date))
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, provider, provcat) %>% distinct()

range(RIMUS23_Migraine_Dxs$date)



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) 
unique(RIMUS23_Doses$drug_class)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, provider , provcat) %>% distinct()  %>% rename("date"="from_dt")

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% bind_rows(RIMUS23_Doses)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% distinct() 


# Remove those with Rx first 12m
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% anti_join(RIMUS23_Migraine_Dxs %>% filter(date<"2019-06-16") %>% select(patid) %>% distinct())

# Select thos ewith first on yera 2 or year 3
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% inner_join(RIMUS23_Migraine_Dxs %>% filter(date<"2021-06-16") %>% select(patid) %>% distinct())



RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, TYPE) %>% distinct() 

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% left_join(
  RIMUS23_Migraine_Dxs %>% group_by(patid) %>% filter(date==min(date)) %>% filter(TYPE==min(TYPE)) %>% distinct() %>%
  select(patid, TYPE) %>% rename("First_Dx"="TYPE") 
  )

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% arrange(patid, date, TYPE)

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>%  group_by(patid) %>% mutate(grp = rle(TYPE)$lengths %>% {rep(seq(length(.)), .)}) 

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% left_join(
  RIMUS23_Migraine_Dxs %>% group_by(patid) %>% filter(grp==max(grp)) %>% distinct() %>%
  select(patid, grp) %>% rename("Lines"="grp") %>% distinct() 
  )


RIMUS23_Migraine_Dxs %>% select(patid, TYPE, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines==1) %>% select(patid, First_Dx) %>% distinct() %>% group_by(First_Dx) %>% count()


RIMUS23_Migraine_Dxs %>% select(patid, TYPE, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% select(patid, First_Dx, TYPE) %>% distinct() %>%
  arrange(patid, First_Dx, TYPE) %>% group_by(patid) %>% mutate(TYPE=paste0(TYPE, collapse=",")) %>% distinct() %>%
  group_by(First_Dx, TYPE) %>% count()


# -----------

# Depression and Anxiety and PCP ModSev patients and Oral CGRPs -------------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
ModSev_vector <- ModSev_vector %>% filter(group=="ModSev") %>% select(patient, weight)
sum(as.numeric(ModSev_vector$weight)) # 11,856,517


Unique_provcats <- fread("Unique_provcats.csv")
unique(Unique_provcats$TYPE)
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")
Unique_provcats <- Unique_provcats %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                          ifelse(TYPE=="PRIMARY CARE", "PCP", 
                                                                 ifelse(TYPE=="INTERNAL MEDICINE", "OTHER",
                                                                        ifelse(TYPE=="INTERAL MEDICINE", "OTHER",
                                                                               ifelse(TYPE=="OBG", "OTHER",
                                                                                      ifelse(TYPE=="PSYCHIATRY", "OTHER", "EXCLUDE")))))))
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")
Unique_provcats <- Unique_provcats %>% filter(TYPE=="PCP")
Unique_provcats <- Unique_provcats %>% select(PROVCAT)


RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date  =as.Date(date))
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, provider, provcat) %>% distinct()
RIMUS23_Migraine_Dxs$date <- as.Date(RIMUS23_Migraine_Dxs$date)
range(RIMUS23_Migraine_Dxs$date)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% filter(date>"2021-05-15")
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(provcat=as.numeric(provcat)) %>% inner_join(Unique_provcats, by=c("provcat"="PROVCAT"))
PCP_last_2_years <- RIMUS23_Migraine_Dxs %>% select(patid) %>% distinct()
names(PCP_last_2_years) <- "patient"

PCP_last_2_years %>% inner_join(ModSev_vector) %>% summarise(tot=sum(as.numeric(weight))) # 3,369,484



RIMUS23_Comorbidities_Extended_Dxs <- read.table("RIMUS23 Comorbidities Extended Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Comorbidities_Extended_Dxs)[1] <- "patient"
RIMUS23_Comorbidities_Extended_Dxs$date <- as.Date(RIMUS23_Comorbidities_Extended_Dxs$date)
range(RIMUS23_Comorbidities_Extended_Dxs$date)
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% filter(date>"2021-05-15")

RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patient, ICD10_diag) %>% distinct() %>% 
  inner_join(ModSev_vector %>% select(patient))

RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>%
  filter(grepl("F3",ICD10_diag)| grepl("F4",ICD10_diag))


RIMUS23_Comorbidities_Extended_Dxs %>% left_join(ModSev_vector) %>% inner_join(PCP_last_2_years) %>%
  filter(grepl("F3",ICD10_diag)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 1,403,637


RIMUS23_Comorbidities_Extended_Dxs %>% left_join(ModSev_vector) %>%  inner_join(PCP_last_2_years) %>%
  filter(grepl("F4",ICD10_diag)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 1,851,299


# Up to 50% each one would be okayish
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9719608/
# https://bmcneurol.biomedcentral.com/articles/10.1186/s12883-014-0238-4#:~:text=Anxiety%20is%20the%20most%20common,12%5D%2C%5B13%5D.


RIMUS23_Comorbidities_Extended_Dxs %>% left_join(ModSev_vector) %>%  inner_join(PCP_last_2_years) %>%
   filter(grepl("F33",ICD10_diag)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 743,777


RIMUS23_Comorbidities_Extended_Dxs %>% left_join(ModSev_vector) %>% inner_join(PCP_last_2_years) %>%
   filter(grepl("F41",ICD10_diag)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 1,670,385


RIMUS23_Comorbidities_Extended_Dxs %>% left_join(ModSev_vector) %>% inner_join(PCP_last_2_years) %>%
   filter(grepl("F41",ICD10_diag)|grepl("F33",ICD10_diag)) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 1,828,165


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, drug_class) %>% distinct() %>% 
  mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt>"2021-05-15")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral") %>% select(patid) %>% rename("patient"="patid") %>% distinct()

RIMUS23_Doses %>% inner_join(ModSev_vector) %>%  inner_join(PCP_last_2_years) %>% summarise(n=sum(as.numeric(weight))) # 342443


RIMUS23_Comorbidities_Extended_Dxs %>% left_join(ModSev_vector) %>%  inner_join(PCP_last_2_years) %>%
   filter(grepl("F33",ICD10_diag)) %>% inner_join(RIMUS23_Doses) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 93,566


RIMUS23_Comorbidities_Extended_Dxs %>% left_join(ModSev_vector) %>% inner_join(PCP_last_2_years) %>%
   filter(grepl("F41",ICD10_diag)) %>% inner_join(RIMUS23_Doses) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 199,370


RIMUS23_Comorbidities_Extended_Dxs %>% left_join(ModSev_vector) %>% inner_join(PCP_last_2_years) %>%
   filter(grepl("F41",ICD10_diag)|grepl("F33",ICD10_diag)) %>% inner_join(RIMUS23_Doses) %>%
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 216,259


# --------

# Create vector acute vs preventive last 48 months ------
All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

Drugs_lookup <- fread("Drugs_lookup.csv")
RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- All_pats %>% left_join(RIMUS23_Drug_Histories) 
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, X1:X60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Month>=49) 
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-c(Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")  %>% distinct()
RIMUS23_Drug_Histories <- separate_rows(RIMUS23_Drug_Histories, Treat, sep = ",", convert=T )
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% distinct() %>% left_join(Drugs_lookup %>% select(drug_id, drug_group), by=c("Treat"="drug_id"))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(patient, weight, drug_group) %>% distinct()
RIMUS23_Drug_Histories$exp <- 1
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% spread(key=drug_group, value=exp)
RIMUS23_Drug_Histories[is.na(RIMUS23_Drug_Histories)] <- 0
Experience <- RIMUS23_Drug_Histories

Experience <- Experience %>% mutate(group=ifelse(`CGRP Injectable`==1|Preventive==1, "Prev", "Acute"))

fwrite(Experience, "Experience_Acute_vs_Prev_last12m.txt")
Experience <- fread("Experience_Acute_vs_Prev_last12m.txt")

Experience %>% filter(Triptans==0&Symptomatic==0&Preventive==0&`CGRP Injectable`==0&`CGRP Oral`==0)


Experience %>% group_by(group) %>% count()

Experience <- fread("Experience_Acute_vs_Prev_last48m.txt")




# ------
# First Specialty vs Subsequent ones  Acute vs Prev pats ------------

# Using Dx only 
Unique_provcats <- fread("Unique_provcats.csv")
unique(Unique_provcats$TYPE)
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")
Unique_provcats <- Unique_provcats %>% mutate(TYPE=ifelse(TYPE=="NEUROLOGY", "NEUROLOGY",
                                                          ifelse(TYPE=="PRIMARY CARE", "PCP", 
                                                                 ifelse(TYPE=="INTERNAL MEDICINE", "OTHER",
                                                                        ifelse(TYPE=="INTERAL MEDICINE", "OTHER",
                                                                               ifelse(TYPE=="OBG", "OTHER",
                                                                                      ifelse(TYPE=="PSYCHIATRY", "OTHER", "EXCLUDE")))))))
Unique_provcats <- Unique_provcats %>% filter(TYPE!="EXCLUDE")



# ALl Dx and Rx  MIG spec

RIMUS23_Migraine_Dxs  <- fread("RIMUS23 Migraine Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(date  =as.Date(date))
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, provider, provcat) %>% distinct()

range(RIMUS23_Migraine_Dxs$date)



RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt=as.Date(from_dt)) 
unique(RIMUS23_Doses$drug_class)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class %in% c("Ditan", "Ergot", "Triptan", "CGRP Oral", "CGRP Injectable"))
RIMUS23_Doses <- RIMUS23_Doses %>% select(patid, from_dt, provider , provcat) %>% distinct()  %>% rename("date"="from_dt")

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% bind_rows(RIMUS23_Doses)
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% distinct() 


# Remove those with Rx first 12m
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% anti_join(RIMUS23_Migraine_Dxs %>% filter(date<"2019-06-16") %>% select(patid) %>% distinct())

# Select thos ewith first on yera 2 or year 3
RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% inner_join(RIMUS23_Migraine_Dxs %>% filter(date<"2021-06-16") %>% select(patid) %>% distinct())



RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% mutate(provcat=as.numeric(provcat)) %>%
  inner_join(Unique_provcats %>% select(PROVCAT, TYPE), by=c("provcat"="PROVCAT"))

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% select(patid, date, TYPE) %>% distinct() 

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% left_join(
  RIMUS23_Migraine_Dxs %>% group_by(patid) %>% filter(date==min(date)) %>% filter(TYPE==min(TYPE)) %>% distinct() %>%
  select(patid, TYPE) %>% rename("First_Dx"="TYPE") 
  )

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% arrange(patid, date, TYPE)

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>%  group_by(patid) %>% mutate(grp = rle(TYPE)$lengths %>% {rep(seq(length(.)), .)}) 

RIMUS23_Migraine_Dxs <- RIMUS23_Migraine_Dxs %>% left_join(
  RIMUS23_Migraine_Dxs %>% group_by(patid) %>% filter(grp==max(grp)) %>% distinct() %>%
  select(patid, grp) %>% rename("Lines"="grp") %>% distinct() 
  )


RIMUS23_Migraine_Dxs %>% select(patid, TYPE, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines==1) %>% select(patid, First_Dx) %>% distinct() %>% group_by(First_Dx) %>% count()


RIMUS23_Migraine_Dxs %>% select(patid, TYPE, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% select(patid, First_Dx, TYPE) %>% distinct() %>%
  arrange(patid, First_Dx, TYPE) %>% group_by(patid) %>% mutate(TYPE=paste0(TYPE, collapse=",")) %>% distinct() %>%
  group_by(First_Dx, TYPE) %>% count()



RIMUS23_Migraine_Dxs %>% select(patid, TYPE, date, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% select(patid, First_Dx, date, TYPE) %>% distinct() %>%
  group_by(patid, First_Dx) %>% summarise(last=max(date)) %>%
  filter(First_Dx=="PCP") %>%
  mutate(elapsed=as.numeric(as.Date("2023-07-15")-last)/30) %>% # ungroup() %>% summarise(median=median(elapsed)) #9
  ggplot(aes(elapsed)) +
  geom_density(alpha=0.7, fill="firebrick", colour="firebrick") +
  theme_minimal() +
  xlab("\n Number of months since last event") + ylab("Patient density")



temp <- RIMUS23_Migraine_Dxs %>% select(patid, TYPE, date, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% select(patid, First_Dx, date, TYPE) %>% distinct() %>% 
  group_by(patid, First_Dx, TYPE) %>% count() %>%
  filter(First_Dx=="PCP") %>% ungroup() %>%
  group_by(patid, First_Dx) %>% mutate(total=sum(n)) %>%
  spread(key=TYPE, value=n)

temp[is.na(temp)] <- 0

median(temp$total) ; mean(temp$total)

temp %>% ggplot(aes(total)) + geom_density()

data.frame(temp %>% group_by(PCP, NEUROLOGY, OTHER) %>% count() %>% arrange(-n))

Experience_Acute_vs_Prev_last48m <- fread("Experience_Acute_vs_Prev_last48m.txt")

Experience_Acute_vs_Prev_last48m <- Experience_Acute_vs_Prev_last48m %>% 
  mutate(group2=ifelse(`CGRP Injectable`==1 | `CGRP Oral`==1, "Prev",
                       ifelse(Triptans==1, "Acute", NA))) %>% drop_na()

Experience_Acute_vs_Prev_last48m <- Experience_Acute_vs_Prev_last48m %>% select(patient, group2) %>% 
  rename("patid"="patient") %>%  select(patid)

Experience_Acute_vs_Prev_last48m$patid <- as.character(Experience_Acute_vs_Prev_last48m$patid)

RIMUS23_Migraine_Dxs %>% inner_join(Experience_Acute_vs_Prev_last48m) %>%
  filter(Lines==1) %>% filter(First_Dx=="OTHER")  %>%   select(patid)  %>% distinct()

RIMUS23_Migraine_Dxs %>% inner_join(Experience_Acute_vs_Prev_last48m) %>%
  filter(Lines!=1) %>% filter(First_Dx=="OTHER") %>% 
  group_by(patid) %>% count() # 1075

Pats_10_rec <- RIMUS23_Migraine_Dxs %>% inner_join(Experience_Acute_vs_Prev_last48m) %>%
  filter(Lines!=1) %>% filter(First_Dx=="OTHER") %>% 
  group_by(patid) %>% count() %>% filter(n>=6) %>%   select(patid)  # 

RIMUS23_Migraine_Dxs %>% inner_join(Pats_10_rec) %>% select(patid, TYPE, date, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% filter(First_Dx=="OTHER") %>%  select(patid, date, TYPE) %>% distinct() %>%
  filter(date>"2022-06-16" & TYPE =="OTHER") %>% select(patid) %>% distinct() # 


#  596 44% PCP last year -> LEFT AND return
#  770 56% NO PCP last year -> LEFT long time ( 476 35% with other last year)


RIMUS23_Migraine_Dxs %>% inner_join(Pats_10_rec) %>% anti_join(
  RIMUS23_Migraine_Dxs %>% select(patid, TYPE, date, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% filter(First_Dx=="PCP") %>%  select(patid, date, TYPE) %>% distinct() %>%
  filter(date>"2022-06-16" & TYPE =="PCP") %>% select(patid) %>% distinct()
) %>% select(patid, TYPE, date, First_Dx, Lines) %>% distinct() %>% ungroup() %>%
  filter(Lines!=1) %>% filter(First_Dx=="PCP") %>%  select(patid, date, TYPE) %>% distinct() %>% 
  filter(date>"2022-06-16") %>% select(patid) %>% distinct()

# -------------------

# Destination stock of Outflows from Oral CGRP - Acute vs Preventive patients -----------
All_pats <- fread("ModSev_Pats_V3.txt")
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

flMIG <- fread("MIG_Flows_Aux._Long_v2.txt")

flMIG <- flMIG %>% select(patient, weight, s1, s2, p2, p2, stops, starts)

flMIG %>% inner_join(All_pats) %>% 
  filter(p2>=49 ) %>% filter(s1=="O"&s2!="O") %>% group_by(s2) %>% summarise(n=sum(weight))


RIMUS23_Doses <- read.table("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses <- RIMUS23_Doses %>% filter(drug_class=="CGRP Oral")
RIMUS23_Doses <- RIMUS23_Doses %>% filter(status != "G") %>% select(-c( status, code, NPI))
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(from_dt = as.Date(from_dt))  %>% filter(from_dt>="2022-06-16"&from_dt<="2023-05-15") %>% filter(days_sup  != "")
RIMUS23_Doses <- RIMUS23_Doses %>% arrange(patid,  from_dt) 
RIMUS23_Doses <- RIMUS23_Doses %>% group_by(patid) %>% mutate(elapsed=as.numeric(lead(from_dt)-from_dt))
RIMUS23_Doses <- RIMUS23_Doses %>% drop_na()
data.frame(RIMUS23_Doses) 
RIMUS23_Doses <- RIMUS23_Doses %>% filter(elapsed <= 92)
RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=30*(as.numeric(qty)/elapsed))

RIMUS23_Doses <- RIMUS23_Doses %>% mutate(rate=ifelse(elapsed==0, 0, rate)) %>%
  group_by(generic, patid, weight) %>% summarise(mean=mean(rate)) %>%
 mutate(mean=ifelse(mean>=13, "Prev", "Acute")) %>%
  ungroup() 

RIMUS23_Doses %>% select(patid, generic, mean) %>% mutate(patid=as.numeric(patid)) %>%
  inner_join(flMIG  %>% mutate(patient=as.numeric(patient)) , by=c("patid"="patient")) %>%
  filter(p2>=49 ) %>% filter(s1=="O"&s2!="O") %>% group_by(mean, s2) %>% summarise(n=sum(weight)) %>%
  spread(key=mean, value=n)


# -----------

# % Triptan Usage per comorbidity ----------------------
RIMUS23_Comorbidities_Extended_Dxs <- read.table("RIMUS23 Comorbidities Extended Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Comorbidities_Extended_Dxs)[1] <- "patient"
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patient, ICD10_diag) %>% distinct()



CAD_pats <- RIMUS23_Comorbidities_Extended_Dxs %>% filter(grepl("I25", ICD10_diag)) %>% select(patient) %>% distinct()
CAD_pats$CAD <- "CAD"
MI_pats <- RIMUS23_Comorbidities_Extended_Dxs %>% filter(grepl("I20", ICD10_diag)|
                                                           grepl("I21", ICD10_diag)|
                                                           grepl("I22", ICD10_diag)|
                                                           grepl("I23", ICD10_diag)|
                                                           grepl("I24", ICD10_diag)) %>% select(patient) %>% distinct()
MI_pats$MI <- "MI"
HTN_pats <- RIMUS23_Comorbidities_Extended_Dxs %>% filter(grepl("I10", ICD10_diag)|
                                                           grepl("I11", ICD10_diag)|
                                                           grepl("I12", ICD10_diag)|
                                                           grepl("I13", ICD10_diag)) %>% select(patient) %>% distinct()
HTN_pats$HTN <- "HTN"
Stroke_pats <- RIMUS23_Comorbidities_Extended_Dxs %>% filter(grepl("I63", ICD10_diag)|
                                                           grepl("I65", ICD10_diag)|
                                                           grepl("I66", ICD10_diag)|
                                                           grepl("I67", ICD10_diag)|
                                                           grepl("I68", ICD10_diag)|
                                                           grepl("I69", ICD10_diag)) %>% select(patient) %>% distinct()
Stroke_pats$STROKE <- "STROKE"

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, weight, AGE) %>% rename("patient"="patid")  
# RIMUS23_Demographics <- RIMUS23_Demographics %>% mutate(AGE=ifelse(AGE<30, "18-30",
#                                                                    ifelse(AGE<50, "30-50",
#                                                                           ifelse(AGE<65,"50-65", ">65"))))
# 



Drugs_lookup <- fread("Drugs_lookup.csv")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "Triptan"], collapse = "|"),")\\b")


RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(grepl(string_Triptan, Treat)) %>% select(patient) %>% distinct()
RIMUS23_Drug_Histories$TRIPTAN <- "TRIPTAN"

length(unique(RIMUS23_Demographics$patient)) # 263848
RIMUS23_Demographics$ALL <- "ALL"

RIMUS23_Demographics <- RIMUS23_Demographics %>% left_join(CAD_pats) %>% left_join(MI_pats) %>% left_join(Stroke_pats) %>% 
  left_join(HTN_pats) %>%  left_join(RIMUS23_Drug_Histories) 



RIMUS23_Demographics %>% group_by(CAD) %>% summarise(n=sum(as.numeric(weight))) # 0.103605 
RIMUS23_Demographics %>% group_by(HTN) %>% summarise(n=sum(as.numeric(weight))) # 0.4117974 
RIMUS23_Demographics %>% group_by(MI) %>% summarise(n=sum(as.numeric(weight))) # 0.05831457 
RIMUS23_Demographics %>% group_by(STROKE) %>% summarise(n=sum(as.numeric(weight))) # 0.1124657 
RIMUS23_Demographics %>% mutate(ANY=ifelse(CAD=="CAD"|STROKE=="STROKE"|MI=="MI", "ANY", NA)) %>%
  group_by(ANY) %>% summarise(n=sum(as.numeric(weight))) # 0.1904962 


RIMUS23_Demographics %>% group_by(ALL, TRIPTAN) %>% summarise(n=sum(as.numeric(weight))) # 0.3910763 overall
RIMUS23_Demographics %>% group_by(CAD, TRIPTAN) %>% summarise(n=sum(as.numeric(weight))) # 0.2568381 CAD 0.4065916 no-CAD
RIMUS23_Demographics %>% group_by(HTN, TRIPTAN) %>% summarise(n=sum(as.numeric(weight))) # 0.3422072 HTN 0.4252893 no-HTN
RIMUS23_Demographics %>% group_by(MI, TRIPTAN) %>% summarise(n=sum(as.numeric(weight))) # 0.2757952 MI 0.3982152 no-MI
RIMUS23_Demographics %>% group_by(STROKE, TRIPTAN) %>% summarise(n=sum(as.numeric(weight))) # 0.2702569 STROKE 0.4063862 no-STROKE
RIMUS23_Demographics %>% mutate(ANY=ifelse(CAD=="CAD"|STROKE=="STROKE"|MI=="MI", "ANY", NA)) %>%
  group_by(ANY,TRIPTAN) %>% summarise(n=sum(as.numeric(weight))) # 0.1904962 

RIMUS23_Demographics %>% group_by(AGE, TRIPTAN) %>% summarise(n=sum(as.numeric(weight))) 

RIMUS23_Demographics %>% group_by(AGE, HTN) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=HTN, value=n) %>% mutate(perc=HTN/(HTN+`<NA>`)) %>%
  mutate(AGE=as.numeric(AGE)) %>%
  ggplot(aes(AGE, perc*100)) +
  geom_point(colour="firebrick", shape = 1, size=2, stroke=2) +
  theme_minimal() +
  ylab("HTN Prevalence \n") + xlab("\n Age (years)") +
  ylim(0,100)


RIMUS23_Demographics %>% group_by(AGE, MI) %>% summarise(n=sum(as.numeric(weight))) %>%
  mutate(AGE=as.numeric(AGE)) %>%
  ggplot(aes(AGE, n, colour=MI, fill=MI)) +
  geom_point( shape = 1, size=2, stroke=2) +
  theme_minimal() +
  ylab("MI Population \n") + xlab("\n Age (years)") + ylim(0,500000)








RIMUS23_Demographics %>% mutate(ANY=ifelse(CAD=="CAD"|STROKE=="STROKE"|MI=="MI", "ANY", NA)) %>%
  group_by(AGE, ANY) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=ANY, value=n) %>% mutate(perc=ANY/(ANY+`<NA>`)) %>%
  mutate(AGE=as.numeric(AGE)) %>%
  ggplot(aes(AGE, perc*100)) +
  geom_point(colour="firebrick", shape = 1, size=2, stroke=2) +
  theme_minimal() +
  ylab("CAD|MI|STROKE Prevalence \n") + xlab("\n Age (years)") +
  ylim(0,100)



RIMUS23_Demographics %>% mutate(ANY=ifelse(CAD=="CAD"|STROKE=="STROKE"|MI=="MI", "ANY", NA)) %>%
  group_by(AGE, ANY) %>% summarise(n=sum(as.numeric(weight))) %>%
  mutate(AGE=as.numeric(AGE)) %>%
  ggplot(aes(AGE, n, colour=ANY, fill=ANY)) +
  geom_point( shape = 1, size=2, stroke=2) +
  theme_minimal() +
  ylab("MI|CAD|STROKE Population \n") + xlab("\n Age (years)") + ylim(0,500000)




# --------------
# Model ICD10s with Triptans vs no Triptans ------------
Drugs_lookup <- fread("Drugs_lookup.csv")
string_Triptan <- paste0("\\b(",paste0(Drugs_lookup$drug_id[Drugs_lookup$drug_class == "Triptan"], collapse = "|"),")\\b")

RIMUS23_Drug_Histories <- read.table("RIMUS23_Drug_Histories_NEW_short_2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, `X1`:`X60`, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(Treat != "-")
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-Month) %>% distinct()
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(grepl(string_Triptan, Treat)) %>% select(patient) %>% distinct()
RIMUS23_Drug_Histories$TRIPTAN <- 1

RIMUS23_Comorbidities_Extended_Dxs <- read.table("RIMUS23 Comorbidities Extended Dxs.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(RIMUS23_Comorbidities_Extended_Dxs)[1] <- "patient"
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(patient, ICD10_diag) %>% distinct()
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs  %>% mutate(ICD10_diag=str_sub(as.character(ICD10_diag), 1L, 3L)) %>% distinct()

sort(unique(RIMUS23_Comorbidities_Extended_Dxs$ICD10_diag))

RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% mutate(exp=1) %>% spread(key=ICD10_diag, value=exp)

RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% left_join(RIMUS23_Drug_Histories)

RIMUS23_Comorbidities_Extended_Dxs[is.na(RIMUS23_Comorbidities_Extended_Dxs)] <- 0

dim(RIMUS23_Comorbidities_Extended_Dxs)

RIMUS23_Demographics <- read.table("RIMUS23 Demographics.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Demographics <- RIMUS23_Demographics %>% select(patid, GENDER, AGE) %>% rename("patient"="patid")  
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% left_join(RIMUS23_Demographics)

RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(-patient)

RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% mutate(GENDER=ifelse(GENDER=="M",1,0)) 

RIMUS23_Comorbidities_Extended_Dxs

library(randomForest)
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% mutate(AGE=as.numeric(AGE))
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% mutate(TRIPTAN=as.factor(TRIPTAN))
RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% sele-AGE
train <- RIMUS23_Comorbidities_Extended_Dxs %>% group_by(TRIPTAN) %>% sample_n(5000) %>% ungroup()

modelAll_1_randomForest <- randomForest(TRIPTAN ~ ., data = train)

summary(modelAll_1_randomForest)

Importance_df <- data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)
Importance_df$VAR <- rownames(Importance_df)
names(train)

summary_means <- train %>%
  group_by(TRIPTAN) %>%
  summarize(across(everything(), mean))

summary_means <- summary_means %>%
  pivot_longer(cols = -TRIPTAN, names_to = "VAR", values_to = "Mean") %>%
  spread(key=TRIPTAN, value=Mean) %>% mutate(Pos=ifelse(`1`>`0`,1,0))

Importance_df <- Importance_df %>% left_join(summary_means)

Importance_df <- Importance_df %>% mutate(MeanDecreaseGini=ifelse(Pos==1, MeanDecreaseGini, -MeanDecreaseGini))
data.frame(Importance_df)

Importance_df)

RIMUS23_Comorbidities_Extended_Dxs <- RIMUS23_Comorbidities_Extended_Dxs %>% select(-c(GENDER, AGE))

PCA.pr_short <- prcomp(RIMUS23_Comorbidities_Extended_Dxs[, -324], center = TRUE, scale = TRUE)
summary(PCA.pr_short)

PCA.pr_short$rotation[,1:10]

my.var = varimax(PCA.pr_short$rotation)
myvarshort <-my.var$loadings[,1:10]
myvarshort <- as.data.frame(myvarshort)

x_y_plot_comorb_prev_triptan <- fread("x_y_plot_comorb_prev_triptan.csv")


library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(x_y_plot_comorb_prev_triptan, aes(x=prev, y=triptan, size = 100*triptan, fill=label, colour=label)) +
  geom_point(alpha=0.7, size=10)+
  geom_text_repel(aes(label = label), 
                  colour = "black", 
                  size = 3,
                  hjust = -1,
                  vjust=0.1,
                  fontface=2)+ 
  scale_size(range = c(1, 2))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 14))+
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  xlab("\nPrevalence Among Migraine")+
  ylab("Prevalence used Triptans\n")+
  ylim(0.25,0.5)+ xlim(0,1.2)


# --------------
# % Of all moderate severa migraine with CGRP or Nasal Spray -----------------------

All_pats <- fread("ModSev_Pats_V3.txt", colClasses = "character", stringsAsFactors = FALSE)
All_pats <- All_pats %>% filter(group=="ModSev") %>% select(patient)

RIMUS23_Doses_2 <- fread("RIMUS23 Doses.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Doses_2 <- RIMUS23_Doses_2 %>% select(code, drug_class, patid, from_dt, days_sup) %>% distinct()
RIMUS23_Doses_2 <- RIMUS23_Doses_2 %>% inner_join(All_pats, by=c("patid"="patient")) 
length(unique(RIMUS23_Doses_2$patid)) # 135660

RIMUS23_Doses_2 <- RIMUS23_Doses_2  %>% mutate(from_dt=as.Date(from_dt)) %>% filter(from_dt+as.numeric(days_sup)>=as.Date("2021-06-16"))

length(unique(RIMUS23_Doses_2$patid[RIMUS23_Doses_2$drug_class=="CGRP Oral"])) # 8363
length(unique(RIMUS23_Doses_2$patid[RIMUS23_Doses_2$drug_class=="CGRP Injectable"|RIMUS23_Doses_2$drug_class=="CGRP Oral"])) # 13711

RIME_MEDICATIONS <- fread("RIME Medications.txt")
RIME_MEDICATIONS <- RIME_MEDICATIONS %>% filter(med_route=="NASAL") %>% select(drug_class, med_code, drug_id, generic_name)
RIME_MEDICATIONS$med_code <- substr(RIME_MEDICATIONS$med_code, start = 3, stop = nchar(RIME_MEDICATIONS$med_code))
RIME_MEDICATIONS <- RIME_MEDICATIONS %>% select(-drug_id)

RIMUS23_Doses_2 <- RIME_MEDICATIONS %>% select(med_code) %>% inner_join(RIMUS23_Doses_2, by=c("med_code"="code"))

unique(RIMUS23_Doses_2$drug_class)
length(unique(RIMUS23_Doses_2$patid)) # Any 733 nasal spray
length(unique(RIMUS23_Doses_2$patid[RIMUS23_Doses_2$drug_class=="Triptan"])) # 507 triptan nasal spray

# -----------------------------------------------------------------

# For how many months have the been lapsed looking back ? -------

ModSev_vector <- fread("ModSev_vector.txt", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- read.table("RIMUS23 Drug Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% inner_join(ModSev_vector)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% filter(group=="ModSev") %>% select(-group)

RIMUS23_Drug_Histories %>% filter(month58!="-"&month59=="-"&month60=="-") %>% summarise(tot=sum(as.numeric(weight)))

RIMUS23_Drug_Histories <- gather(RIMUS23_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
RIMUS23_Drug_Histories$Month <- parse_number(as.character(RIMUS23_Drug_Histories$Month))
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% select(-disease)
RIMUS23_Drug_Histories <- RIMUS23_Drug_Histories %>% arrange(patient, desc(Month))
 



RIMUS23_Drug_Histories <- data.frame(RIMUS23_Drug_Histories %>% group_by(patient) %>% 
  slice(if(any(grepl("-",Treat))) 1:which.max(!grepl("-",Treat)) else row_number())   %>%
    ungroup() %>% filter(Treat=="-"))

RIMUS23_Drug_Histories %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  group_by(n) %>% summarise(tot=sum(as.numeric(weight)))

# -----------

                            
                            
