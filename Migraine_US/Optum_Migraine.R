library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
options(scipen = 999)

# Demographics EDA ---------------------------------------------------------------------------------------
RIME_Demographics <- 
  read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

# ----
# Gender and Age distribution ------------------------------------------------------
length(unique(RIME_Demographics$patid)) #314065
sum(as.numeric(RIME_Demographics$weight)) #27510192

RIME_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight) %>% 
  distinct() %>% summarise(n = sum(as.numeric(weight))) 

data.frame(RIME_Demographics %>% select(patid, weight, diagnosis) %>% group_by(diagnosis) %>% 
             summarise(n = n()) %>% arrange(-n))

data.frame(RIME_Demographics %>% select(patid, weight, diagnosis) %>% group_by(diagnosis) %>% 
             summarise(n = sum(as.numeric(weight))) %>% arrange(-n))


TIME_Demographics_Migraine_Filtered <- RIME_Demographics %>% filter(diagnosis != "-") %>% 
  select(patid, weight, age, gender) %>% distinct() %>% group_by(gender, age) %>%
  summarise(n= sum(as.numeric(weight)))

data.frame(TIME_Demographics_Migraine_Filtered)

write.csv(TIME_Demographics_Migraine_Filtered, "TIME_Demographics_Migraine_Filtered.csv")


# ----
# Elapsed time since diagnosis and since last seen ---------------------------------------------------------
library(zoo)

RIME_Demographics <- RIME_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight,migraine_earliest) %>%
  mutate(migraine_earliest = as.Date(migraine_earliest)) 

RIME_Demographics %>% 
  mutate(lag_months = (as.yearmon("2021-7-31") - as.yearmon(RIME_Demographics$migraine_earliest))*12) %>%
  mutate(lag_bins = ifelse(lag_months<=12, "< 12 months",
                           ifelse(lag_months>12&lag_months<=24, "12 to 24 months",
                                  ifelse(lag_months>24&lag_months<=36, "24 to 36 months",
                                         ifelse(lag_months>36&lag_months<=48, "36 to 48 months",
                                                ifelse(lag_months>48&lag_months<=60, "48 to 60 months","> 60 months")))))) %>%
  group_by(lag_bins) %>% summarise(pats_sum = sum(as.numeric(weight)))

RIME_Demographics <- RIME_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight,migraine_latest) %>%
  mutate(migraine_latest = as.Date(migraine_latest))

RIME_Demographics %>% 
  mutate(lag_months = (as.yearmon("2021-7-31") - as.yearmon(RIME_Demographics$migraine_latest))*12) %>%
  mutate(lag_bins = ifelse(lag_months<=12, "< 12 months",
                           ifelse(lag_months>12&lag_months<=24, "12 to 24 months",
                                  ifelse(lag_months>24&lag_months<=36, "24 to 36 months",
                                         ifelse(lag_months>36&lag_months<=48, "36 to 48 months",
                                                ifelse(lag_months>48&lag_months<=60, "48 to 60 months","> 60 months")))))) %>%
  group_by(lag_bins) %>% summarise(pats_sum = sum(as.numeric(weight)))




# ----
# Catgeories ---------------------------------------------------------------------
RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

RIME_Demographics <- RIME_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight, diagnosis)

RIME_Demographics %>% filter(grepl("Intractable",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight))) #

RIME_Demographics %>% filter(grepl("Aura",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight))) #

RIME_Demographics %>% filter(grepl("Severe",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight))) #

RIME_Demographics %>% filter(grepl("Chronic",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight))) #

RIME_Demographics %>% filter(grepl("Targeted Therapy",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight))) #

RIME_Demographics %>% filter(grepl("CGRP Oral",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight))) #

RIME_Demographics %>% filter(grepl("CGRP Injectable",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight))) #

RIME_Demographics %>% filter(grepl("CGRP Combined",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight))) #

107981.9+617393.5+129664.2










# ----
# Division --------------------------------------------
RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

RIME_Demographics %>%filter(diagnosis != "-") %>% group_by(division) %>% summarise(pop = sum(as.numeric(weight)))

RIME_Demographics %>%filter(diagnosis != "-") %>% group_by(race) %>% summarise(pop = sum(as.numeric(weight)))

RIME_Demographics %>%filter(diagnosis != "-") %>% group_by(income) %>% summarise(pop = sum(as.numeric(weight)))

RIME_Demographics %>%filter(diagnosis != "-") %>% group_by(wealth) %>% summarise(pop = sum(as.numeric(weight)))


# ----
# Dossiers EDA ---------------------------------------------------------------------------------------
RIME_Dossiers <- 
  read.table("RIME Dossiers.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

#19043067
RIME_Dossiers %>% select(patid, weight) %>% distinct() %>% 
  summarise(n = sum(as.numeric(weight))) 

18338559+3713117
RIME_Dossiers %>% select(patid, intractable, weight) %>% distinct() %>% group_by(intractable) %>% summarise(total = sum(as.numeric(weight)))





# ----
# Events EDA ---------------------------------------------------------------------------------------
RIME_Events <- read.table("RIME Events.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
#18360360
RIME_Events %>% select(patid, weight) %>% distinct() %>% summarise(n = sum(as.numeric(weight))) 

RIME_Events_number <- RIME_Events %>% select(patid, weight, code) %>% group_by(patid, weight) %>% summarise(n=n())

data.frame(RIME_Events_number %>% ungroup() %>% group_by(n) %>% summarise(count = n()) %>% filter(n <=100))


RIME_Events <- RIME_Events %>% mutate(claimed = as.Date(claimed))
RIME_Events <- RIME_Events %>% select(patid, claimed) %>% group_by(patid) 
RIME_Events <- RIME_Events %>%  group_by(patid)  %>% mutate(lags = claimed - lag(claimed))
data.frame(RIME_Events %>% mutate(lags = as.numeric(lags)) %>% filter(!is.na(lags)) %>% ungroup() %>% group_by(lags) %>%
  summarise(n=n()))

RIME_Events %>% mutate(lags = as.numeric(lags)) %>% filter(!is.na(lags)) %>% ungroup() %>% summarise(n=median(lags)) #36
RIME_Events %>% mutate(lags = as.numeric(lags)) %>% filter(!is.na(lags)) %>% ungroup() %>% summarise(n=mean(lags)) #121

length(unique(RIME_Events$patid))

RIME_Events %>% mutate(lags = as.numeric(lags)) %>% filter(!is.na(lags)) %>% ungroup() %>%
  mutate(bins = ifelse(lags < 30, "<1 Month",
                       ifelse(lags < 90, "1 - 3 Months",
                              ifelse(lags >=90 & lags <180, "3 - 6 Months",
                                     ifelse(lags >=180 & lags <360, "6 - 12 Months",
                                            ifelse(lags >=360 & lags < 720, "1 - 2 years",
                                                   ifelse(lags >=720 & lags<1080, "2 - 3 years",
                                                          ifelse(lags >=1080 & lags <1440, "3 - 4 years", ">4 years")))))))) %>%
  group_by(bins) %>% summarise(n=n()) %>%
  arrange(-n) %>%
  mutate(total = sum(n)) %>%
  mutate(percent= ((n/total)*100)) %>%
  ggplot(aes(x = reorder(bins, -percent), y = percent))+
  geom_bar(stat="identity", alpha = 0.8, show.legend = FALSE, fill="cadetblue3") +
  theme(axis.text.x = element_text(angle = 0, hjust=0.5)) +
  xlab("\nDistance Event Records") + ylab("% of patient-visits\n") +
  ggtitle("Distribution of distances between visits")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))


# The complete distribution:
RIME_Events <- read.table("RIME Events.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
RIME_Events <- RIME_Events %>% mutate(claimed = as.Date(claimed))
RIME_Events <- RIME_Events %>% select(patid, claimed)
RIME_Events <- RIME_Events %>%  group_by(patid)  %>% mutate(lags = claimed - lag(claimed))

RIME_Events <- RIME_Events %>% ungroup() %>% mutate(lags = as.numeric(lags)) %>% filter(!is.na(lags)) %>% 
  mutate(lags = ifelse(lags>366, 366, lags))



data.frame(RIME_Events %>% group_by(lags) %>% summarise(n=n()))


RIME_Events %>% group_by(lags) %>% summarise(n=n()) %>% arrange(-n) %>%
  mutate(total = sum(n)) %>%
  mutate(percent= ((n/total)*100)) %>%
  ggplot(aes(x = lags, y = percent))+
  geom_bar(stat="identity", alpha = 0.8, show.legend = FALSE, fill="cadetblue3") +
  theme(axis.text.x = element_text(angle = 0, hjust=0.5)) +
  xlab("\nDistance Event Records") + ylab("% of patient-visits\n") +
  ggtitle("Distribution of distances between visits")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))









# ----
# Drug Penetrance  Month 60 -------------------------------------------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)

# all pats
MIG_Drug_Histories %>% summarise(sum_weights = sum(as.numeric(weight))) # 19043067
# sum of weights of the treated ones at m60 9590919 
MIG_Drug_Histories %>% filter(month60 != "-") %>% summarise(sum_weights = sum(as.numeric(weight)))
# sum of weights of the non-treated ones 9452149
MIG_Drug_Histories %>% filter(month60 == "-") %>% summarise(sum_weights = sum(as.numeric(weight)))

MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T)
names(MIG_Drug_Histories)[3] <- "molecule"


plots <- data.frame(MIG_Drug_Histories %>% 
             filter(molecule != "-")%>%
             group_by(molecule) %>% 
             summarise(pats=sum(as.numeric(weight))) %>% 
             mutate(percentage = (pats/18330405)*100) %>%
             left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class)) %>% 
             group_by(drug_class)%>%
             arrange(drug_class, -percentage)) %>% group_by(drug_class)%>%
  do(plots=ggplot(data=.) + aes(x=reorder(generic_name, -percentage), y=percentage, fill=generic_name, label=round(percentage, digits = 2)) + 
       scale_fill_viridis_d() + geom_col(show.legend = F) + ylim(0,7) +  geom_text(vjust=-1)+
       ylab("% Penetrance Month 60\n") + xlab("") + ggtitle(unique(.$drug_class)) +
       theme(panel.grid.major=element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.title.x=element_blank(),
             axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)))

plots[[2]]


# ----
# Class Pentrance Month 60 --------------------------------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )
names(MIG_Drug_Histories)[3] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(molecule != "-") %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
  mutate(sum_weights_percent = (sum_weights / 18330405)*100))


# ----
# Drug Penetrance for those on Rimegrapant or Ubrogepant on m60 --------------------

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60) %>% filter(grepl("135|136",month60))
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )
names(MIG_Drug_Histories)[3] <- "molecule"
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(molecule = as.character(molecule))
MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(molecule != "-") %>% select(patient, weight, molecule, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

MIG_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #113762

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 113762)*100))



RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60) %>% filter(grepl("135",month60))
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )
names(MIG_Drug_Histories)[3] <- "molecule"
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(molecule = as.character(molecule))
MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(molecule != "-") %>% select(patient, weight, molecule, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

MIG_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #24086

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 24086)*100))



RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60) %>% filter(grepl("136",month60))
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )
names(MIG_Drug_Histories)[3] <- "molecule"
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(molecule = as.character(molecule))
MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(molecule != "-") %>% select(patient, weight, molecule, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

MIG_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #90386

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 90386)*100))

# ----
# Ever Treated pats # 18330405 ----------------------------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
# sum across rows, to see hoe many remain zero "0" 
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  # on drug 
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0  # no drug
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

MIG_Drug_Histories_LONG %>% filter(SUM != 0) %>% summarise(pats = sum(as.numeric(weight))) #18330405
198423+15
205974-7536
# ----
# Classs Penetrance across entire 60 month period --------------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
length(unique(MIG_Drug_Histories$patient)) # 205974
sum(as.numeric(MIG_Drug_Histories$weight)) # 19043067
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
  mutate(sum_weights_percent = (sum_weights / 18330405)*100)) %>% arrange(-sum_weights_percent)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(drug_class)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, grp)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient, weight) %>% summarize(across(everything(), max))
data.frame(MIG_Drug_Histories %>% ungroup() %>% group_by(grp) %>% summarise(total=sum(as.numeric(weight))))
MIG_Drug_Histories %>% ungroup() %>% summarise(weighted.mean(as.numeric(grp), as.numeric(weight)))

# ----
# How many patients have tried x # of triptans/opioids/NSAID in the last 5 years? --------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
length(unique(MIG_Drug_Histories$patient)) # 205974
sum(as.numeric(MIG_Drug_Histories$weight)) # 19043067

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_class)

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories %>% filter(drug_class=="Triptan")
MIG_Drug_Histories_NSAID <- MIG_Drug_Histories %>% filter(drug_class=="NSAID")
MIG_Drug_Histories_Opioid <- MIG_Drug_Histories %>% filter(drug_class=="Weak Opioid" | drug_class=="Strong Opioid")

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% distinct()
MIG_Drug_Histories_NSAID <- MIG_Drug_Histories_NSAID %>% distinct()
MIG_Drug_Histories_Opioid <- MIG_Drug_Histories_Opioid %>% distinct()

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})
MIG_Drug_Histories_NSAID <- MIG_Drug_Histories_NSAID %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})
MIG_Drug_Histories_Opioid <- MIG_Drug_Histories_Opioid %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% select(patient, weight, grp)
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient, weight) %>% summarize(across(everything(), max))
MIG_Drug_Histories_Triptans %>% ungroup() %>% group_by(grp) %>% summarise(total=sum(as.numeric(weight)))
MIG_Drug_Histories_Triptans %>% ungroup() %>% summarise(weighted.mean(as.numeric(grp), as.numeric(weight)))



MIG_Drug_Histories_NSAID <- MIG_Drug_Histories_NSAID %>% select(patient, weight, grp)
MIG_Drug_Histories_NSAID <- MIG_Drug_Histories_NSAID %>% group_by(patient, weight) %>% summarize(across(everything(), max))
MIG_Drug_Histories_NSAID %>% ungroup() %>% group_by(grp) %>% summarise(total=sum(as.numeric(weight)))
MIG_Drug_Histories_NSAID %>% ungroup() %>% summarise(weighted.mean(as.numeric(grp), as.numeric(weight)))


MIG_Drug_Histories_Opioid <- MIG_Drug_Histories_Opioid %>% select(patient, weight, grp)
MIG_Drug_Histories_Opioid <- MIG_Drug_Histories_Opioid %>% group_by(patient, weight) %>% summarize(across(everything(), max))
MIG_Drug_Histories_Opioid %>% ungroup() %>% group_by(grp) %>% summarise(total=sum(as.numeric(weight)))
MIG_Drug_Histories_Opioid %>% ungroup() %>% summarise(weighted.mean(as.numeric(grp), as.numeric(weight)))

# ----
# Number of lines per stock on month 60 ----------------------------------------------
MIG_Box_Stocks <- read.table("MIG Box Stocks.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Box_Stocks %>% filter(period == "60") %>% summarise(n=sum(as.numeric(pats))) #18330402

MIG_Box_Stocks %>% filter(period == "60") %>% 
  summarise(lines = weighted.mean(as.numeric(line_of_therapy), as.numeric(pats))) #8.444672

library(spatstat)
MIG_Box_Stocks %>% filter(period == "60") %>% 
  summarise(lines = weighted.median(as.numeric(line_of_therapy), as.numeric(pats))) #6.5


data.frame(MIG_Box_Stocks %>% filter(period == "60") %>% 
  group_by(line_of_therapy)%>%
  summarise(pats=(sum(as.numeric(pats)))) %>%
  mutate(line_of_therapy = as.numeric(line_of_therapy)) %>%
  ungroup() %>% mutate(percent=(pats/18330405)*100) %>%
  arrange(line_of_therapy)%>%
    mutate(global=sum(percent))) %>%
  summarise(wa=weighted.mean(line_of_therapy, pats)) #8.444672

MIG_Box_Stocks %>% filter(period == "60") %>% mutate(line_of_therapy = as.numeric(line_of_therapy)) %>% mutate(pats = as.numeric(pats)) %>% group_by(box) %>%
  summarise(wa = weighted.mean(line_of_therapy, pats))



MIG_Box_Stocks %>% filter(period == "60") %>% mutate(line_of_therapy = as.numeric(line_of_therapy)) %>% mutate(pats = as.numeric(pats)) %>% group_by(box) %>%
  summarise(wa = weighted.median(line_of_therapy, pats))


Marimekko_StocksByLineOfTherapy <-  MIG_Box_Stocks %>% filter(period == "60") %>% 
  select(box, line_of_therapy, pats) %>%
  group_by(box) %>%
  mutate(total_pats = sum(as.numeric(pats)))%>%
  mutate(line_of_therapy = as.numeric(line_of_therapy))%>%
    mutate(line_of_therapy=ifelse(line_of_therapy>5, 6, line_of_therapy))%>%
  ungroup()%>%
  mutate(line_of_therapy_percent=((as.numeric(pats)/total_pats)*100))%>%
  mutate(total_pats_percent=(total_pats/sum(unique(total_pats)))*100)
  
write.csv(Marimekko_StocksByLineOfTherapy, "Marimekko_StocksByLineOfTherapy.csv")



data.frame(MIG_Box_Stocks %>% filter(period == "60") %>% 
  select(box, line_of_therapy, pats) %>%
  mutate(line_of_therapy = as.numeric(line_of_therapy)) %>%
  mutate(line_of_therapy=ifelse(line_of_therapy>5, 6, line_of_therapy)) %>%
  group_by(line_of_therapy, box) %>%
  summarise(pats=(sum(as.numeric(pats)))) %>%
  ungroup() %>%
  group_by(line_of_therapy) %>% 
  mutate(total_pats_line = sum(as.numeric(pats)))%>%
  mutate(line_of_therapy_percent=((pats/total_pats_line))))



  #
# ----
# Number of Rx per patient (excluding G) ----------------------------------------------------------------------
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% group_by(pat_id) %>% summarise(n=n())
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(n = as.numeric(n))

data.frame(MIG_Doses_BIG %>% group_by(n) %>% summarise(n2 = n()))


# ----
# Number of gap days per patient for the same molecule ------------------------------------
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(generic_name, pat_id, from_dt)
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>% arrange(pat_id, generic_name, from_dt)
MIG_Doses_BIG <- MIG_Doses_BIG %>% group_by(pat_id, generic_name) %>% 
  mutate(from_dt = as.numeric(from_dt)) %>% mutate(diffs = from_dt-lag(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(diffs = replace_na(diffs, 0))



Gaps_between_scripts <- MIG_Doses_BIG %>% ungroup() %>% group_by(diffs) %>% summarise(n=n())
write.csv(Gaps_between_scripts, "Gaps_between_scripts.csv")

MIG_Doses_BIG %>% ungroup() %>% group_by(diffs) %>% summarise(n=n()) %>% 
  filter(diffs >0)%>%
  filter(diffs <=100) %>%
  mutate(n2=(n/13807972)*100) %>%
  ggplot(aes(diffs, n2))+
  geom_col(fill="deepskyblue4", colour="deepskyblue4")+
  theme(panel.background = element_blank()) +
  ylab("% of all scripts (exc. G)\n")+xlab("\nDistance between Prescriptions (days, 1-100)")



# ----
# Calculate number of supply days per patient per drug -------------------------------------
MIG_Doses <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Doses <- MIG_Doses %>% filter(status != "G")
MIG_Doses <- MIG_Doses %>% filter(drug_group != "Preventative")
MIG_Doses <- MIG_Doses %>% filter(drug_class != "Antipsychotic" & drug_class != "Analgesic" & drug_class != "Hospitalization" & drug_class != "Sedative" & drug_class != "CGRP Injectable")
MIG_Doses <- MIG_Doses %>% select(-c(prov_unique, prov_type, specialty, taxonomy1, taxonomy2))
MIG_Doses <- MIG_Doses %>% mutate(from_dt = as.Date(from_dt))
#unique(MIG_Doses$drug_class)
MIG_Doses <- MIG_Doses %>% select(pat_id, generic_name, drug_group, drug_class, weight, from_dt, dayssup)


MIG_Doses_m12 <- MIG_Doses %>% filter(from_dt >= "2020-09-30") %>% filter(dayssup != "")
MIG_Doses_m12 <- MIG_Doses_m12 %>% group_by(pat_id) %>% mutate(total_n_pills = sum(as.numeric(dayssup)))
MIG_Doses_m12 <- MIG_Doses_m12 %>% group_by(pat_id, drug_class) %>%  mutate(total_n_pills_class = sum(as.numeric(dayssup)))
MIG_Doses_m12 <- MIG_Doses_m12 %>% ungroup() %>% group_by(pat_id) %>% mutate(pills_per_month = total_n_pills/12)
MIG_Doses_m12 <- MIG_Doses_m12 %>% ungroup() %>% group_by(pat_id, drug_class) %>% mutate(pills_per_month_class = total_n_pills_class/12)

MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month) %>% 
  distinct() %>% mutate(frequency = ifelse(pills_per_month>=15, "Chronic",
                                           ifelse(pills_per_month>=4, "Intermediate", "Acute"))) %>%
  group_by(frequency) %>% summarise(pats = sum(as.numeric(weight)))
# 
library(spatstat)

MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month) %>% 
  distinct() %>% summarise(mean = weighted.mean(pills_per_month, as.numeric(weight))) #12.5

MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month) %>% 
  distinct() %>%
  select(pills_per_month)%>%
  ggplot(aes(pills_per_month))+
  geom_density(size=2, fill="deepskyblue4", colour="deepskyblue4", alpha=0.7)+
  xlim(0,100)+
  theme(panel.background = element_blank())+
  ylab("Patient Sample Proportion\n")+
  xlab("\nTotal Number of Supply Days per Month")


MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month_class) %>% 
  distinct() %>% group_by(pat_id) %>% filter(pills_per_month_class == max(pills_per_month_class)) %>% 
  ungroup() %>% mutate(frequency = ifelse(pills_per_month_class>=15, "Chronic",
                                           ifelse(pills_per_month_class>=4, "Episodic", "Acute"))) %>%
  group_by(frequency) %>% summarise(pats = sum(as.numeric(weight)))

length(unique(MIG_Doses_m12$pat_id))

MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, drug_class, pills_per_month_class) %>% distinct() %>%
  group_by(drug_class) %>% summarise(mean_month = weighted.mean(pills_per_month_class, as.numeric(weight)))


MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, drug_class, pills_per_month_class) %>% distinct() %>%
  group_by(drug_class) %>% summarise(mean_month = weighted.median(pills_per_month_class, as.numeric(weight)))


MIG_Doses_dayssup_m12_perMonth_perclass <- MIG_Doses_m12 %>% ungroup() %>% 
  select(pat_id, weight, drug_class, pills_per_month, pills_per_month_class) %>% distinct() %>%
  group_by(pat_id) %>% mutate(min= max(pills_per_month_class))








# MIG_Doses %>% filter(dayssup != "") %>% group_by(pat_id) %>% mutate(total_n_pills = sum(as.numeric(dayssup))) %>% ungroup() %>%
#   group_by(pat_id, drug_class) %>%  mutate(total_n_pills_class = sum(as.numeric(dayssup))) %>% ungroup() %>% 
#   group_by(pat_id) %>% mutate(pills_per_month = total_n_pills/60) %>% ungroup() %>% 
#   group_by(pat_id, drug_class) %>% mutate(pills_per_month_class = total_n_pills_class/60) %>% ungroup() %>% 
#   select(pat_id, weight, pills_per_month) %>% 
#   distinct() %>% mutate(frequency = ifelse(pills_per_month>=15, "Chronic",
#                                            ifelse(pills_per_month>=4, "Episodic", "Acute"))) %>%
#   group_by(frequency) %>% summarise(pats = sum(as.numeric(weight)))


# MIG_Doses %>% filter(dayssup != "") %>% group_by(pat_id) %>% mutate(total_n_pills = sum(as.numeric(dayssup))) %>% ungroup() %>%
#   group_by(pat_id, drug_class) %>%  mutate(total_n_pills_class = sum(as.numeric(dayssup))) %>% ungroup() %>% 
#   group_by(pat_id) %>% mutate(pills_per_month = total_n_pills/60) %>% ungroup() %>% 
#   group_by(pat_id, drug_class) %>% mutate(pills_per_month_class = total_n_pills_class/60) %>% ungroup() %>%
#   select(pat_id, weight, drug_class, pills_per_month_class) %>% distinct() %>%
#   group_by(drug_class) %>% summarise(mean_month = weighted.mean(pills_per_month_class, as.numeric(weight)))




# ----
# Share  of drug class per age bucket ------------------------------------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )
names(MIG_Drug_Histories)[3] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()


data.frame(MIG_Drug_Histories %>%
             group_by(drug_class) %>%
             summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 18330405)*100) %>%
             filter(!is.na(drug_class)))

RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Demographics <- RIME_Demographics %>% select(patid, weight, age)
names(RIME_Demographics)[1] <- "patient"
MIG_Drug_Histories<- MIG_Drug_Histories %>% left_join(RIME_Demographics, by=c("patient"="patient"))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(age = as.numeric(age))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(age_group = ifelse(age>=18 & age<30,"18_to_29", 
                                                                       ifelse(age>=30 & age<40, "30_to_39", 
                                                                              ifelse(age>=40 & age<50, "40_to_49",
                                                                                     ifelse(age>=50 & age<60, "50_to_59", 
                                                                                            ifelse(age>=60 & age<70,"60_to_69",
                                                                                                   ifelse(age>=70 & age<80, "70_to_79", "+80")))))))



data.frame(MIG_Drug_Histories %>% group_by(drug_class, age_group) %>% 
             summarise(sum_weights = sum(as.numeric(weight.x))) %>% filter(!is.na(drug_class)) %>%
             ungroup() %>% group_by(drug_class) %>% mutate(total_weight=sum(sum_weights)))


### per group rather than class
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )
names(MIG_Drug_Histories)[3] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_group))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_group)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()


data.frame(MIG_Drug_Histories %>%
             group_by(drug_group) %>%
             summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 18330405)*100) %>%
             filter(!is.na(drug_group)))
# 

RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Demographics <- RIME_Demographics %>% select(patid, weight, age)
names(RIME_Demographics)[1] <- "patient"
MIG_Drug_Histories<- MIG_Drug_Histories %>% left_join(RIME_Demographics, by=c("patient"="patient"))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(age = as.numeric(age))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(age_group = ifelse(age>=18 & age<30,"18_to_29", 
                                                                       ifelse(age>=30 & age<40, "30_to_39", 
                                                                              ifelse(age>=40 & age<50, "40_to_49",
                                                                                     ifelse(age>=50 & age<60, "50_to_59", 
                                                                                            ifelse(age>=60 & age<70,"60_to_69",
                                                                                                   ifelse(age>=70 & age<80, "70_to_79", "+80")))))))



data.frame(MIG_Drug_Histories %>% group_by(drug_group, age_group) %>% 
             summarise(sum_weights = sum(as.numeric(weight.x))) %>% filter(!is.na(drug_group)) %>%
             ungroup() %>% group_by(drug_group) %>% mutate(total_weight=sum(sum_weights)))


# ----
# Concomitant drug therapy patietns Oral CGRP and Injectable CGRP -------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )

Patients_CGRP_Oral <- MIG_Drug_Histories %>% 
  group_by(patient) %>% filter(month60 == "135" | month60=="136") %>% select(patient)
Patients_CGRP_Oral <- Patients_CGRP_Oral %>% distinct()

Patients_CGRP_Inj <- MIG_Drug_Histories %>% 
  group_by(patient) %>% filter(month60 == "137" | month60=="138" | month60 == "139" | month60=="140") %>% 
  select(patient)  %>% distinct()

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)

Patients_CGRP_Oral <- Patients_CGRP_Oral %>% left_join(MIG_Drug_Histories)
Patients_CGRP_Inj <- Patients_CGRP_Inj %>% left_join(MIG_Drug_Histories)

Patients_CGRP_Oral_combos <- Patients_CGRP_Oral %>% ungroup() %>% filter(grepl(",",month60))
Patients_CGRP_Oral_combos <- separate_rows(Patients_CGRP_Oral_combos, month60, sep = ",", convert=T )
names(Patients_CGRP_Oral_combos)[3] <- "molecule"
Patients_CGRP_Oral_combos$molecule <- as.character(Patients_CGRP_Oral_combos$molecule)
Patients_CGRP_Oral_combos <- Patients_CGRP_Oral_combos %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
Patients_CGRP_Oral_combos <- Patients_CGRP_Oral_combos %>%  select(patient, weight, drug_class)
Patients_CGRP_Oral_combos <- Patients_CGRP_Oral_combos %>% distinct()
Patients_CGRP_Oral_combos <- Patients_CGRP_Oral_combos %>% group_by(patient) %>% arrange(drug_class)
Combos_weights <- Patients_CGRP_Oral_combos %>% select(patient, weight)
Combos_weights <- Combos_weights %>% distinct()
Patients_CGRP_Oral_combos <- Patients_CGRP_Oral_combos %>% group_by(patient) %>% summarise(drug_classes = toString(drug_class))
Patients_CGRP_Oral_combos <- Patients_CGRP_Oral_combos %>% left_join(Combos_weights)
data.frame(Patients_CGRP_Oral_combos %>% group_by(drug_classes) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(-n))

Patients_CGRP_Oral_combos %>% group_by(drug_classes) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(-n) %>%
  mutate(drug_classes= as.factor(drug_classes)) %>%
  filter(n>=1000) %>%
  ggplot(aes(x=n, y=reorder(drug_classes,n))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="firebrick" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy among ORAL CGRP patients")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())








Patients_CGRP_Inj_combos <- Patients_CGRP_Inj %>% ungroup() %>% filter(grepl(",",month60))
Patients_CGRP_Inj_combos <- separate_rows(Patients_CGRP_Inj_combos, month60, sep = ",", convert=T )
names(Patients_CGRP_Inj_combos)[3] <- "molecule"
Patients_CGRP_Inj_combos$molecule <- as.character(Patients_CGRP_Inj_combos$molecule)
Patients_CGRP_Inj_combos <- Patients_CGRP_Inj_combos %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
Patients_CGRP_Inj_combos <- Patients_CGRP_Inj_combos %>%  select(patient, weight, drug_class)
Patients_CGRP_Inj_combos <- Patients_CGRP_Inj_combos %>% distinct()
Patients_CGRP_Inj_combos <- Patients_CGRP_Inj_combos %>% group_by(patient) %>% arrange(drug_class)
Combos_weights <- Patients_CGRP_Inj_combos %>% select(patient, weight)
Combos_weights <- Combos_weights %>% distinct()
Patients_CGRP_Inj_combos <- Patients_CGRP_Inj_combos %>% group_by(patient) %>% summarise(drug_classes = toString(drug_class))
Patients_CGRP_Inj_combos <- Patients_CGRP_Inj_combos %>% left_join(Combos_weights)
data.frame(Patients_CGRP_Inj_combos %>% group_by(drug_classes) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(-n))

Patients_CGRP_Inj_combos %>% group_by(drug_classes) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(-n) %>%
  mutate(drug_classes= as.factor(drug_classes)) %>%
  filter(n>=2500) %>%
  ggplot(aes(x=n, y=reorder(drug_classes,n))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="firebrick" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy among Injectable CGRP patients")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



# ----
# Total Durations on each class over 60 months------------------------------------------------------------
#CGRP Injectable
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(3:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('137',.), ~replace(., grepl('137', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('138',.), ~replace(., grepl('138', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('139',.), ~replace(., grepl('139', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('140',.), ~replace(., grepl('140', .), "CGRP Injectable"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Injectable",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

CGRP_Injectable_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(CGRP_Injectable_Periods)[3] <- "Duration"
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% left_join(MIG_Drug_Histories) 
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% mutate(weight = as.numeric(weight))
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% distinct()

library(spatstat)
weighted.mean(CGRP_Injectable_Periods$Total_Duration, CGRP_Injectable_Periods$weight)  #11.91515
weighted.median(CGRP_Injectable_Periods$Total_Duration, CGRP_Injectable_Periods$weight)  #9.5

data.frame(CGRP_Injectable_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))

CGRP_Injectable_Periods %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))


#CGRP Oral
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(3:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "CGRP Oral"))%>% 
  mutate_if(grepl('136',.), ~replace(., grepl('136', .), "CGRP Oral"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Oral",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

CGRP_Oral_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(CGRP_Oral_Periods)[3] <- "Duration"
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% left_join(MIG_Drug_Histories) 
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% mutate(weight = as.numeric(weight))
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% distinct()

library(spatstat)
weighted.mean(CGRP_Oral_Periods$Total_Duration, CGRP_Oral_Periods$weight)  #3.704939
weighted.median(CGRP_Oral_Periods$Total_Duration, CGRP_Oral_Periods$weight)  #2.5

data.frame(CGRP_Oral_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))
# 



CGRP_Oral_Periods %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))


#Ditan
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(3:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('77',.), ~replace(., grepl('77', .), "Ditan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ditan",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Ditan_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Ditan_Periods)[3] <- "Duration"
Ditan_Periods <- Ditan_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Ditan_Periods <- Ditan_Periods %>% left_join(MIG_Drug_Histories) 
Ditan_Periods <- Ditan_Periods %>% mutate(weight = as.numeric(weight))
Ditan_Periods <- Ditan_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Ditan_Periods <- Ditan_Periods %>% distinct()

library(spatstat)
weighted.mean(Ditan_Periods$Total_Duration, Ditan_Periods$weight)  #3.027713
weighted.median(Ditan_Periods$Total_Duration, Ditan_Periods$weight)  #1.5

data.frame(Ditan_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))
# 


Ditan_Periods %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))


#Triptans
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(3:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('70',.), ~replace(., grepl('70', .), "Triptan")) %>%
  mutate_if(grepl('71',.), ~replace(., grepl('71', .), "Triptan")) %>%
  mutate_if(grepl('72',.), ~replace(., grepl('72', .), "Triptan")) %>%
  mutate_if(grepl('73',.), ~replace(., grepl('73', .), "Triptan")) %>%
  mutate_if(grepl('74',.), ~replace(., grepl('74', .), "Triptan")) %>%
  mutate_if(grepl('75',.), ~replace(., grepl('75', .), "Triptan")) %>%
  mutate_if(grepl('76',.), ~replace(., grepl('76', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Triptan_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Triptan_Periods)[3] <- "Duration"
Triptan_Periods <- Triptan_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Triptan_Periods <- Triptan_Periods %>% left_join(MIG_Drug_Histories) 
Triptan_Periods <- Triptan_Periods %>% mutate(weight = as.numeric(weight))
Triptan_Periods <- Triptan_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Triptan_Periods <- Triptan_Periods %>% distinct()

library(spatstat)
weighted.mean(Triptan_Periods$Total_Duration, Triptan_Periods$weight) #10.94688
weighted.median(Triptan_Periods$Total_Duration, Triptan_Periods$weight)   #2.5

data.frame(Triptan_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))
# 
Triptan_Periods %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))




#Ergot
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(3:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('68',.), ~replace(., grepl('68', .), "Ergot")) %>%
  mutate_if(grepl('69',.), ~replace(., grepl('69', .), "Ergot"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ergot",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)


MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Ergot_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Ergot_Periods)[3] <- "Duration"
Ergot_Periods <- Ergot_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Ergot_Periods <- Ergot_Periods %>% left_join(MIG_Drug_Histories) 
Ergot_Periods <- Ergot_Periods %>% mutate(weight = as.numeric(weight))
Ergot_Periods <- Ergot_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Ergot_Periods <- Ergot_Periods %>% distinct()

library(spatstat)
weighted.mean(Ergot_Periods$Total_Duration, Ergot_Periods$weight) #3.85546
weighted.median(Ergot_Periods$Total_Duration, Ergot_Periods$weight) #1

data.frame(Ergot_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))

Ergot_Periods %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))



#Weak Opioid
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(3:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "Weak Opioid")) %>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "Weak Opioid")) %>%
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "Weak Opioid")) %>%
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "Weak Opioid"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Weak Opioid",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Weak_Opioid_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Weak_Opioid_Periods)[3] <- "Duration"
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% left_join(MIG_Drug_Histories) 
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% mutate(weight = as.numeric(weight))
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% distinct()

library(spatstat)
weighted.mean(Weak_Opioid_Periods$Total_Duration, Weak_Opioid_Periods$weight) #10.05844
weighted.median(Weak_Opioid_Periods$Total_Duration, Weak_Opioid_Periods$weight) #1.5

data.frame(Weak_Opioid_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))

Weak_Opioid_Periods %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))



#Strong Opioid
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(3:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "Strong Opioid")) %>%
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "Strong Opioid")) %>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "Strong Opioid")) %>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "Strong Opioid")) %>%
  mutate_if(grepl('38',.), ~replace(., grepl('38', .), "Strong Opioid")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('39', .), "Strong Opioid")) %>%
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "Strong Opioid")) %>%
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "Strong Opioid")) %>%
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "Strong Opioid")) %>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "Strong Opioid")) %>%
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Strong Opioid")) %>%
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Strong Opioid")) %>%
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Strong Opioid")) %>%
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Strong Opioid")) %>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Strong Opioid")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Strong Opioid"))


MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Strong Opioid",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Strong_Opioid_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Strong_Opioid_Periods)[3] <- "Duration"
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% left_join(MIG_Drug_Histories) 
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% mutate(weight = as.numeric(weight))
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% distinct()

library(spatstat)
weighted.mean(Strong_Opioid_Periods$Total_Duration, Strong_Opioid_Periods$weight)  #7.483725
weighted.median(Strong_Opioid_Periods$Total_Duration, Strong_Opioid_Periods$weight)  #1.5

data.frame(Strong_Opioid_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


Strong_Opioid_Periods %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))



#NSAID
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(3:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(3{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(3{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(5{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(5{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(6{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(6{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(7{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(7{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(17{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "NSAID"))


MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="NSAID",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

NSAID_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(NSAID_Periods)[3] <- "Duration"
NSAID_Periods <- NSAID_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
NSAID_Periods <- NSAID_Periods %>% left_join(MIG_Drug_Histories) 
NSAID_Periods <- NSAID_Periods %>% mutate(weight = as.numeric(weight))
NSAID_Periods <- NSAID_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
NSAID_Periods <- NSAID_Periods %>% distinct()

library(spatstat)
weighted.mean(NSAID_Periods$Total_Duration, NSAID_Periods$weight) #6.79841
weighted.median(NSAID_Periods$Total_Duration, NSAID_Periods$weight) #2.5  

data.frame(NSAID_Periods %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))
# 
NSAID_Periods %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))

sum(NSAID_Periods$weight)



#
#
# ----
# How many switches in the last 12 months ? ------------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(1,2,50:62)
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month48:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
# MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(Month = as.numeric(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, grp) %>% group_by(patient) %>% 
  filter(grp == max(grp)) %>% distinct() %>% mutate(switches = grp-1) %>% ungroup() %>%
  group_by(switches) %>% summarise(pats_switch = sum(as.numeric(weight)))



library(spatstat)
MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, grp) %>% group_by(patient) %>% 
  filter(grp == max(grp)) %>% distinct() %>% mutate(switches = grp-1) %>% ungroup() %>% 
  summarise(weighted.mean(switches, as.numeric(weight))) #3.02

MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, grp) %>% group_by(patient) %>% 
  filter(grp == max(grp)) %>% distinct() %>% mutate(switches = grp-1) %>% ungroup() %>% 
  summarise(weighted.median(switches, as.numeric(weight))) #1.5

#Import stock at month60
MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,63)
MIG_Box_Histories <- MIG_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))

MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, grp) %>% group_by(patient) %>%
  filter(grp == max(grp)) %>% distinct() %>% mutate(switches = grp-1) %>% ungroup() %>% 
  left_join(MIG_Box_Histories) %>% group_by(month60) %>%
  summarise(weighted.mean(switches, as.numeric(weight)))


MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, grp) %>% group_by(patient) %>%
  filter(grp == max(grp)) %>% distinct() %>% mutate(switches = grp-1) %>% ungroup() %>% 
  left_join(MIG_Box_Histories) %>% group_by(month60) %>%
  summarise(weighted.median(switches, as.numeric(weight)))


data.frame(MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, grp) %>% group_by(patient) %>% 
             filter(grp == max(grp)) %>% distinct() %>% mutate(switches = grp-1) %>% ungroup() %>% 
             left_join(MIG_Box_Histories) %>% group_by(month60, switches) %>% 
             summarise(pats_switch = sum(as.numeric(weight))))

#

#
# ----
# Create long table with stocks, flows, etc, with Pedro's code --------------------------

library(data.table)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(scales)
library(splitstackshape)
library(ggridges)

MIG_Drug_Histories     <- fread("MIG Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
MIG_Box_Histories     <- fread("MIG Box Histories.txt", integer64 = "character", stringsAsFactors = F)

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
  MIG_Box_Histories[,i+2] <- unlist(lapply(MIG_Box_Histories[,i+2],function(x) str_sub(x, 2L, 2L)))
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

# ----
# Flows to  / from Rimegepant  -------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))


MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("135",d1)) %>% 
  filter(grepl("135",d2)) %>% group_by(s1) %>% summarise(pats=sum(as.numeric(weight)))
# #Inflows

MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(grepl("135",d1)) %>% 
  filter(!grepl("135",d2)) %>%  group_by(s2) %>% summarise(pats=sum(as.numeric(weight)))


data.frame(MIG_Flows_Aux._Long %>% filter(!grepl("135",d1)) %>% 
             filter(grepl("135",d2)) %>% group_by(p1, s1) %>% summarise(pats=sum(as.numeric(weight))))

c("I", "O", "D", "d", "p", "A", "a", "x")
c("x", "a", "A", "p", "d", "D", "O", "I")

MIG_Flows_Aux._Long %>% filter(!grepl("135",d1)) %>% 
  filter(grepl("135",d2)) %>% 
  mutate(p1 = ifelse(p1==44|p1==45,44,
                     ifelse(p1==46|p1==47, 46,
                            ifelse(p1==48|p1==49,48,
                                   ifelse(p1==50|p1==51,50,
                                          ifelse(p1==52|p1==53,52,
                                                 ifelse(p1==54|p1==55,54,
                                                        ifelse(p1==56|p1==57,56,
                                                               ifelse(p1==58|p1==59,58,60)))))))))%>%
  group_by(p1, s1) %>% summarise(pats=sum(as.numeric(weight))) %>%
  ggplot(aes(x=p1, y=pats, fill=factor(s1, levels = c("I", "O", "D", "d", "p", "A", "a", "x")))) + 
  geom_area(alpha=0.6 , size=1, colour="white", position = "stack", show.legend = F)+
  scale_fill_brewer(palette = "Paired")+
  theme(panel.background = element_blank())+
  xlab("\nMonth")+ylab("Population (n)\n")



data.frame(MIG_Flows_Aux._Long %>% filter(!grepl("135",d1)) %>% 
             filter(grepl("135",d2)) %>% 
             mutate(p1 = ifelse(p1==44|p1==45,44,
                                ifelse(p1==46|p1==47, 46,
                                       ifelse(p1==48|p1==49,48,
                                              ifelse(p1==50|p1==51,50,
                                                     ifelse(p1==52|p1==53,52,
                                                            ifelse(p1==54|p1==55,54,
                                                                   ifelse(p1==56|p1==57,56,
                                                                          ifelse(p1==58|p1==59,58,60)))))))))%>%
             group_by(p1, s1) %>% summarise(pats=sum(as.numeric(weight))) %>% ungroup()%>% group_by(p1) %>% 
             mutate(n=sum(pats)) %>% ungroup() %>% mutate(percent=pats/n))

# ----
# Flows to / from Injectable CGRPs --------------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))


MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("I",s1)) %>% 
  filter(grepl("I",s2)) %>% group_by(s1) %>% summarise(pats=sum(as.numeric(weight)))
#Inflow


MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(grepl("I",s1)) %>% 
  filter(!grepl("I",s2)) %>% group_by(s2) %>% summarise(pats=sum(as.numeric(weight)))

# ----
# Flows to / from Oral CGRPs --------------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))

MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("135",d1)&!grepl("136",d1)) %>% 
  filter(grepl("135",d2)|grepl("136",d2)) %>% filter(s2=="O") %>% group_by(s1) %>% summarise(pats=sum(as.numeric(weight)))


MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("135",d1)&!grepl("136",d1)) %>% 
  filter(grepl("135",d2)|grepl("136",d2)) %>% filter(s2=="I") %>% group_by(s1) %>% summarise(pats=sum(as.numeric(weight)))



# Sankey
# Injectable CGRP [1.8] Lapsed
# Injectable CGRP [0.6] Symptomatic
# Injectable CGRP [0.3] Acute
# Injectable CGRP [2.9] Preventive
# Injectable CGRP [2.9] Preventive+Symptomatic
# Injectable CGRP [1.9] Preventive+Acute
# Injectable CGRP [58.5] CGRP Injectable

MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(grepl("135",d1)|grepl("136",d1)) %>% 
  filter(!grepl("135",d2)&!grepl("136",d2)) %>% group_by(s1, s2) %>% summarise(pats=sum(as.numeric(weight)))




# ----
# Number of lines per patient on each stock -----------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup() %>% filter(Treat != "-")
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient, weight) %>% distinct()

MIG_Drug_Histories$treat_original<- MIG_Drug_Histories$Treat
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient, weight, treat_original) %>% arrange(patient, weight, treat_original, -Treat)
MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup %>% group_by(patient, weight, treat_original) %>% mutate(treat_new = paste(Treat, collapse=",")) 
MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, treat_new) %>% distinct()

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(treat_new)$lengths %>% {rep(seq(length(.)), .)})

Number_lines_treat <- MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, grp) %>% group_by(patient) %>% 
  filter(grp == max(grp)) %>% ungroup()


# Long file with stocks
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, d2, s2)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% left_join(Number_lines_treat, by=c("patient"="patient"))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(grp = ifelse(is.na(grp), 0, grp))

library(spatstat)


MIG_Flows_Aux._Long %>% filter(grp != 0) %>% group_by(s2) %>% summarise(n=weighted.median(grp,as.numeric(weight.x)))





#ever treated 

MIG_Flows_Aux._Long %>% filter(grp != 0) %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long %>% filter(grp != 0) %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #9.021561
MIG_Flows_Aux._Long %>% filter(grp != 0) %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #5.5



MIG_Flows_Aux._Long %>% filter(grp != 0) %>% mutate(grp=ifelse(grp>=1 & grp<=3, "1-3", 
                                                               ifelse(grp>=4 & grp<=6, "4-6",
                                                                      ifelse(grp>=7&grp<=10,"7-10","10+"))))%>% 
  group_by(grp) %>% summarise(n=sum(as.numeric(weight.x)))


MIG_Flows_Aux._Long %>% filter(s2 == "A") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long %>% filter(s2 == "A") %>%  summarise(n=weighted.median(grp,as.numeric(weight.x))) #7.1521
MIG_Flows_Aux._Long %>% filter(s2 == "A") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #5.5
#on ACUTE treat


MIG_Flows_Aux._Long %>% filter(s2 == "A") %>% mutate(grp=ifelse(grp>=1 & grp<=3, "1-3", 
                                                                                                       ifelse(grp>=4 & grp<=6, "4-6",
                                                                                                              ifelse(grp>=7&grp<=10,"7-10","10+"))))%>%
  group_by(grp) %>% summarise(n=sum(as.numeric(weight.x)))

MIG_Flows_Aux._Long %>% filter(s2 == "O") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long %>% filter(s2 == "O") %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #18.62301
MIG_Flows_Aux._Long %>% filter(s2 == "O") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #15.5
# #on ORAL CGRP


MIG_Flows_Aux._Long %>% filter(s2 == "O") %>% mutate(grp=ifelse(grp>=1 & grp<=3, "1-3", 
                                                                ifelse(grp>=4 & grp<=6, "4-6",
                                                                       ifelse(grp>=7&grp<=10,"7-10","10+")))) %>% 
  group_by(grp) %>% summarise(n=sum(as.numeric(weight.x)))


MIG_Flows_Aux._Long %>% filter(s2 == "I") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #19.85705
MIG_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #16.5
#on CGRP INJECT


MIG_Flows_Aux._Long %>% filter(s2 == "I")%>% group_by(grp) %>%  mutate(grp=ifelse(grp>=1 & grp<=3, "1-3", 
                                                                                  ifelse(grp>=4 & grp<=6, "4-6",
                                                                                         ifelse(grp>=7&grp<=10,"7-10","10+")))) %>% 
  summarise(n=sum(as.numeric(weight.x)))


MIG_Flows_Aux._Long %>% filter(grepl("135",d2)) %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long %>% filter(grepl("135",d2)) %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #21.7244
MIG_Flows_Aux._Long %>% filter(grepl("135",d2)) %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #18.5
# #ON RIMEGEPANT

MIG_Flows_Aux._Long %>% filter(grepl("135",d2)) %>% mutate(grp=ifelse(grp>=1 & grp<=3, "1-3", 
                                                                      ifelse(grp>=4 & grp<=6, "4-6",
                                                                             ifelse(grp>=7&grp<=10,"7-10","10+")))) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))

# #Pedro s pipeline
# MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
#                                   colClasses = "character", stringsAsFactors = FALSE)
# 
# setDT(MIG_Flows_Aux._Long)
# 
# data1                <- MIG_Flows_Aux._Long[p1 >= 1, .(patient,weight,p1,d1)]
# names(data1)[c(3,4)] <- c("month","drugs")  
# 
# data2                <- MIG_Flows_Aux._Long[p2 == 60, .(patient,weight,p2,d2)]
# names(data2)[c(3,4)] <- c("month","drugs") 
# 
# data <- setDT(data.frame(rbind(data1,data2)))
# 
# # nr of lines & nr of months rx by patient
# data$Rx <- (data$drugs != "-")*1 
# data <- data[Rx == 1,  ':=' (nr_lines = length(unique(drugs)), months_rx = sum(Rx)), by = .(patient)]
# data <- data[Rx == 1]



# ----
# Number of Triptans or CGRPs ever tried per patient on each stock -----------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_class)

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories %>% filter(drug_class=="Triptan")
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories %>% filter(drug_class=="CGRP Oral" | drug_class=="CGRP Injectable" )

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% distinct()
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% distinct()

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% select(patient, weight, grp)
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient, weight) %>% summarize(across(everything(), max))

MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% select(patient, weight, grp)
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% group_by(patient, weight) %>% summarize(across(everything(), max))



# Long file with stocks
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, d2, s2, p1_RxExp, flow)

MIG_Flows_Aux._Long_Triptans <- MIG_Flows_Aux._Long %>% left_join(MIG_Drug_Histories_Triptans, by=c("patient"="patient"))
MIG_Flows_Aux._Long_Triptans <- MIG_Flows_Aux._Long_Triptans %>% mutate(grp = ifelse(is.na(grp), 0, grp))

MIG_Flows_Aux._Long_CGRPs <- MIG_Flows_Aux._Long %>% left_join(MIG_Drug_Histories_CGRPs, by=c("patient"="patient"))
MIG_Flows_Aux._Long_CGRPs <- MIG_Flows_Aux._Long_CGRPs %>% mutate(grp = ifelse(is.na(grp), 0, grp))

library(spatstat)

#Triptans
#ever treated 
MIG_Flows_Aux._Long_Triptans %>% filter(p1_RxExp != 0) %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_Triptans %>% filter(p1_RxExp != 0) %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #0.4915907
MIG_Flows_Aux._Long_Triptans %>% filter(p1_RxExp != 0) %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #0

MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "A") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "A") %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #1.22655
MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "A") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #0.5


MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "O") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "O") %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #1.319469
MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "O") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #0.5


MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "I") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "I") %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #1.343518
MIG_Flows_Aux._Long_Triptans %>% filter(s2 == "I") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #0.5


MIG_Flows_Aux._Long_Triptans %>% filter(grepl("135",d2)) %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_Triptans %>% filter(grepl("135",d2)) %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #1.319469
MIG_Flows_Aux._Long_Triptans %>% filter(grepl("135",d2)) %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #0.5





#CGRPs
#ever treated 
MIG_Flows_Aux._Long_CGRPs %>% filter(p1_RxExp != 0) %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_CGRPs %>% filter(p1_RxExp != 0) %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #0.0576589
MIG_Flows_Aux._Long_CGRPs %>% filter(p1_RxExp != 0) %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #0

MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "A") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "A") %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #0.03884738
MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "A") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #0


MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "O") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "O") %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #1.339196
MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "O") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #1


MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "I") %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "I") %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #1.318918
MIG_Flows_Aux._Long_CGRPs %>% filter(s2 == "I") %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #1

MIG_Flows_Aux._Long_CGRPs %>% filter(grepl("135",d2)) %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% group_by(grp) %>% 
  summarise(n=sum(as.numeric(weight.x)))
MIG_Flows_Aux._Long_CGRPs %>% filter(grepl("135",d2)) %>% summarise(n=weighted.mean(grp,as.numeric(weight.x))) #1.932127
MIG_Flows_Aux._Long_CGRPs %>% filter(grepl("135",d2)) %>% summarise(n=weighted.median(grp,as.numeric(weight.x))) #1.5
# #ON RIMEGEPANT
#
# ----
# Share of Rimegepant of flows to Oral CGRs or All CGRPs --------------------------------------- 
# Long file with stocks
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p1 >= "48") %>% select(patient, weight, d1, d2, s1, s2, flow)

MIG_Flows_Aux._Long %>% filter(flow =="1") %>% filter(s2=="O") %>% group_by(s1) %>% summarise(n=sum(as.numeric(weight)))

MIG_Flows_Aux._Long %>% filter(flow =="1") %>% filter(s2=="O") %>% filter(grepl("135",d2)) %>% filter(!grepl("135",d1)) %>% group_by(s1) %>% summarise(n=sum(as.numeric(weight)))



# For all CGRPs
MIG_Flows_Aux._Long %>% filter(flow =="1") %>% filter(s2=="O" | s2=="I") %>% group_by(s1) %>% summarise(n=sum(as.numeric(weight)))


MIG_Flows_Aux._Long %>% filter(flow =="1") %>%  filter(s2=="O" | s2=="I") %>% filter(grepl("135",d2)) %>% filter(!grepl("114",d1)) %>% group_by(s1) %>% summarise(n=sum(as.numeric(weight)))

                                           
# ----
# Persistency / visibility NSAID Opioid  -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(3{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(3{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(5{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(5{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(6{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(6{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(7{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(7{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "NSAID"))



MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="NSAID",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

NSAID_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(NSAID_Periods_MIG)[3] <- "Duration"

NSAID_Periods_MIG_VIZ <- NSAID_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(NSAID_Periods_MIG_VIZ, "NSAID_Periods_MIG_VIZ.csv")



# Persistency / visibility Weak Opioid  -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(31{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(31{1})(\\D|$)', .), "Weak Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(32{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(32{1})(\\D|$)', .), "Weak Opioid"))%>%
  mutate_if(grepl('(^|\\D)(33{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(33{1})(\\D|$)', .), "Weak Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(34{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(34{1})(\\D|$)', .), "Weak Opioid"))



MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Weak Opioid",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Weak_Opioid_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Weak_Opioid_Periods_MIG)[3] <- "Duration"

Weak_Opioid_Periods_MIG_VIZ <- Weak_Opioid_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                       select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Weak_Opioid_Periods_MIG_VIZ, "Weak_Opioid_Periods_MIG_VIZ.csv")


# Persistency / visibility Strong Opioid  -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(35{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(35{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(36{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(36{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(37{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(37{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(38{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(38{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(39{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(39{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(40{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(40{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(41{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(41{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(42{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(42{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(43{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(43{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(44{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(44{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(45{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(45{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(46{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(46{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(47{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(47{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(48{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(48{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(49{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(49{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(50{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(50{1})(\\D|$)', .), "Strong Opioid"))



MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Strong Opioid",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Strong_Opioid_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Strong_Opioid_Periods_MIG)[3] <- "Duration"

Strong_Opioid_Periods_MIG_VIZ <- Strong_Opioid_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Strong_Opioid_Periods_MIG_VIZ, "Strong_Opioid_Periods_MIG_VIZ.csv")



# Persistency / visibility Ergot -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(54{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(54{1})(\\D|$)', .), "Ergot"))%>% 
  mutate_if(grepl('(^|\\D)(55{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(55{1})(\\D|$)', .), "Ergot"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ergot",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Ergot_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Ergot_Periods_MIG)[3] <- "Duration"

Ergot_Periods_MIG_VIZ <- Ergot_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Ergot_Periods_MIG_VIZ, "Ergot_Periods_MIG_VIZ.csv")







# Persistency / visibility Triptan -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(56{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(56{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(57{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(57{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(58{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(58{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(59{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(59{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(60{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(60{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(61{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(61{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(62{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(62{1})(\\D|$)', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Triptan_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Triptan_Periods_MIG)[3] <- "Duration"

Triptan_Periods_MIG_VIZ <- Triptan_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Triptan_Periods_MIG_VIZ, "Triptan_Periods_MIG_VIZ.csv")



# Persistency / visibility Ditan -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(63{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(63{1})(\\D|$)', .), "Ditan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ditan",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Ditan_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Ditan_Periods_MIG)[3] <- "Duration"

Ditan_Periods_MIG_VIZ <- Ditan_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Ditan_Periods_MIG_VIZ, "Ditan_Periods_MIG_VIZ.csv")





# Persistency / visibility CGRP Oral -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(114{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(114{1})(\\D|$)', .), "CGRP Oral"))%>% 
  mutate_if(grepl('(^|\\D)(115{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(115{1})(\\D|$)', .), "CGRP Oral"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Oral",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

CGRP_Oral_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(CGRP_Oral_Periods_MIG)[3] <- "Duration"

CGRP_Oral_Periods_MIG_VIZ <- CGRP_Oral_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(CGRP_Oral_Periods_MIG_VIZ, "CGRP_Oral_Periods_MIG_VIZ.csv")






# Persistency / visibility CGRP Injectable -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(116{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(116{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(117{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(117{1})(\\D|$)', .), "CGRP Injectable"))%>%
  mutate_if(grepl('(^|\\D)(118{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(118{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(119{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(119{1})(\\D|$)', .), "CGRP Injectable"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Injectable",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

CGRP_Injectable_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(CGRP_Injectable_Periods_MIG)[3] <- "Duration"

CGRP_Injectable_Periods_MIG_VIZ <- CGRP_Injectable_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(CGRP_Injectable_Periods_MIG_VIZ, "CGRP_Injectable_Periods_MIG_VIZ.csv")


# ----
# Persistency with medians -----
library(spatstat)
# Persistency / visibility NSAID Opioid  -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(3{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(3{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(5{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(5{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(6{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(6{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(7{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(7{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "NSAID"))%>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "NSAID"))%>% 
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "NSAID"))



MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="NSAID",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

NSAID_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(NSAID_Periods_MIG)[3] <- "Duration"

NSAID_Periods_MIG_VIZ <- NSAID_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

NSAID_Periods_MIG <- NSAID_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                       select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) 

NSAID_Periods_MIG <- NSAID_Periods_MIG  %>% distinct() 

weighted.median(NSAID_Periods_MIG$Total_duration, NSAID_Periods_MIG$weight) #2.5

# Persistency / visibility Weak Opioid  -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(30{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(30{1})(\\D|$)', .), "Weak Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(31{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(31{1})(\\D|$)', .), "Weak Opioid"))%>%
  mutate_if(grepl('(^|\\D)(32{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(32{1})(\\D|$)', .), "Weak Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(33{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(33{1})(\\D|$)', .), "Weak Opioid"))



MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Weak Opioid",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Weak_Opioid_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Weak_Opioid_Periods_MIG)[3] <- "Duration"

Weak_Opioid_Periods_MIG_VIZ <- Weak_Opioid_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                       select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)



Weak_Opioid_Periods_MIG <- Weak_Opioid_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) 




Weak_Opioid_Periods_MIG <- Weak_Opioid_Periods_MIG  %>% distinct() 

weighted.median(Weak_Opioid_Periods_MIG$Total_duration, Weak_Opioid_Periods_MIG$weight) #1.5

# Persistency / visibility Strong Opioid  -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(34{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(34{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(35{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(35{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(36{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(36{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(37{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(37{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(38{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(38{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(39{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(39{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(40{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(40{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(41{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(41{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(42{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(42{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(43{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(43{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(44{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(44{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(45{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(45{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(46{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(46{1})(\\D|$)', .), "Strong Opioid"))%>% 
  mutate_if(grepl('(^|\\D)(47{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(47{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(48{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(48{1})(\\D|$)', .), "Strong Opioid"))%>%
  mutate_if(grepl('(^|\\D)(49{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(49{1})(\\D|$)', .), "Strong Opioid"))



MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Strong Opioid",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Strong_Opioid_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Strong_Opioid_Periods_MIG)[3] <- "Duration"

Strong_Opioid_Periods_MIG_VIZ <- Strong_Opioid_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)



Strong_Opioid_Periods_MIG <- Strong_Opioid_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                       select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration)




Strong_Opioid_Periods_MIG <- Strong_Opioid_Periods_MIG  %>% distinct() 

weighted.median(Strong_Opioid_Periods_MIG$Total_duration, Strong_Opioid_Periods_MIG$weight) #1.5

# Persistency / visibility Ergot -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(68{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(68{1})(\\D|$)', .), "Ergot"))%>% 
  mutate_if(grepl('(^|\\D)(69{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(69{1})(\\D|$)', .), "Ergot"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ergot",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Ergot_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Ergot_Periods_MIG)[3] <- "Duration"

Ergot_Periods_MIG_VIZ <- Ergot_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)




Ergot_Periods_MIG <- Ergot_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                       select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration)




Ergot_Periods_MIG <- Ergot_Periods_MIG  %>% distinct() 

weighted.median(Ergot_Periods_MIG$Total_duration, Ergot_Periods_MIG$weight) #1

# Persistency / visibility Triptan -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(70{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(70{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(71{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(71{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(72{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(72{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(73{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(73{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(74{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(74{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(75{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(75{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(76{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(76{1})(\\D|$)', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Triptan_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Triptan_Periods_MIG)[3] <- "Duration"

Triptan_Periods_MIG_VIZ <- Triptan_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)




Triptan_Periods_MIG <- Triptan_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration)


Triptan_Periods_MIG <- Triptan_Periods_MIG  %>% distinct() 

weighted.median(Triptan_Periods_MIG$Total_duration, Triptan_Periods_MIG$weight) #2.5

# Persistency / visibility Ditan -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(77{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(77{1})(\\D|$)', .), "Ditan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ditan",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

Ditan_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Ditan_Periods_MIG)[3] <- "Duration"

Ditan_Periods_MIG_VIZ <- Ditan_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)




Ditan_Periods_MIG <- Ditan_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                       select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration)



Ditan_Periods_MIG <- Ditan_Periods_MIG  %>% distinct() 

weighted.median(Ditan_Periods_MIG$Total_duration, Ditan_Periods_MIG$weight) #1.5

# Persistency / visibility CGRP Oral -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(135{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(135{1})(\\D|$)', .), "CGRP Oral"))%>% 
  mutate_if(grepl('(^|\\D)(136{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(136{1})(\\D|$)', .), "CGRP Oral"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Oral",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

CGRP_Oral_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(CGRP_Oral_Periods_MIG)[3] <- "Duration"

CGRP_Oral_Periods_MIG_VIZ <- CGRP_Oral_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)




CGRP_Oral_Periods_MIG <- CGRP_Oral_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration)


CGRP_Oral_Periods_MIG <- CGRP_Oral_Periods_MIG  %>% distinct() 

weighted.mean(CGRP_Oral_Periods_MIG$Total_duration, CGRP_Oral_Periods_MIG$weight) #3.704939




# Persistency / visibility CGRP Injectable -------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(137{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(137{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(138{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(138{1})(\\D|$)', .), "CGRP Injectable"))%>%
  mutate_if(grepl('(^|\\D)(139{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(139{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(140{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(140{1})(\\D|$)', .), "CGRP Injectable"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Injectable",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

CGRP_Injectable_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(CGRP_Injectable_Periods_MIG)[3] <- "Duration"

CGRP_Injectable_Periods_MIG_VIZ <- CGRP_Injectable_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)



CGRP_Injectable_Periods_MIG <- CGRP_Injectable_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration)


CGRP_Injectable_Periods_MIG <- CGRP_Injectable_Periods_MIG  %>% distinct() 

weighted.median(CGRP_Injectable_Periods_MIG$Total_duration, CGRP_Injectable_Periods_MIG$weight) #2.5



# ----
# Choord chart -----
library(circlize)

CHoord_Chart_MIG_US <- read.csv("CHoord_Chart_MIG_US.csv")
names(CHoord_Chart_MIG_US)[1] <- "Lapsed"

CHoord_Chart_MIG_US <- CHoord_Chart_MIG_US[,2:9]

colnames(CHoord_Chart_MIG_US) <- c("Lapsed", "CGRP I", "Sympt", "Acute", "Prev", "Prev + Sympt", "Prev + Acute", "CGRP O")
rownames(CHoord_Chart_MIG_US) <- colnames(CHoord_Chart_MIG_US)

CHoord_Chart_MIG_US <- as.matrix(CHoord_Chart_MIG_US)

circos.clear()

cols <- hcl.colors(8, "ag_Sunset")

chordDiagram(CHoord_Chart_MIG_US, 
             grid.col = cols, 
             directional = 1, 
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.04, 
             annotationTrackHeight = c(0.1, 0.1),
             link.arr.type = "big.arrow", 
             link.sort = TRUE, 
             link.largest.ontop = TRUE,
             annotationTrack = c("grid","name"),  
             transparency = 0.3)

# ----
# Duration vs Number of Lines -------------------------------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(1,2,51:62)
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(ONvsOFF = ifelse(Treat == "-", 0, 1))
MIG_Drug_Histories <- MIG_Drug_Histories%>%group_by(patient)%>%arrange(patient)
MIG_Drug_Histories <- MIG_Drug_Histories%>%group_by(patient)%>%mutate(Duration = sum(ONvsOFF))%>%ungroup()%>%select(-c(ONvsOFF))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% distinct()
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(lines = n())
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Treat))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(lines = ifelse(lines == 3 |lines == 4, 3,
                                                                   ifelse(lines >= 5, 5, lines)))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(Duration = ifelse(Duration >= 1 & Duration <= 3, 1,
                                                                      ifelse(Duration >= 4 & Duration <= 8, 4,
                                                                             ifelse(Duration >= 9 & Duration <= 11, 9, 12))))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(weight =as.numeric(weight))
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

MIG_Drug_Histories %>% ungroup() %>% group_by(Duration, lines) %>% summarise(n= sum(weight)) %>% ungroup() %>%
  mutate(total=sum(n)) %>%
  mutate(percent = (n/sum(n)*100)) %>%
  mutate(Duration = as.character(Duration))%>%
  mutate(Duration = ifelse(Duration == "1", "1 to 3 months",
                           ifelse(Duration == "4", "4 to 8 months",
                                  ifelse(Duration == "9", "9 to 11 months", "Full 12 months"))))%>%
  mutate(lines = as.character(lines))%>%
  mutate(lines = ifelse(lines == "3", "3 to 4 lines",
                        ifelse(lines == "5", "5+ lines", 
                               ifelse(lines == "1", "1 line",
                                      ifelse(lines =="2", "2 lines",lines)))))%>%
  ggplot(aes(x = as.factor(Duration), 
             y = as.factor(lines),
             colour = as.factor(Duration),
             size = percent)) +
  geom_point() +
  geom_text(aes(label = paste(round(percent, digits = 0),"%")), 
            colour = "white", 
            size = 3) +
  scale_x_discrete(position = "top") + scale_y_discrete(limits=rev)+
  scale_size_continuous(range = c(10, 30)) + scale_color_brewer(palette = "RdGy") +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())


# ----
# Number of drugs per stock m60 ---------------------------------------------------
#Import stock at month60
MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,63)
MIG_Box_Histories <- MIG_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))
names(MIG_Box_Histories)[3] <- "Box_m60"
#drugs m60
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease)) %>% select(1,2,62)

MIG_Box_Histories <- MIG_Box_Histories %>% left_join(MIG_Drug_Histories)

MIG_Box_Histories <- MIG_Box_Histories %>% filter(month60 != "-")
MIG_Box_Histories <- separate_rows(MIG_Box_Histories, month60, sep = ",", convert=T)
MIG_Box_Histories <- MIG_Box_Histories %>% group_by(patient) %>% mutate(drugs_n = n())
MIG_Box_Histories <- MIG_Box_Histories %>% select(-c(month60))
MIG_Box_Histories <- MIG_Box_Histories %>% distinct()
MIG_Box_Histories <- MIG_Box_Histories %>% mutate(drugs_n = ifelse(drugs_n>=6,6, drugs_n))

MIG_Box_Histories %>% ungroup() %>% group_by(Box_m60) %>% summarise(n= weighted.mean(drugs_n, as.numeric(weight)))


data.frame(MIG_Box_Histories %>% ungroup() %>% group_by(Box_m60, drugs_n) %>% summarise(n=sum(as.numeric(weight))))

# ----
# Switch matrix -----------------------------------------------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% filter(p1 >=48)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(flow == "1")

#sum(as.numeric(MIG_Flows_Aux._Long$weight))

data.frame(MIG_Flows_Aux._Long %>% group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))))
57416959


MIG_Flows_Aux._Long %>% group_by(s1, s2) %>% summarise(n=sum(as.numeric(weight))) %>%
  mutate(s1 = factor(s1, levels = c("x", "a", "A", "p", "d", "D", "O", "I")))%>% 
  mutate(s2 = factor(s2, levels = c("x", "a", "A", "p", "d", "D", "O", "I"))) %>%
  mutate(n=n/1000)%>%
  ggplot(aes(x = as.factor(s2), 
             y = as.factor(s1),
             colour=as.factor(s2))) +
  geom_point(aes(size = n), alpha=0.5) +
  geom_text(aes(label = round(n, digits = 0),
                colour="bisque4",
                size=0.01)) +
  scale_x_discrete(position = "top") + scale_y_discrete(limits=rev)+ scale_colour_viridis_d(option = "D") +
  scale_size_continuous(range = c(10, 30)) + 
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())

# ----
# Concomitant classes at m60 -------------------------------------------------------------------
# Ever treated
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  # on drug 
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0  # no drug
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
MIG_Drug_Histories_LONG %>% filter(SUM != 0) %>% summarise(pats = sum(as.numeric(weight))) #18330405
Ever_treated <- MIG_Drug_Histories_LONG %>% filter(SUM != 0)
Ever_treated <- Ever_treated %>% select(patient, weight)

#Import stock at month60
MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,63)
MIG_Box_Histories <- MIG_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))
names(MIG_Box_Histories)[3] <- "Box_m60"
#drugs m60
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease)) %>% select(1,2,62)
MIG_Box_Histories <- MIG_Box_Histories %>% left_join(MIG_Drug_Histories)

Ever_treated <- Ever_treated %>% left_join(MIG_Box_Histories)

Ever_treated %>% group_by(Box_m60) %>% summarise(n=sum(as.numeric(weight))) %>% mutate(percent = n/18330405)



Ever_treated <- Ever_treated %>% mutate(combo = ifelse(grepl(",",month60), "Combo", "Mono"))

Ever_treated %>% group_by(Box_m60, combo) %>% summarise(n=sum(as.numeric(weight)))

Ever_treated <- separate_rows(Ever_treated, month60, sep = ",", convert=T)
names(Ever_treated)[4] <- "molecule"

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

Ever_treated %>% left_join(RIME_Ingredients %>% select(molecule, drug_group)) %>%
  select(patient, weight, Box_m60, drug_group) %>% distinct() %>%
  group_by(Box_m60, drug_group) %>% summarise(n=sum(as.numeric(weight)))

.
# ----
# For lapsed on m60, how long have they been lapsed? -------------------------------------------------------------------

# 1. Ever treated
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  # on drug 
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0  # no drug
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
MIG_Drug_Histories_LONG %>% filter(SUM != 0) %>% summarise(pats = sum(as.numeric(weight))) #18330405
Ever_treated <- MIG_Drug_Histories_LONG %>% filter(SUM != 0)
Ever_treated <- Ever_treated %>% select(patient, weight)

# 2. Import stock at month60
MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,63)
MIG_Box_Histories <- MIG_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))
names(MIG_Box_Histories)[3] <- "Box_m60"

# 3. drugs m60
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease)) 
MIG_Box_Histories <- MIG_Box_Histories %>% left_join(MIG_Drug_Histories)

Ever_treated <- Ever_treated %>% left_join(MIG_Box_Histories)
Ever_treated <- Ever_treated %>% filter(Box_m60 == "x")
Ever_treated <- gather(Ever_treated, Month, Treat, month1:month60, factor_key=TRUE)
Ever_treated <- Ever_treated %>% mutate(Treat = ifelse(Treat=="-", 0, 1))

Ever_treated$Month <- str_replace(Ever_treated$Month, "month1", "1")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month2", "2")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month3", "3")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month4", "4")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month5", "5")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month6", "6")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month7", "7")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month8", "8")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month9", "9")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month10", "10")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month11", "11")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month12", "12")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month13", "13")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month14", "14")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month15", "15")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month16", "16")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month17", "17")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month18", "18")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month19", "19")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month20", "20")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month21", "21")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month22", "22")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month23", "23")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month24", "24")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month25", "25")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month26", "26")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month27", "27")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month28", "28")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month29", "29")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month30", "30")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month31", "31")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month32", "32")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month33", "33")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month34", "34")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month35", "35")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month36", "36")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month37", "37")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month38", "38")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month39", "39")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month40", "40")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month41", "41")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month42", "42")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month43", "43")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month44", "44")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month45", "45")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month46", "46")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month47", "47")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month48", "48")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month49", "49")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month50", "50")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month51", "51")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month52", "52")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month53", "53")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month54", "54")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month55", "55")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month56", "56")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month57", "57")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month58", "58")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month59", "59")
Ever_treated$Month <- str_replace(Ever_treated$Month, "month60", "60")

Ever_treated$Month <- as.numeric(Ever_treated$Month)
Ever_treated <- Ever_treated %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})
Ever_treated <- Ever_treated %>% group_by(patient) %>% arrange(patient)
Ever_treated <- Ever_treated %>% group_by(patient) %>%  filter(grp == max(grp))


Ever_treated %>% group_by(patient, weight) %>% count() %>% ungroup() %>%
  mutate(Total_lapsed_bucket = ifelse(n == 1, "1", 
                                      ifelse(n >1 & n < 6, "2 to 6",
                                             ifelse(n>=6 & n <12, "6 to 12",
                                                    ifelse(n>=12&n<24, "12 to 24",
                                                           ifelse(n>=24&n<36, "24 to 36",
                                                                  ifelse(n>=36&n<48, "36 to 48",
                                                                         ifelse(n>=48&n<60, "48 to 60", "60")))))))) %>%
  group_by(Total_lapsed_bucket) %>%
  summarise(pats = sum(as.numeric(weight)))



Ever_treated_summary <- Ever_treated %>% group_by(patient, weight) %>% count() %>% ungroup()

library(spatstat)
weighted.mean(Ever_treated_summary$n, as.numeric(Ever_treated_summary$weight))  #14.74632
weighted.median(Ever_treated_summary$n, as.numeric(Ever_treated_summary$weight))  #8.5

# ----
# Provider sources -------------------
library(readxl)
MIG_Doses <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
Provider_codes <- read_excel("Provider_codes.xlsx")
names(Provider_codes)[1] <- "specialty"

MIG_Doses_short <- MIG_Doses %>% select(generic_name, drug_id, pat_id, weight, specialty)

data.frame(MIG_Doses_short %>% left_join(Provider_codes) %>% filter(!is.na(DESCRIPTION)) %>%
             group_by(DESCRIPTION) %>% summarise(n=n()) %>% arrange(-n))

# ----
# Drug penetrance over time --------------------------
# repeat each month

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)
MIG_Drug_Histories <-  separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )
names(MIG_Drug_Histories)[3] <- "molecule"
MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class) %>% filter(!is.na(drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()
data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))))


# ----
# Classs Penetrance vs Duration across last 12 month period --------------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
length(unique(MIG_Drug_Histories$patient)) # 205974
sum(as.numeric(MIG_Drug_Histories$weight)) # 19043067
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(1,2,51:62)
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 18330405)*100)) %>% arrange(-sum_weights_percent)



# Durations 12 months
#CGRP Injectable
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('137',.), ~replace(., grepl('137', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('138',.), ~replace(., grepl('138', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('139',.), ~replace(., grepl('139', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('140',.), ~replace(., grepl('140', .), "CGRP Injectable"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Injectable",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

CGRP_Injectable_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(CGRP_Injectable_Periods)[3] <- "Duration"
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% left_join(MIG_Drug_Histories) 
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% mutate(weight = as.numeric(weight))
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% distinct()

library(spatstat)
weighted.mean(CGRP_Injectable_Periods$Total_Duration, CGRP_Injectable_Periods$weight)  #7.773353
weighted.median(CGRP_Injectable_Periods$Total_Duration, CGRP_Injectable_Periods$weight)  #8.5




#CGRP Oral
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "CGRP Oral"))%>% 
  mutate_if(grepl('136',.), ~replace(., grepl('136', .), "CGRP Oral"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Oral",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

CGRP_Oral_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(CGRP_Oral_Periods)[3] <- "Duration"
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% left_join(MIG_Drug_Histories) 
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% mutate(weight = as.numeric(weight))
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% distinct()

library(spatstat)
weighted.mean(CGRP_Oral_Periods$Total_Duration, CGRP_Oral_Periods$weight)  #3.594764
weighted.median(CGRP_Oral_Periods$Total_Duration, CGRP_Oral_Periods$weight)  #1.5




#Ditan
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('77',.), ~replace(., grepl('77', .), "Ditan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ditan",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Ditan_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Ditan_Periods)[3] <- "Duration"
Ditan_Periods <- Ditan_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Ditan_Periods <- Ditan_Periods %>% left_join(MIG_Drug_Histories) 
Ditan_Periods <- Ditan_Periods %>% mutate(weight = as.numeric(weight))
Ditan_Periods <- Ditan_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Ditan_Periods <- Ditan_Periods %>% distinct()

library(spatstat)
weighted.mean(Ditan_Periods$Total_Duration, Ditan_Periods$weight)  #2.930845
weighted.median(Ditan_Periods$Total_Duration, Ditan_Periods$weight)  #1.5



#Triptans
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('70',.), ~replace(., grepl('70', .), "Triptan")) %>%
  mutate_if(grepl('71',.), ~replace(., grepl('71', .), "Triptan")) %>%
  mutate_if(grepl('72',.), ~replace(., grepl('72', .), "Triptan")) %>%
  mutate_if(grepl('73',.), ~replace(., grepl('73', .), "Triptan")) %>%
  mutate_if(grepl('74',.), ~replace(., grepl('74', .), "Triptan")) %>%
  mutate_if(grepl('75',.), ~replace(., grepl('75', .), "Triptan")) %>%
  mutate_if(grepl('76',.), ~replace(., grepl('76', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Triptan_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Triptan_Periods)[3] <- "Duration"
Triptan_Periods <- Triptan_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Triptan_Periods <- Triptan_Periods %>% left_join(MIG_Drug_Histories) 
Triptan_Periods <- Triptan_Periods %>% mutate(weight = as.numeric(weight))
Triptan_Periods <- Triptan_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Triptan_Periods <- Triptan_Periods %>% distinct()

library(spatstat)
weighted.mean(Triptan_Periods$Total_Duration, Triptan_Periods$weight) #4.473923
weighted.median(Triptan_Periods$Total_Duration, Triptan_Periods$weight)   #2.5



#Ergot
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('68',.), ~replace(., grepl('68', .), "Ergot")) %>%
  mutate_if(grepl('69',.), ~replace(., grepl('69', .), "Ergot"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ergot",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)


MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Ergot_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Ergot_Periods)[3] <- "Duration"
Ergot_Periods <- Ergot_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Ergot_Periods <- Ergot_Periods %>% left_join(MIG_Drug_Histories) 
Ergot_Periods <- Ergot_Periods %>% mutate(weight = as.numeric(weight))
Ergot_Periods <- Ergot_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Ergot_Periods <- Ergot_Periods %>% distinct()

library(spatstat)
weighted.mean(Ergot_Periods$Total_Duration, Ergot_Periods$weight) #2.629074
weighted.median(Ergot_Periods$Total_Duration, Ergot_Periods$weight) #1




#Weak Opioid
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "Weak Opioid")) %>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "Weak Opioid")) %>%
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "Weak Opioid")) %>%
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "Weak Opioid"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Weak Opioid",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Weak_Opioid_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Weak_Opioid_Periods)[3] <- "Duration"
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% left_join(MIG_Drug_Histories) 
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% mutate(weight = as.numeric(weight))
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% distinct()

library(spatstat)
weighted.mean(Weak_Opioid_Periods$Total_Duration, Weak_Opioid_Periods$weight) #4.757771
weighted.median(Weak_Opioid_Periods$Total_Duration, Weak_Opioid_Periods$weight) #1.5



#Strong Opioid
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "Strong Opioid")) %>%
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "Strong Opioid")) %>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "Strong Opioid")) %>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "Strong Opioid")) %>%
  mutate_if(grepl('38',.), ~replace(., grepl('38', .), "Strong Opioid")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('39', .), "Strong Opioid")) %>%
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "Strong Opioid")) %>%
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "Strong Opioid")) %>%
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "Strong Opioid")) %>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "Strong Opioid")) %>%
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Strong Opioid")) %>%
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Strong Opioid")) %>%
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Strong Opioid")) %>%
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Strong Opioid")) %>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Strong Opioid")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Strong Opioid"))


MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Strong Opioid",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Strong_Opioid_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Strong_Opioid_Periods)[3] <- "Duration"
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% left_join(MIG_Drug_Histories) 
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% mutate(weight = as.numeric(weight))
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% distinct()

library(spatstat)
weighted.mean(Strong_Opioid_Periods$Total_Duration, Strong_Opioid_Periods$weight)  #4.175102
weighted.median(Strong_Opioid_Periods$Total_Duration, Strong_Opioid_Periods$weight)  #1.5




#NSAID
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(3{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(3{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(5{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(5{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(6{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(6{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(7{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(7{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(17{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "NSAID"))


MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="NSAID",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

NSAID_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(NSAID_Periods)[3] <- "Duration"
NSAID_Periods <- NSAID_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
NSAID_Periods <- NSAID_Periods %>% left_join(MIG_Drug_Histories) 
NSAID_Periods <- NSAID_Periods %>% mutate(weight = as.numeric(weight))
NSAID_Periods <- NSAID_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
NSAID_Periods <- NSAID_Periods %>% distinct()

library(spatstat)
weighted.mean(NSAID_Periods$Total_Duration, NSAID_Periods$weight) #3.105789
weighted.median(NSAID_Periods$Total_Duration, NSAID_Periods$weight) #2.5  




#Steroid
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('58',.), ~replace(., grepl('58', .), "Steroid")) %>%
  mutate_if(grepl('59',.), ~replace(., grepl('59', .), "Steroid")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Steroid",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Steroid_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Steroid_Periods)[3] <- "Duration"
Steroid_Periods <- Steroid_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Steroid_Periods <- Steroid_Periods %>% left_join(MIG_Drug_Histories) 
Steroid_Periods <- Steroid_Periods %>% mutate(weight = as.numeric(weight))
Steroid_Periods <- Steroid_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Steroid_Periods <- Steroid_Periods %>% distinct()

library(spatstat)
weighted.mean(Steroid_Periods$Total_Duration, Steroid_Periods$weight)  #1.957854
weighted.median(Steroid_Periods$Total_Duration, Steroid_Periods$weight)  #1




#Neural
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('132',.), ~replace(., grepl('132', .), "Neural")) %>%
  mutate_if(grepl('133',.), ~replace(., grepl('133', .), "Neural"))  %>%
  mutate_if(grepl('134',.), ~replace(., grepl('134', .), "Neural")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Neural",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Neural_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Neural_Periods)[3] <- "Duration"
Neural_Periods <- Neural_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Neural_Periods <- Neural_Periods %>% left_join(MIG_Drug_Histories) 
Neural_Periods <- Neural_Periods %>% mutate(weight = as.numeric(weight))
Neural_Periods <- Neural_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Neural_Periods <- Neural_Periods %>% distinct()

library(spatstat)
weighted.mean(Neural_Periods$Total_Duration, Neural_Periods$weight)  #7.278547
weighted.median(Neural_Periods$Total_Duration, Neural_Periods$weight)  #7.5






#Hospitalization
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('67',.), ~replace(., grepl('67', .), "Hospitalization"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Hospitalization",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Hospitalization_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Hospitalization_Periods)[3] <- "Duration"
Hospitalization_Periods <- Hospitalization_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Hospitalization_Periods <- Hospitalization_Periods %>% left_join(MIG_Drug_Histories) 
Hospitalization_Periods <- Hospitalization_Periods %>% mutate(weight = as.numeric(weight))
Hospitalization_Periods <- Hospitalization_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Hospitalization_Periods <- Hospitalization_Periods %>% distinct()

library(spatstat)
weighted.mean(Hospitalization_Periods$Total_Duration, Hospitalization_Periods$weight)  #1.102518
weighted.median(Hospitalization_Periods$Total_Duration, Hospitalization_Periods$weight)  #1







#Antiemetic
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Antiemetic")) %>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Antiemetic")) %>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Antiemetic")) %>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Antiemetic")) %>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Antiemetic")) %>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Antiemetic")) %>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Antiemetic")) %>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Antiemetic")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antiemetic",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Antiemetic_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Antiemetic_Periods)[3] <- "Duration"
Antiemetic_Periods <- Antiemetic_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Antiemetic_Periods <- Antiemetic_Periods %>% left_join(MIG_Drug_Histories) 
Antiemetic_Periods <- Antiemetic_Periods %>% mutate(weight = as.numeric(weight))
Antiemetic_Periods <- Antiemetic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Antiemetic_Periods <- Antiemetic_Periods %>% distinct()

library(spatstat)
weighted.mean(Antiemetic_Periods$Total_Duration, Antiemetic_Periods$weight)  #2.609105
weighted.median(Antiemetic_Periods$Total_Duration, Antiemetic_Periods$weight)  #1





#SSRI
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('122',.), ~replace(., grepl('122', .), "SSRI")) %>%
  mutate_if(grepl('123',.), ~replace(., grepl('123', .), "SSRI")) %>%
  mutate_if(grepl('124',.), ~replace(., grepl('124', .), "SSRI")) %>%
  mutate_if(grepl('125',.), ~replace(., grepl('125', .), "SSRI")) %>%
  mutate_if(grepl('126',.), ~replace(., grepl('126', .), "SSRI"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SSRI",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

SSRI_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(SSRI_Periods)[3] <- "Duration"
SSRI_Periods <- SSRI_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
SSRI_Periods <- SSRI_Periods %>% left_join(MIG_Drug_Histories) 
SSRI_Periods <- SSRI_Periods %>% mutate(weight = as.numeric(weight))
SSRI_Periods <- SSRI_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
SSRI_Periods <- SSRI_Periods %>% distinct()

library(spatstat)
weighted.mean(SSRI_Periods$Total_Duration, SSRI_Periods$weight)  #8.307375
weighted.median(SSRI_Periods$Total_Duration, SSRI_Periods$weight)  #9.5






#SNRI
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('127',.), ~replace(., grepl('127', .), "SNRI")) %>%
  mutate_if(grepl('128',.), ~replace(., grepl('128', .), "SNRI")) %>%
  mutate_if(grepl('129',.), ~replace(., grepl('129', .), "SNRI")) %>%
  mutate_if(grepl('130',.), ~replace(., grepl('130', .), "SNRI")) %>%
  mutate_if(grepl('131',.), ~replace(., grepl('131', .), "SNRI"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SNRI",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

SNRI_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(SNRI_Periods)[3] <- "Duration"
SNRI_Periods <- SNRI_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
SNRI_Periods <- SNRI_Periods %>% left_join(MIG_Drug_Histories) 
SNRI_Periods <- SNRI_Periods %>% mutate(weight = as.numeric(weight))
SNRI_Periods <- SNRI_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
SNRI_Periods <- SNRI_Periods %>% distinct()

library(spatstat)
weighted.mean(SNRI_Periods$Total_Duration, SNRI_Periods$weight)  #8.359759
weighted.median(SNRI_Periods$Total_Duration, SNRI_Periods$weight)  #9.5






#Muscle Relax
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('78',.), ~replace(., grepl('78', .), "Muscle")) %>%
  mutate_if(grepl('79',.), ~replace(., grepl('79', .), "Muscle")) %>%
  mutate_if(grepl('80',.), ~replace(., grepl('80', .), "Muscle")) %>%
  mutate_if(grepl('81',.), ~replace(., grepl('81', .), "Muscle")) %>%
  mutate_if(grepl('82',.), ~replace(., grepl('82', .), "Muscle"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Muscle",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Muscle_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Muscle_Periods)[3] <- "Duration"
Muscle_Periods <- Muscle_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Muscle_Periods <- Muscle_Periods %>% left_join(MIG_Drug_Histories) 
Muscle_Periods <- Muscle_Periods %>% mutate(weight = as.numeric(weight))
Muscle_Periods <- Muscle_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Muscle_Periods <- Muscle_Periods %>% distinct()

library(spatstat)
weighted.mean(Muscle_Periods$Total_Duration, Muscle_Periods$weight)  #4.565242
weighted.median(Muscle_Periods$Total_Duration, Muscle_Periods$weight)  #1.5









#BetaBlocker 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('100',.), ~replace(., grepl('100', .), "BetaBlocker")) %>%
  mutate_if(grepl('101',.), ~replace(., grepl('101', .), "BetaBlocker")) %>%
  mutate_if(grepl('102',.), ~replace(., grepl('102', .), "BetaBlocker")) %>%
  mutate_if(grepl('103',.), ~replace(., grepl('103', .), "BetaBlocker")) %>%
  mutate_if(grepl('104',.), ~replace(., grepl('104', .), "BetaBlocker")) %>%
  mutate_if(grepl('105',.), ~replace(., grepl('105', .), "BetaBlocker")) %>%
  mutate_if(grepl('106',.), ~replace(., grepl('106', .), "BetaBlocker"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="BetaBlocker",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

BetaBlocker_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(BetaBlocker_Periods)[3] <- "Duration"
BetaBlocker_Periods <- BetaBlocker_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
BetaBlocker_Periods <- BetaBlocker_Periods %>% left_join(MIG_Drug_Histories) 
BetaBlocker_Periods <- BetaBlocker_Periods %>% mutate(weight = as.numeric(weight))
BetaBlocker_Periods <- BetaBlocker_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
BetaBlocker_Periods <- BetaBlocker_Periods %>% distinct()

library(spatstat)
weighted.mean(BetaBlocker_Periods$Total_Duration, BetaBlocker_Periods$weight)  #8.451911
weighted.median(BetaBlocker_Periods$Total_Duration, BetaBlocker_Periods$weight)  #9.5








#Cardiovascular 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('95',.), ~replace(., grepl('95', .), "Cardiovascular")) %>%
  mutate_if(grepl('96',.), ~replace(., grepl('96', .), "Cardiovascular")) %>%
  mutate_if(grepl('97',.), ~replace(., grepl('97', .), "Cardiovascular")) %>%
  mutate_if(grepl('98',.), ~replace(., grepl('98', .), "Cardiovascular")) %>%
  mutate_if(grepl('99',.), ~replace(., grepl('99', .), "Cardiovascular"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Cardiovascular",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Cardiovascular_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Cardiovascular_Periods)[3] <- "Duration"
Cardiovascular_Periods <- Cardiovascular_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Cardiovascular_Periods <- Cardiovascular_Periods %>% left_join(MIG_Drug_Histories) 
Cardiovascular_Periods <- Cardiovascular_Periods %>% mutate(weight = as.numeric(weight))
Cardiovascular_Periods <- Cardiovascular_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Cardiovascular_Periods <- Cardiovascular_Periods %>% distinct()

library(spatstat)
weighted.mean(Cardiovascular_Periods$Total_Duration, Cardiovascular_Periods$weight)  #8.980977
weighted.median(Cardiovascular_Periods$Total_Duration, Cardiovascular_Periods$weight)  #11.5






#Sedative 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('60',.), ~replace(., grepl('60', .), "Sedative")) %>%
  mutate_if(grepl('61',.), ~replace(., grepl('61', .), "Sedative")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Sedative",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Sedative_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Sedative_Periods)[3] <- "Duration"
Sedative_Periods <- Sedative_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Sedative_Periods <- Sedative_Periods %>% left_join(MIG_Drug_Histories) 
Sedative_Periods <- Sedative_Periods %>% mutate(weight = as.numeric(weight))
Sedative_Periods <- Sedative_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Sedative_Periods <- Sedative_Periods %>% distinct()

library(spatstat)
weighted.mean(Sedative_Periods$Total_Duration, Sedative_Periods$weight)  #3.505118
weighted.median(Sedative_Periods$Total_Duration, Sedative_Periods$weight)  #1







#Tricyclic 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('112',.), ~replace(., grepl('112', .), "Tricyclic")) %>%
  mutate_if(grepl('113',.), ~replace(., grepl('113', .), "Tricyclic"))%>%
  mutate_if(grepl('113',.), ~replace(., grepl('114', .), "Tricyclic")) %>%
  mutate_if(grepl('115',.), ~replace(., grepl('115', .), "Tricyclic"))%>%
  mutate_if(grepl('116',.), ~replace(., grepl('116', .), "Tricyclic")) %>%
  mutate_if(grepl('117',.), ~replace(., grepl('117', .), "Tricyclic"))%>%
  mutate_if(grepl('118',.), ~replace(., grepl('118', .), "Tricyclic")) %>%
  mutate_if(grepl('119',.), ~replace(., grepl('119', .), "Tricyclic"))%>%
  mutate_if(grepl('120',.), ~replace(., grepl('120', .), "Tricyclic")) %>%
  mutate_if(grepl('121',.), ~replace(., grepl('121', .), "Tricyclic"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Tricyclic",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Tricyclic_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Tricyclic_Periods)[3] <- "Duration"
Tricyclic_Periods <- Tricyclic_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Tricyclic_Periods <- Tricyclic_Periods %>% left_join(MIG_Drug_Histories) 
Tricyclic_Periods <- Tricyclic_Periods %>% mutate(weight = as.numeric(weight))
Tricyclic_Periods <- Tricyclic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Tricyclic_Periods <- Tricyclic_Periods %>% distinct()

library(spatstat)
weighted.mean(Tricyclic_Periods$Total_Duration, Tricyclic_Periods$weight)  #6.957931
weighted.median(Tricyclic_Periods$Total_Duration, Tricyclic_Periods$weight)  # 6.5






# Analgesic 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "Analgesic")) %>%
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "Analgesic")) %>%
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "Analgesic")) %>%
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "Analgesic")) %>%
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "Analgesic")) %>%
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "Analgesic")) %>%
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "Analgesic")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Analgesic",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Analgesic_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Analgesic_Periods)[3] <- "Duration"
Analgesic_Periods <- Analgesic_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Analgesic_Periods <- Analgesic_Periods %>% left_join(MIG_Drug_Histories) 
Analgesic_Periods <- Analgesic_Periods %>% mutate(weight = as.numeric(weight))
Analgesic_Periods <- Analgesic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Analgesic_Periods <- Analgesic_Periods %>% distinct()

library(spatstat)
weighted.mean(Analgesic_Periods$Total_Duration, Analgesic_Periods$weight)  # 7.940442
weighted.median(Analgesic_Periods$Total_Duration, Analgesic_Periods$weight)  # 8.5









# Calcium 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('107',.), ~replace(., grepl('107', .), "Calcium")) %>%
  mutate_if(grepl('108',.), ~replace(., grepl('108', .), "Calcium")) %>%
  mutate_if(grepl('109',.), ~replace(., grepl('109', .), "Calcium")) %>%
  mutate_if(grepl('110',.), ~replace(., grepl('110', .), "Calcium")) %>%
  mutate_if(grepl('111',.), ~replace(., grepl('111', .), "Calcium")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Calcium",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Calcium_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Calcium_Periods)[3] <- "Duration"
Calcium_Periods <- Calcium_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Calcium_Periods <- Calcium_Periods %>% left_join(MIG_Drug_Histories) 
Calcium_Periods <- Calcium_Periods %>% mutate(weight = as.numeric(weight))
Calcium_Periods <- Calcium_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Calcium_Periods <- Calcium_Periods %>% distinct()

library(spatstat)
weighted.mean(Calcium_Periods$Total_Duration, Calcium_Periods$weight)  # 7.847336
weighted.median(Calcium_Periods$Total_Duration, Calcium_Periods$weight)  # 8.5






# Antipsychotic 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('62',.), ~replace(., grepl('62', .), "Antipsychotic")) %>%
  mutate_if(grepl('63',.), ~replace(., grepl('63', .), "Antipsychotic")) %>%
  mutate_if(grepl('64',.), ~replace(., grepl('64', .), "Antipsychotic")) %>%
  mutate_if(grepl('65',.), ~replace(., grepl('65', .), "Antipsychotic")) %>%
  mutate_if(grepl('66',.), ~replace(., grepl('66', .), "Antipsychotic")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antipsychotic",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Antipsychotic_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Antipsychotic_Periods)[3] <- "Duration"
Antipsychotic_Periods <- Antipsychotic_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Antipsychotic_Periods <- Antipsychotic_Periods %>% left_join(MIG_Drug_Histories) 
Antipsychotic_Periods <- Antipsychotic_Periods %>% mutate(weight = as.numeric(weight))
Antipsychotic_Periods <- Antipsychotic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Antipsychotic_Periods <- Antipsychotic_Periods %>% distinct()

library(spatstat)
weighted.mean(Antipsychotic_Periods$Total_Duration, Antipsychotic_Periods$weight)  # 2.625299
weighted.median(Antipsychotic_Periods$Total_Duration, Antipsychotic_Periods$weight)  # 1



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
  ylab("Weighted Average Duration (months)\n")
# ----
# Evolution of scripts over time --------------------------------------------
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(drug_id, generic_name, drug_group, pat_id, weight, from_dt)

setDT(MIG_Doses_BIG)[, Month_Yr := format(as.Date(from_dt), "%Y-%m") ]

MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(from_dt >= "2015-12-01") %>% filter(from_dt <= "2021-07-31")

length(unique(MIG_Doses_BIG$pat_id)) #200058


MIG_Doses_BIG %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Scripts \n")


MIG_Doses_BIG %>% select(pat_id, weight, Month_Yr) %>% distinct() %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Patients \n")


data.frame(MIG_Doses_BIG %>% select(pat_id,weight, Month_Yr) %>% group_by(Month_Yr) %>% mutate(script_count = sum(as.numeric(weight))) %>% ungroup() %>%
             select(pat_id, weight, Month_Yr, script_count) %>% distinct() %>% group_by(Month_Yr) %>% mutate(pat_count = sum(as.numeric(weight))) %>% 
             select(Month_Yr,script_count, pat_count) %>% distinct() %>% arrange(Month_Yr) %>% mutate(scripts_pat = script_count/pat_count))%>%
  ggplot(aes(x=Month_Yr, y=scripts_pat))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Scripts per Patient \n")


# ----
# Elapsed time since last seen OR last script ---------------------------------------------------------
library(zoo)
RIME_Demographics <- 
  read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

RIME_Demographics_latest_dig <- RIME_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight,migraine_latest) %>%
  mutate(migraine_latest = as.Date(migraine_latest))

MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(pat_id, weight, from_dt)
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(from_dt <= "2021-07-31")
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(from_dt > "2016-07-31")
MIG_Doses_BIG <- MIG_Doses_BIG %>% group_by(pat_id) %>% filter(from_dt == max(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>% distinct()
names(MIG_Doses_BIG)[1] <- "patid"

MIG_Doses_BIG_pats <- MIG_Doses_BIG%>% select(patid)

JOINED <- MIG_Doses_BIG %>% 
  full_join(RIME_Demographics_latest_dig, by = c("patid"="patid", "weight"="weight", "from_dt"="migraine_latest")) %>% arrange(patid)
JOINED <- JOINED %>% group_by(patid) %>% filter(from_dt == max(from_dt))

JOINED <- MIG_Doses_BIG_pats %>% left_join(JOINED)

JOINED %>% ungroup() %>%
  mutate(lag_months = (as.yearmon("2021-7-31") - as.yearmon(JOINED$from_dt))*12) %>%
  mutate(lag_bins = ifelse(lag_months<=12, "< 12 months",
                           ifelse(lag_months>12&lag_months<=24, "12 to 24 months",
                                  ifelse(lag_months>24&lag_months<=36, "24 to 36 months",
                                         ifelse(lag_months>36&lag_months<=48, "36 to 48 months",
                                                ifelse(lag_months>48&lag_months<=60, "48 to 60 months","> 60 months")))))) %>%
  group_by(lag_bins) %>% summarise(pats_sum = sum(as.numeric(weight)))



# ----
# Concomitant drugs before and after Injectable CGRP Start -------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))
   
#pats with inj CGRP after m48
MIG_CGRP_INJ_After_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 >=48)%>%
  filter(grepl("137",d2) | grepl("138",d2) | grepl("139",d2) | grepl("140",d2)) %>% select(patient) %>% distinct()
#pats with inj CGRP before m48
MIG_CGRP_INJ_Before_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 < 48) %>%
  filter(grepl("137",d2) | grepl("138",d2) | grepl("139",d2) | grepl("140",d2) | grepl("137",d1) | grepl("138",d1) | grepl("139",d1) | grepl("140",d1)) %>% select(patient) %>% distinct()
#pats with inj CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% anti_join(MIG_CGRP_INJ_Before_48)
rm(MIG_CGRP_INJ_Before_48)

MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% left_join(MIG_Flows_Aux._Long)
# first occurence of Injectable CGRP, month before and after as well, remove those who started on m60
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% group_by(patient) %>% filter(s2=="I") %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1) %>% mutate(after=p2+1) %>% filter(after <= 60)
# collpase month to single column
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% arrange(patient, n)
#pick only month before and month after
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% group_by(patient)  %>% filter(n==min(n)|n==max(n))
# add drugs
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, p2, d2)), by=c("patient"="patient", "n"="p2"))
# to long, each molecule
MIG_CGRP_INJ_After_48 <- separate_rows(MIG_CGRP_INJ_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_INJ_After_48)[3] <- "molecule"

# drugs look up
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
RIME_Ingredients <- RIME_Ingredients %>% select(molecule, drug_class)
# add drug classes
MIG_CGRP_INJ_After_48<- MIG_CGRP_INJ_After_48 %>% left_join(RIME_Ingredients) %>% arrange(patient)
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% mutate(n=ifelse(n==min(n), "Before", "After"))
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct()))
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% ungroup() %>% select(patient, n, drug_class, weight) %>% distinct()
MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% group_by(patient, n, weight) %>% summarise(drug_classes = toString(drug_class))

MIG_CGRP_INJ_After_48 <- MIG_CGRP_INJ_After_48 %>% ungroup() %>% mutate(drug_classes = ifelse(drug_classes=="NA", "Lapsed", drug_classes))

MIG_CGRP_INJ_After_48  %>% filter(n=="Before") %>% group_by(n, drug_classes) %>% summarise(pats = sum(as.numeric(weight))) %>% arrange(-pats) %>%
  mutate(drug_classes= as.factor(drug_classes)) %>%
  filter(pats>=1000) %>%
  ggplot(aes(x=pats, y=reorder(drug_classes,pats))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="midnightblue" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy immediately BEFORE Injectable CGRP start")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


MIG_CGRP_INJ_After_48  %>% filter(n=="After") %>% group_by(n, drug_classes) %>% summarise(pats = sum(as.numeric(weight))) %>% arrange(-pats) %>%
  mutate(drug_classes= as.factor(drug_classes)) %>%
  filter(pats>=1000) %>%
  ggplot(aes(x=pats, y=reorder(drug_classes,pats))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="firebrick" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy immediately AFTER Injectable CGRP start")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())





# ----
# Concomitant drugs before and after Oral CGRP Start -------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with ORAL CGRP after m48
MIG_CGRP_ORAL_After_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP before m48
MIG_CGRP_ORAL_Before_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("135",d1) | grepl("136",d1)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% anti_join(MIG_CGRP_ORAL_Before_48)
rm(MIG_CGRP_ORAL_Before_48)

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join(MIG_Flows_Aux._Long)

# first occurence of ORAL CGRP, month before and after as well, remove those who started on m60
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient) %>% filter(grepl("135",d2) | grepl("136",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1) %>% mutate(after=p2+1) %>% filter(after <= 60)
# collpase month to single column
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% arrange(patient, n)
#pick only month before and month after
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient)  %>% filter(n==min(n)|n==max(n))
# add drugs
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, p2, d2)), by=c("patient"="patient", "n"="p2"))
# to long, each molecule
MIG_CGRP_ORAL_After_48 <- separate_rows(MIG_CGRP_ORAL_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_ORAL_After_48)[3] <- "molecule"

# drugs look up
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
RIME_Ingredients <- RIME_Ingredients %>% select(molecule, drug_class)
# add drug classes
MIG_CGRP_ORAL_After_48<- MIG_CGRP_ORAL_After_48 %>% left_join(RIME_Ingredients) %>% arrange(patient)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% mutate(n=ifelse(n==min(n), "Before", "After"))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct()))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% select(patient, n, drug_class, weight) %>% distinct()
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient, n, weight) %>% summarise(drug_classes = toString(drug_class))

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% mutate(drug_classes = ifelse(drug_classes=="NA", "Lapsed", drug_classes))

MIG_CGRP_ORAL_After_48  %>% filter(n=="Before") %>% group_by(n, drug_classes) %>% summarise(pats = sum(as.numeric(weight))) %>% arrange(-pats) %>%
  mutate(drug_classes= as.factor(drug_classes)) %>%
  filter(pats>=1000) %>%
  ggplot(aes(x=pats, y=reorder(drug_classes,pats))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="midnightblue" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy immediately BEFORE Oral CGRP start")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



MIG_CGRP_ORAL_After_48  %>% filter(n=="After") %>% group_by(n, drug_classes) %>% summarise(pats = sum(as.numeric(weight))) %>% arrange(-pats) %>%
  mutate(drug_classes= as.factor(drug_classes)) %>%
  filter(pats>=1000) %>%
  ggplot(aes(x=pats, y=reorder(drug_classes,pats))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="firebrick" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy immediately AFTER Oral CGRP start")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



# ----
# CLass Penetrance before and after Oral CGRP Start ------------------------------------------------------------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with ORAL CGRP after m48
MIG_CGRP_ORAL_After_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP before m48
MIG_CGRP_ORAL_Before_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("135",d1) | grepl("136",d1)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% anti_join(MIG_CGRP_ORAL_Before_48)
rm(MIG_CGRP_ORAL_Before_48)

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join(MIG_Flows_Aux._Long)
# first occurence of ORAL CGRP, month before and after as well, remove those who started on m60
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient) %>% filter(grepl("135",d2) | grepl("136",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1) %>% mutate(after=p2+1) %>% filter(after <= 60)
# collpase month to single column

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% arrange(patient, n)

# add drugs
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, p2, s2, d2)), by=c("patient"="patient", "n"="p2"))
# to long, each molecule
MIG_CGRP_ORAL_After_48 <- separate_rows(MIG_CGRP_ORAL_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_ORAL_After_48)[4] <- "molecule"
# drugs look up
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
RIME_Ingredients <- RIME_Ingredients %>% select(molecule, drug_class)
# add drug classes
MIG_CGRP_ORAL_After_48<- MIG_CGRP_ORAL_After_48 %>% left_join(RIME_Ingredients) %>% arrange(patient)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% mutate(n=ifelse(n==min(n), "Before", 
                                                                     ifelse(n==max(n),"After", "Current")))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct()))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% select(patient, n, s2, drug_class, weight) %>% distinct()
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% mutate(drug_class = ifelse(is.na(drug_class), "Lapsed", drug_class))

# Total weights = 179529

before_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before") %>% group_by(n, s2, drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange(s2, -penetrance) %>%
             mutate(drug_class= as.factor(drug_class)))

current_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Current") %>% group_by(n, s2, drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange(s2, -penetrance) %>%
             mutate(drug_class= as.factor(drug_class))) 

after_penetrance <- data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After") %>% group_by(n, s2, drug_class) %>% summarise(penetrance = (sum(as.numeric(weight)))) %>% arrange(s2, -penetrance) %>%
             mutate(drug_class= as.factor(drug_class))) 


write.csv(before_penetrance, "before_Oral_CGRP_s2_penetrance.csv")
write.csv(current_penetrance, "current_Oral_CGRP_s2_penetrance.csv")
write.csv(after_penetrance, "after_Oral_CGRP_s2_penetrance.csv")
write.csv(MIG_CGRP_ORAL_After_48, "MIG_CGRP_ORAL_Pats_Starting_After_48_Class_Penetrance.csv")


# ----
# Number of drugs before and after Oral CGRP Start ------------------------------------------------------------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with ORAL CGRP after m48
MIG_CGRP_ORAL_After_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP before m48
MIG_CGRP_ORAL_Before_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("135",d1) | grepl("136",d1)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% anti_join(MIG_CGRP_ORAL_Before_48)
rm(MIG_CGRP_ORAL_Before_48)

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join(MIG_Flows_Aux._Long)
# first occurence of ORAL CGRP, month before and after as well, remove those who started on m60
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient) %>% filter(grepl("135",d2) | grepl("136",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1) %>% mutate(after=p2+1) %>% filter(after <= 60)
# collpase month to single column
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% arrange(patient, n)

# add drugs
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, p2, d2)), by=c("patient"="patient", "n"="p2"))
# to long, each molecule
MIG_CGRP_ORAL_After_48 <- separate_rows(MIG_CGRP_ORAL_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_ORAL_After_48)[3] <- "molecule"
# drugs look up
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
RIME_Ingredients <- RIME_Ingredients %>% select(molecule, drug_class)
# add drug classes
MIG_CGRP_ORAL_After_48<- MIG_CGRP_ORAL_After_48 %>% left_join(RIME_Ingredients) %>% arrange(patient)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% mutate(n=ifelse(n==min(n), "Before", 
                                                                     ifelse(n==max(n),"After", "Current")))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct()))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% mutate(drug_class = ifelse(is.na(drug_class), "Lapsed", drug_class))

length(unique(MIG_CGRP_ORAL_After_48$patient)) #1689
# Total weights = 179529

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.508011

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Current") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#4.099935

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.851191
# ----
# Number of drugs before and after Oral CGRP Start -4 to +4  -------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with ORAL CGRP after m48
MIG_CGRP_ORAL_After_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP before m48
MIG_CGRP_ORAL_Before_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("135",d1) | grepl("136",d1)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% anti_join(MIG_CGRP_ORAL_Before_48)
rm(MIG_CGRP_ORAL_Before_48)

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join(MIG_Flows_Aux._Long)
# first occurence of ORAL CGRP, month before and after as well, remove those who started on m60
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% group_by(patient) %>% filter(grepl("135",d2) | grepl("136",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% 
  mutate(before=p2-1) %>% mutate(before2=p2-2) %>% mutate(before3=p2-3) %>% mutate(before4=p2-4) %>% 
  mutate(after=p2+1) %>% mutate(after2=p2+2) %>% mutate(after3=p2+3) %>% mutate(after4=p2+4) %>% filter(after4 <= 60)
# collpase month to single column
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% arrange(patient, n)

# add drugs
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, p2, d2)), by=c("patient"="patient", "n"="p2"))
# to long, each molecule
MIG_CGRP_ORAL_After_48 <- separate_rows(MIG_CGRP_ORAL_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_ORAL_After_48)[3] <- "molecule"
# drugs look up
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
RIME_Ingredients <- RIME_Ingredients %>% select(molecule, drug_class)
# add drug classes
MIG_CGRP_ORAL_After_48<- MIG_CGRP_ORAL_After_48 %>% left_join(RIME_Ingredients) %>% arrange(patient)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% mutate(n=ifelse(n==min(n), "Before4",ifelse(n==min(n)+1, "Before3", ifelse(n==min(n)+2, "Before2", ifelse(n==min(n)+3, "Before", 
                                                                                                                                                               ifelse(n==max(n), "After4",ifelse(n==max(n)-1, "After3", ifelse(n==max(n)-2, "After2", ifelse(n==max(n)-3, "After", "Current")))))))))

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join((MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct()))
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% ungroup() %>% mutate(drug_class = ifelse(is.na(drug_class), "Lapsed", drug_class))

length(unique(MIG_CGRP_ORAL_After_48$patient)) #1109


data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before4") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.487297

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before3") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.584094

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before2") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.625233

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Before") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.708382

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="Current") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#4.282656

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.993026

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After2") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.848074

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After3") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.823954

data.frame(MIG_CGRP_ORAL_After_48  %>% filter(n=="After4") %>% filter(drug_class!="Lapsed") %>% group_by(n, patient, weight) %>% 
             summarise(ndrugs = n()) %>% ungroup() %>% summarise(weighted = weighted.mean(ndrugs, as.numeric(weight))))
#3.817062

# ----
# Number of drugs ON vs OFF Oral CGRP lastr 12m naive pats ----------------------------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with ORAL CGRP after m48
MIG_CGRP_ORAL_After_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP before m48
MIG_CGRP_ORAL_Before_48 <- MIG_Flows_Aux._Long %>% 
  filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("135",d1) | grepl("136",d1)) %>% select(patient) %>% distinct()
#pats with ORAL CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% anti_join(MIG_CGRP_ORAL_Before_48)
rm(MIG_CGRP_ORAL_Before_48)

MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% left_join(MIG_Flows_Aux._Long)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% filter(p2>=49) 
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% select(patient, weight, p2, d2)
MIG_CGRP_ORAL_After_48 <- MIG_CGRP_ORAL_After_48 %>% mutate(oralCGRP = ifelse(grepl("135",d2) | grepl("136",d2), "Yes", "No"))

# to long, each molecule
MIG_CGRP_ORAL_After_48 <- separate_rows(MIG_CGRP_ORAL_After_48, d2, sep = ",", convert=T )
names(MIG_CGRP_ORAL_After_48)[4] <- "molecule"

length(unique(MIG_CGRP_ORAL_After_48$patient)) #1872

MIG_CGRP_ORAL_After_48 %>% filter(molecule!= "-") %>% group_by(patient, weight, p2, oralCGRP) %>% summarise(ndrugs=n()) %>% ungroup() %>%
  filter(oralCGRP == "No") %>% summarise(meandrugs = weighted.mean(ndrugs, as.numeric(weight))) #3.33

MIG_CGRP_ORAL_After_48 %>% filter(molecule!= "-") %>% group_by(patient, weight, p2, oralCGRP) %>% summarise(ndrugs=n()) %>% ungroup() %>%
  filter(oralCGRP == "Yes") %>% summarise(meandrugs = weighted.mean(ndrugs, as.numeric(weight))) #4.22


# ----
# MECE Last 12m, MAX class -----------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
# sum across rows, to see hoe many remain zero "0" 
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  # on drug 
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0  # no drug
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% filter(SUM != 0) %>% select(patient)


MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p2=as.numeric(p2))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p2 >=49)

MIG_Flows_Aux._Long <- MIG_Drug_Histories_LONG %>% left_join(MIG_Flows_Aux._Long)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(s2 = ifelse(s2 == "x" , 0,
                                                                  ifelse(s2=="a", 1,
                                                                         ifelse(s2=="A", 2,
                                                                                ifelse(s2=="p",3,
                                                                                       ifelse(s2=="d",4,
                                                                                              ifelse(s2=="D",5,
                                                                                                     ifelse(s2=="O",6,
                                                                                                            ifelse(s2=="I",7,s2)))))))))

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(s2 = as.numeric(s2))

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% arrange(patient) %>% filter(s2 == max(s2)) %>% slice(1)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(s2 = ifelse(s2 == 0 , "x",
                                                                  ifelse(s2==1, "a",
                                                                         ifelse(s2==2, "A",
                                                                                ifelse(s2==3,"p",
                                                                                       ifelse(s2==4,"d",
                                                                                              ifelse(s2==5,"D",
                                                                                                     ifelse(s2==6,"O",
                                                                                                            ifelse(s2==7,"I",s2)))))))))

MIG_Flows_Aux._Long %>% ungroup() %>% group_by(s2) %>% summarise(n=sum(as.numeric(weight)))

MIG_Flows_Aux._Long_MAX <- MIG_Flows_Aux._Long %>% select(patient, weight, s2) 
names(MIG_Flows_Aux._Long_MAX)[3] <- "MAX_stock"

# ----
# Primary vs Secondary flows, Mark's definitions -------------------------------
MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,52:63)

MIG_Box_Histories <- gather(MIG_Box_Histories, Month, Treat, month49:month60, factor_key=TRUE)

MIG_Box_Histories <- MIG_Box_Histories %>% mutate(type = str_sub(Treat, 1L, 1L)) %>% mutate(stock = str_sub(Treat, 2L, 2L))

MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month49", "49")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month50", "50")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month51", "51")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month52", "52")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month53", "53")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month54", "54")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month55", "55")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month56", "56")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month57", "57")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month58", "58")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month59", "59")
MIG_Box_Histories$Month <- str_replace(MIG_Box_Histories$Month, "month60", "60")
MIG_Box_Histories$Month <- as.numeric(MIG_Box_Histories$Month)

MIG_Box_Histories <- MIG_Box_Histories %>% filter(type != "=") %>% mutate(type = ifelse((type=="-"),"Primary", "Secondary"))

MIG_Box_Histories %>% group_by(stock, type) %>% summarise(n=sum(as.numeric(weight)))



MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

MIG_Flows_Aux._Long %>% filter(p2>=49) %>% filter(flow=="1") %>% filter(s1 == s2) %>% group_by(s2) %>% summarise(n=sum(as.numeric(weight)))


# ----
# Patients ever exposure to Rimegepant or Oral CGRP within the last 12m + MAX STOCK ---------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
# sum across rows, to see hoe many remain zero "0" 
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  # on drug 
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0  # no drug
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
# Ever-Treated (i.e. 18 Million)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% filter(SUM != 0) %>% select(patient)

# Long table, filtered for ever-treated
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, d2, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p2=as.numeric(p2))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p2 >=49)
MIG_Flows_Aux._Long <- MIG_Drug_Histories_LONG %>% left_join(MIG_Flows_Aux._Long)

# Pick max stock within the last 12m
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(s2 = ifelse(s2 == "x" , 0,
                                                                  ifelse(s2=="a", 1,
                                                                         ifelse(s2=="A", 2,
                                                                                ifelse(s2=="p",3,
                                                                                       ifelse(s2=="d",4,
                                                                                              ifelse(s2=="D",5,
                                                                                                     ifelse(s2=="O",6,
                                                                                                            ifelse(s2=="I",7,s2)))))))))

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(s2 = as.numeric(s2))

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% arrange(patient) %>% filter(s2 == max(s2)) %>% slice(1)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(s2 = ifelse(s2 == 0 , "x",
                                                                  ifelse(s2==1, "a",
                                                                         ifelse(s2==2, "A",
                                                                                ifelse(s2==3,"p",
                                                                                       ifelse(s2==4,"d",
                                                                                              ifelse(s2==5,"D",
                                                                                                     ifelse(s2==6,"O",
                                                                                                            ifelse(s2==7,"I",s2)))))))))

MIG_Flows_Aux._Long %>% ungroup() %>% group_by(s2) %>% summarise(n=sum(as.numeric(weight)))


# Save max stock within the last 12m
MIG_Flows_Aux._Long_MAX <- MIG_Flows_Aux._Long %>% select(patient, weight, s2) 
names(MIG_Flows_Aux._Long_MAX)[3] <- "MAX_stock"

# # # # # # # 
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, d2, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p2=as.numeric(p2))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p2 >=49)
MIG_Flows_Aux._Long <- MIG_Drug_Histories_LONG %>% left_join(MIG_Flows_Aux._Long)


                                                         MIG_Flows_Aux._Long %>% filter(grepl("135",d2)) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))
Pats_ever_Rimegepant_last_12m <- MIG_Flows_Aux._Long %>% filter(grepl("135",d2)) %>% select(patient, weight) %>% distinct()

Pats_ever_Rimegepant_last_12m %>% left_join(MIG_Flows_Aux._Long_MAX) %>% group_by(MAX_stock) %>% summarise(n=sum(as.numeric(weight)))


MIG_Flows_Aux._Long %>% filter(grepl("135",d2)|grepl("136",d2)) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight)))
Pats_ever_oral_CGRP_last_12m <- MIG_Flows_Aux._Long %>% filter(grepl("135",d2)|grepl("136",d2)) %>% select(patient, weight) %>% distinct()

                                                         
Pats_ever_oral_CGRP_last_12m %>% left_join(MIG_Flows_Aux._Long_MAX) %>% group_by(MAX_stock) %>% summarise(n=sum(as.numeric(weight)))

# ----
# Classs Penetrance across entire 60 month period for patients in PREVENTIVE m60 s2  --------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p2 == "60") %>% filter(s2 =="p")
Pats_Preventive <- MIG_Flows_Aux._Long %>% select(patient) %>% distinct()

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- Pats_Preventive %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
length(unique(MIG_Drug_Histories$patient)) # 56526
sum(as.numeric(MIG_Drug_Histories$weight)) # 4503140
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 4503140)*100)) %>% arrange(-sum_weights_percent)

# ----
# Primary vs Secondary INTRAFLOWS --------------------------------------------
#Import Libs
library(data.table)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(scales)
library(splitstackshape)
library(ggridges)

# Create Look-ups
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

string_CGRP_Oral <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Acute"], collapse = "|"),")\\b")
string_Symptomatic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Preventative <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Preventative"], collapse = "|"),")\\b")

# Long file with flows
flMIGLong   <- fread("MIG_Flows_Aux._Long_v2.txt", integer64 = "character", stringsAsFactors = F)

data <- flMIGLong[flow == 1 & s1 == s2,.(patient,weight,p1,p2,d1,d2,s1,s2,flow)]

# Determine changes in class and number of drugs, for s1 & s2, for each therapy class

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




# Symptomatic Therapy class (a) - flags
data <- data[, Symptomatic_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x,string_Symptomatic),str_c(unlist(str_extract_all(x,string_Symptomatic)),collapse = ","),"")))]
data <- data[, Symptomatic_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x,string_Symptomatic),str_c(unlist(str_extract_all(x,string_Symptomatic)),collapse = ","),"")))]
data <- data[, nr_Symptomatic_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Symptomatic)*1), str_split(x,","))))]
data <- data[, nr_Symptomatic_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Symptomatic)*1), str_split(x,","))))]
data <- data[, nr_Symptomatic_Unq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Symptomatic_d1","Symptomatic_d2")] 
data <- data[, Symptomatic_flow_type := ifelse(nr_Symptomatic_d2 < nr_Symptomatic_d1 & nr_Symptomatic_Unq_d1d2 > nr_Symptomatic_d1, "D+S", 
                                               ifelse(nr_Symptomatic_d2 > nr_Symptomatic_d1 & nr_Symptomatic_Unq_d1d2 > nr_Symptomatic_d2, "A+S",
                                                      ifelse(nr_Symptomatic_d2 < nr_Symptomatic_d1, "D", 
                                                             ifelse(nr_Symptomatic_d2 > nr_Symptomatic_d1, "A", 
                                                                    ifelse(nr_Symptomatic_d2 == nr_Symptomatic_d1 & Symptomatic_d2 != Symptomatic_d1, "S","-")))))]


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






# Intraflows summary table
# A - adding drugs; D - Dropping drugs; S - Switch of drugs; A + S - Adding and switching drugs; D + S - Dropping and switching drugs; 
intrFlw <- data[,.(patient, weight, p1, p2, d1, d2, s1, s2, flow, Oral_CGRP_flow_type, Injectable_CGRP_flow_type, Acute_flow_type, Symptomatic_flow_type, Preventative_flow_type)]
intrFlw <- intrFlw[, year := ifelse(p2 <= 12, 1, ifelse(p2 <= 24, 2, ifelse(p2 <= 36, 3, ifelse(p2 <= 48, 4, 5))))]



intrFlw %>% filter(year == "5") %>% filter(Injectable_CGRP_flow_type == "-") %>% filter(s2 == "I") %>% summarise(n=sum(as.numeric(weight))) #1864815
intrFlw %>% filter(year == "5") %>% filter(Injectable_CGRP_flow_type != "-")  %>% group_by(Injectable_CGRP_flow_type) %>% summarise(n=sum(as.numeric(weight))) #24223.78

intrFlw %>% filter(year == "5") %>% filter(Oral_CGRP_flow_type == "-") %>% filter(s2 == "O") %>% summarise(n=sum(as.numeric(weight))) #148344
intrFlw %>% filter(year == "5") %>% filter(Oral_CGRP_flow_type != "-") %>% filter(s2 == "O") %>% group_by(Oral_CGRP_flow_type) %>% summarise(n=sum(as.numeric(weight))) #4454.38


intrFlw %>% filter(year == "5") %>% filter(Acute_flow_type == "-") %>% filter(s2 == "A") %>% summarise(n=sum(as.numeric(weight))) #526267.8
intrFlw %>% filter(year == "5") %>% filter(Acute_flow_type != "-") %>% filter(s2 == "A") %>% group_by(Acute_flow_type) %>% summarise(n=sum(as.numeric(weight))) 



intrFlw %>% filter(year == "5") %>% filter(Symptomatic_flow_type == "-") %>% filter(s2 == "a") %>% summarise(n=sum(as.numeric(weight))) #0
intrFlw %>% filter(year == "5") %>% filter(Symptomatic_flow_type != "-") %>% filter(s2 == "a") %>% group_by(Symptomatic_flow_type) %>% summarise(n=sum(as.numeric(weight))) 




intrFlw %>% filter(year == "5") %>% filter(Preventative_flow_type == "-") %>% filter(s2 == "p") %>% summarise(n=sum(as.numeric(weight))) #0
intrFlw %>% filter(year == "5") %>% filter(Preventative_flow_type != "-") %>% filter(s2 == "p") %>% group_by(Preventative_flow_type) %>% summarise(n=sum(as.numeric(weight))) 

# ----

# DECISON RESOURCING BUSINESS RULES _ PAT BOXES -------------------------------------
library("data.table")
library("tidyverse")

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))


# 1.A # NEW STARTS. No Rx until the current month --------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories[,4:63]

MIG_Drug_Histories[MIG_Drug_Histories=="-"]<-0
MIG_Drug_Histories[MIG_Drug_Histories!=0]<-1

MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)

no_Rx_till_current <- function(x) ifelse(x == 0, ifelse(cumsum(x==1)>=1, NA, "Lapsed_Till_Current"), NA)

MIG_Drug_Histories_1A <- apply(MIG_Drug_Histories,1,no_Rx_till_current)
MIG_Drug_Histories_1A = t(MIG_Drug_Histories_1A)

MIG_Drug_Histories_1A <- as.data.frame(MIG_Drug_Histories_1A)


# 1.B	# Lapsed in Dec-19 (month 41) for 6+ months, re-starting after Jan-20 (month 42) directly to Oral CGRP ----
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories[,4:63]

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate(month42 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month42 != "-"  & month41 == "-" , "START", month42))%>%
  mutate(month43 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month43 != "-"   & month42 == "-", "START", month43))%>%
  mutate(month44 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month44 != "-"  & month43 == "-", "START", month44))%>%
  mutate(month45 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month45 != "-"  & month44 == "-", "START", month45))%>%
  mutate(month46 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month46 != "-"  & month45 == "-", "START", month46))%>%
  mutate(month47 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month47 != "-"  & month46 == "-", "START", month47))%>%
  mutate(month48 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month48 != "-"  & month47 == "-", "START", month48))%>%
  mutate(month49 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month49 != "-"  & month48 == "-", "START", month49))%>%
  mutate(month50 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month50 != "-"  & month49 == "-", "START", month50))%>%
  mutate(month51 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month51 != "-"  & month50 == "-", "START", month51))%>%
  mutate(month52 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month52 != "-"  & month51 == "-", "START", month52))%>%
  mutate(month53 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month53 != "-"  & month52 == "-", "START", month53))%>%
  mutate(month54 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month54 != "-"  & month53 == "-", "START", month54))%>%
  mutate(month55 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month55 != "-"  & month54 == "-", "START", month55))%>%
  mutate(month56 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month56 != "-"  & month55 == "-", "START", month56))%>%
  mutate(month57 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month57 != "-"  & month56 == "-", "START", month57))%>%
  mutate(month58 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month58 != "-"  & month57 == "-", "START", month58))%>%
  mutate(month59 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month59 != "-"  & month58 == "-", "START", month59))%>%
  mutate(month60 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month60 != "-"  & month59 == "-", "START", month60))

Oral_starts_m42 <- function(x) ifelse(cumsum(x=="START")>=1, "START_Post_Jan20", NA)
MIG_Drug_Histories_1B <- apply(MIG_Drug_Histories,1,Oral_starts_m42)
MIG_Drug_Histories_1B = t(MIG_Drug_Histories_1B)
MIG_Drug_Histories_1B <- as.data.frame(MIG_Drug_Histories_1B)

# 2.A # HIGH INTENSITY. Patients with at least one Rx of a CGRP Injectable previously ----------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories[,4:63]

string_CGRP_Inj <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")

MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, function(x) ifelse(str_detect(x,string_CGRP_Inj),1,0))

Inj_CGRP_previously <- function(x) ifelse(x == 0, ifelse(cumsum(x==1)>=1, "CGRP_Inj", NA), "CGRP_Inj")

MIG_Drug_Histories_2A <- apply(MIG_Drug_Histories,1,Inj_CGRP_previously)
MIG_Drug_Histories_2A = t(MIG_Drug_Histories_2A)
MIG_Drug_Histories_2A <- as.data.frame(MIG_Drug_Histories_2A)


# 2.B # HIGH SEVERETY Patients with at least one Rx of a CGRP Oral previously ---------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories[,4:63]

string_CGRP_Oral <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "CGRP Oral"], collapse = "|"),")\\b")

MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, function(x) ifelse(str_detect(x,string_CGRP_Oral),1,0))

Oral_CGRP_previously <- function(x) ifelse(x == 0, ifelse(cumsum(x==1)>=1, "CGRP_Oral", NA), "CGRP_Oral")

MIG_Drug_Histories_2B <- apply(MIG_Drug_Histories,1,Oral_CGRP_previously)
MIG_Drug_Histories_2B = t(MIG_Drug_Histories_2B)
MIG_Drug_Histories_2B <- as.data.frame(MIG_Drug_Histories_2B)



# 3. # LOW SEVERITY - Patients with no history of preventive OR CGRPs drugs------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories[,4:63]

string_Preventative_AND_CGRP <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Preventative" | RIME_Ingredients$drug_group == "CGRP Oral" |  RIME_Ingredients$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")

MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, function(x) ifelse(str_detect(x,string_Preventative_AND_CGRP),1,0))

Preventative_AND_CGRP_previously <- function(x) ifelse(x == 0, ifelse(cumsum(x==1)==0, "Low_Severity", NA), NA)

MIG_Drug_Histories_3 <- apply(MIG_Drug_Histories,1,Preventative_AND_CGRP_previously)
MIG_Drug_Histories_3 = t(MIG_Drug_Histories_3)
MIG_Drug_Histories_3 <- as.data.frame(MIG_Drug_Histories_3)



# 4A. # MODERATE SEVERITY-Patients currently on Preventive + Acute or Preventive + Symptomatic which are not on any of the other groups-----
MIG_Drug_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories[,4:63]

string_combo_prev <- paste0("\\b(","d|D",")\\b")

MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, function(x) ifelse(str_detect(x,string_combo_prev),1,0))

current_combo_prev <- function(x) ifelse(x == 1, "Combo_Preventive", NA)

MIG_Drug_Histories_4A <- apply(MIG_Drug_Histories,1,current_combo_prev)
MIG_Drug_Histories_4A = t(MIG_Drug_Histories_4A)
MIG_Drug_Histories_4A <- as.data.frame(MIG_Drug_Histories_4A)




# ---------

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient)

MIG_Drug_Histories_1A <- MIG_Drug_Histories %>% bind_cols(MIG_Drug_Histories_1A)
MIG_Drug_Histories_1B <- MIG_Drug_Histories %>% bind_cols(MIG_Drug_Histories_1B)
MIG_Drug_Histories_2A <- MIG_Drug_Histories %>% bind_cols(MIG_Drug_Histories_2A)
MIG_Drug_Histories_2B <- MIG_Drug_Histories %>% bind_cols(MIG_Drug_Histories_2B)
MIG_Drug_Histories_3 <- MIG_Drug_Histories %>% bind_cols(MIG_Drug_Histories_3)
MIG_Drug_Histories_4A <- MIG_Drug_Histories %>% bind_cols(MIG_Drug_Histories_4A)

save(MIG_Drug_Histories_1A, MIG_Drug_Histories_1B, MIG_Drug_Histories_2A, 
     MIG_Drug_Histories_2B, MIG_Drug_Histories_3, MIG_Drug_Histories_4A, file = "Decison_Sourcing_Boxes.RData")

load("Decison_Sourcing_Boxes.RData")

temp <- MIG_Drug_Histories_1A %>% bind_rows(MIG_Drug_Histories_1B) %>% bind_rows(MIG_Drug_Histories_2A) %>%
  bind_rows(MIG_Drug_Histories_2B) %>% bind_rows(MIG_Drug_Histories_3) %>% bind_rows(MIG_Drug_Histories_4A) %>%
  arrange(patient)

temp[] <- lapply(temp, as.character)

temp[2:61][is.na(temp[2:61])] <- 0

temp[temp=="Lapsed_Till_Current"]<-6
temp[temp=="START_Post_Jan20"]<-5
temp[temp=="CGRP_Inj"]<-4
temp[temp=="CGRP_Oral"]<-3
temp[temp=="Low_Severity"]<-2
temp[temp=="Combo_Preventive"]<-1

temp2 <- temp %>% group_by(patient) %>% summarise(across(everything(),max))

temp2 %>% group_by(month60) %>% summarise(n=n())



fwrite(temp2,"Decision_resourcing_boxes.txt")

# Segmentation Classification Boxes ----------------------
Decision_resourcing_boxes <- read.table("Decision_resourcing_boxes.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
Decision_resourcing_boxes <- gather(Decision_resourcing_boxes, Month, Treat, month1:month60, factor_key=TRUE)

Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month1", "1")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month2", "2")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month3", "3")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month4", "4")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month5", "5")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month6", "6")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month7", "7")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month8", "8")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month9", "9")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month10", "10")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month11", "11")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month12", "12")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month13", "13")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month14", "14")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month15", "15")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month16", "16")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month17", "17")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month18", "18")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month19", "19")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month20", "20")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month21", "21")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month22", "22")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month23", "23")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month24", "24")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month25", "25")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month26", "26")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month27", "27")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month28", "28")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month29", "29")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month30", "30")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month31", "31")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month32", "32")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month33", "33")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month34", "34")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month35", "35")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month36", "36")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month37", "37")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month38", "38")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month39", "39")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month40", "40")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month41", "41")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month42", "42")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month43", "43")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month44", "44")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month45", "45")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month46", "46")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month47", "47")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month48", "48")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month49", "49")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month50", "50")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month51", "51")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month52", "52")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month53", "53")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month54", "54")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month55", "55")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month56", "56")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month57", "57")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month58", "58")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month59", "59")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month60", "60")

"Lapsed_Till_Current"<-6
"START_Post_Jan20"<-5
"CGRP_Inj"<-4
"CGRP_Oral"<-3
"Low_Severity"<-2
"Combo_Preventive"<-1
"Other (mod sev, 4b)"<-0

# Classification before and after Rimegepant Start
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with Rimegepant after m48
MIG_Rimegepant_After_48 <- MIG_Flows_Aux._Long %>% filter(p1 >=48)%>%
  filter(grepl("135",d2)) %>% select(patient) %>% distinct()
#pats with Rimegepant before m48
MIG_Rimegepant_Before_48 <- MIG_Flows_Aux._Long %>% filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("135",d1)) %>% select(patient) %>% distinct()
#pats withRimegepant after m48 but not before m48 (i.e. naive)
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% anti_join(MIG_Rimegepant_Before_48)
rm(MIG_Rimegepant_Before_48)
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% left_join(MIG_Flows_Aux._Long)

# first occurence of Rimegepant, month before and after as well
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% group_by(patient) %>% filter(grepl("135",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1)
# collpase month to single column
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% arrange(patient, n)
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% group_by(patient) %>% filter(n==min(n))
MIG_Rimegepant_After_48$n <- as.character(MIG_Rimegepant_After_48$n)

MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% left_join(Decision_resourcing_boxes) %>% filter(n==Month) %>% select(-Month)
names(MIG_Rimegepant_After_48)[3] <- "Segmentation"
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% left_join(MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct())

MIG_Rimegepant_After_48 %>% group_by(Segmentation) %>% summarise(n=sum(as.numeric(weight)))



# Classification before and after CGRP Start
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with CGRP after m48
MIG_CGRP_After_48 <- MIG_Flows_Aux._Long %>% filter(p1 >=48)%>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2) | grepl("138",d2) | grepl("139",d2) | grepl("140",d2)) %>% select(patient) %>% distinct()
#pats with CGRP before m48
MIG_CGRP_Before_48 <- MIG_Flows_Aux._Long %>% filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2) | grepl("138",d2) | grepl("139",d2) | grepl("140",d2) | grepl("135",d1) | grepl("136",d1) | grepl("137",d1) | grepl("138",d1) | grepl("139",d1) | grepl("140",d1)) %>% select(patient) %>% distinct()
#pats CGRP after m48 but not before m48 (i.e. naive)
MIG_CGRP_After_48 <- MIG_CGRP_After_48 %>% anti_join(MIG_CGRP_Before_48)
rm(MIG_CGRP_Before_48)
MIG_CGRP_After_48 <- MIG_CGRP_After_48 %>% left_join(MIG_Flows_Aux._Long)

# first occurence of CGRP, month before and after as well
MIG_CGRP_After_48 <- MIG_CGRP_After_48 %>% group_by(patient) %>% filter(grepl("135",d2) | grepl("136",d2) | grepl("137",d2) | grepl("138",d2) | grepl("139",d2) | grepl("140",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1)
# collpase month to single column
MIG_CGRP_After_48 <- MIG_CGRP_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_CGRP_After_48 <- MIG_CGRP_After_48 %>% arrange(patient, n)
MIG_CGRP_After_48 <- MIG_CGRP_After_48 %>% group_by(patient) %>% filter(n==min(n))
MIG_CGRP_After_48$n <- as.character(MIG_CGRP_After_48$n)

MIG_CGRP_After_48 <- MIG_CGRP_After_48 %>% left_join(Decision_resourcing_boxes) %>% filter(n==Month) %>% select(-Month)
names(MIG_CGRP_After_48)[3] <- "Segmentation"
MIG_CGRP_After_48 <- MIG_CGRP_After_48 %>% left_join(MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct())

MIG_CGRP_After_48 %>% group_by(Segmentation) %>% summarise(n=sum(as.numeric(weight)))


# ----

# Number of Triptans or CGRPs ever tried per patient on each stock -----------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_class)

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories %>% filter(drug_class=="Triptan")
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories %>% filter(drug_class=="CGRP Oral" | drug_class=="CGRP Injectable" )

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% distinct()
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% distinct()

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% select(patient, weight, grp)
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient, weight) %>% summarize(across(everything(), max))

MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% select(patient, weight, grp)
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% group_by(patient, weight) %>% summarize(across(everything(), max))

# Long file with stocks
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, d2, s2, p1_RxExp, flow)

MIG_Flows_Aux._Long_Triptans <- MIG_Flows_Aux._Long %>% left_join(MIG_Drug_Histories_Triptans, by=c("patient"="patient"))
MIG_Flows_Aux._Long_Triptans <- MIG_Flows_Aux._Long_Triptans %>% mutate(grp = ifelse(is.na(grp), 0, grp))

MIG_Flows_Aux._Long_CGRPs <- MIG_Flows_Aux._Long %>% left_join(MIG_Drug_Histories_CGRPs, by=c("patient"="patient"))
MIG_Flows_Aux._Long_CGRPs <- MIG_Flows_Aux._Long_CGRPs %>% mutate(grp = ifelse(is.na(grp), 0, grp))

library(spatstat)
data.frame(MIG_Flows_Aux._Long_Triptans %>% filter(p1_RxExp != 0) %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% 
             mutate(s2 = ifelse(grepl("135",d2),"Rimegepant",
                                ifelse(s2=="O" & !grepl("135",d2), "Ubrogepant",
                                       ifelse(s2=="I", "Injectable", "Other")))) %>% group_by(grp, s2) %>% 
             summarise(n=sum(as.numeric(weight.x))))

data.frame(MIG_Flows_Aux._Long_CGRPs %>% filter(p1_RxExp != 0) %>% mutate(grp=ifelse(grp>=6, 6, grp)) %>% 
             mutate(s2 = ifelse(grepl("135",d2),"Rimegepant",
                                ifelse(s2=="O" & !grepl("135",d2), "Ubrogepant",
                                       ifelse(s2=="I", "Injectable", "Other")))) %>% group_by(grp, s2) %>% 
             summarise(n=sum(as.numeric(weight.x))))


# ------
# Number of dep lines m30, DURATION TO LINE10TH ----------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup() %>% filter(Treat != "-")
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient, weight) %>% distinct(Treat, .keep_all = TRUE)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

Number_lines_treat <- MIG_Drug_Histories %>% ungroup() %>% select(patient, weight, Month, grp) %>% group_by(patient) 

#import again
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories_lines <- MIG_Drug_Histories %>% left_join(Number_lines_treat, by = c("patient"="patient", "weight"="weight", "Month"="Month"))

MIG_Drug_Histories_lines <- data.frame(MIG_Drug_Histories_lines %>% fill(grp))


MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month1", "1")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month2", "2")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month3", "3")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month4", "4")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month5", "5")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month6", "6")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month7", "7")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month8", "8")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month9", "9")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month10", "10")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month11", "11")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month12", "12")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month13", "13")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month14", "14")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month15", "15")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month16", "16")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month17", "17")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month18", "18")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month19", "19")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month20", "20")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month21", "21")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month22", "22")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month23", "23")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month24", "24")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month25", "25")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month26", "26")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month27", "27")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month28", "28")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month29", "29")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month30", "30")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month31", "31")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month32", "32")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month33", "33")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month34", "34")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month35", "35")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month36", "36")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month37", "37")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month38", "38")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month39", "39")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month40", "40")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month41", "41")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month42", "42")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month43", "43")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month44", "44")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month45", "45")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month46", "46")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month47", "47")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month48", "48")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month49", "49")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month50", "50")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month51", "51")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month52", "52")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month53", "53")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month54", "54")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month55", "55")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month56", "56")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month57", "57")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month58", "58")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month59", "59")
MIG_Drug_Histories_lines$Month <- str_replace(MIG_Drug_Histories_lines$Month, "month60", "60")

MIG_Drug_Histories_lines$Month <- as.numeric(MIG_Drug_Histories_lines$Month)

MIG_Drug_Histories_lines_m30_first10 <- data.frame(MIG_Drug_Histories_lines %>% group_by(patient) %>% 
                                                     filter(Month>=30) %>% filter(cumsum(grp == 10) <= 1))

Lines_m30_first10_start_duration <- data.frame(MIG_Drug_Histories_lines_m30_first10 %>% group_by(patient) %>% mutate(dep_lines=min(grp)) %>%
                                                 mutate(Duration=max(Month)-min(Month)))

library(spatstat)
Lines_m30_first10_start_duration %>% select(patient, weight, dep_lines, Duration) %>% distinct() %>% 
  filter(dep_lines<=9) %>% summarise(n=weighted.median(Duration, as.numeric(weight)))

temp <- Lines_m30_first10_start_duration %>% select(patient, weight, dep_lines, Duration) %>% distinct() %>%
  group_by(dep_lines, Duration) %>% summarise(total= sum(as.numeric(weight))) %>%
  spread(key = dep_lines, value = total)

temp2 <- temp %>% select(1:11)

write.csv(temp2, "Time_to_Line10_vs_DepLine_m30.csv")




# --------

# Physician distribution scripts -----------------------------------------
Physicians_Vanguard_Lookup <- read.csv("Physicians_Vanguard_Lookup.csv", colClasses = "character", stringsAsFactors = FALSE)
removeQuotes <- function(x) gsub("\'", "", x)
Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% mutate_if(is.character, removeQuotes)
names(Physicians_Vanguard_Lookup)[1] <- "specialty"

MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 




MIG_Doses_BIG %>% group_by(specialty) %>% summarise(n=n()) %>% left_join(Physicians_Vanguard_Lookup) %>% 
  ungroup() %>% group_by(Physician) %>% summarise(n2=sum(n))


MIG_Doses_BIG %>% filter(drug_group == "CGRP Oral") %>% group_by(specialty) %>% 
  summarise(n=n()) %>% left_join(Physicians_Vanguard_Lookup) %>% ungroup() %>% group_by(Physician) %>% summarise(n2=sum(n))


MIG_Doses_BIG %>% filter(drug_group == "CGRP Injectable") %>% group_by(specialty) %>% 
  summarise(n=n()) %>% left_join(Physicians_Vanguard_Lookup) %>% ungroup() %>% group_by(Physician) %>% summarise(n2=sum(n))


MIG_Doses_BIG %>% filter(drug_class == "Triptan") %>% group_by(specialty) %>% 
  summarise(n=n()) %>% left_join(Physicians_Vanguard_Lookup) %>% ungroup() %>% group_by(Physician) %>% summarise(n2=sum(n))


MIG_Doses_BIG %>% filter(generic_name == "Rimegepant") %>% group_by(specialty) %>% 
  summarise(n=n()) %>% left_join(Physicians_Vanguard_Lookup) %>% ungroup() %>% group_by(Physician) %>% summarise(n2=sum(n))


MIG_Doses_BIG %>% filter(drug_group == "Preventative") %>% group_by(specialty) %>% 
  summarise(n=n()) %>% left_join(Physicians_Vanguard_Lookup) %>% ungroup() %>% group_by(Physician) %>% summarise(n2=sum(n))


#2271 unique neurologists

inj_summary <- MIG_Doses_BIG %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") %>% 
  left_join(Physicians_Vanguard_Lookup) %>% filter(Physician == "NEUROLOGIST") %>% 
  group_by(prov_unique) %>% summarise(n_scripts=n()) %>% arrange(-n_scripts) %>%
  mutate(percent_scripts=(n_scripts/26018)*100) %>%
  mutate(perc_physic=(1/7389)*100)%>%
  mutate(perc_physic_CUM=cumsum(perc_physic))%>%
  mutate(percent_scripts_CUM=cumsum(percent_scripts))%>%
  ggplot(aes(perc_physic_CUM, percent_scripts_CUM))+
  geom_point(colour="firebrick")+
  xlim(0,100)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nProportion of Neurologists #")+ylab("Proportion of CGRP Scripts\n")




MIG_Doses_BIG %>% filter(drug_group == "CGRP Injectable"| drug_group == "CGRP Oral") %>% 
  arrange(pat_id, from_dt) %>% group_by(pat_id) %>% slice(1) %>% ungroup()%>%
  group_by(specialty) %>% summarise(n=n()) %>% left_join(Physicians_Vanguard_Lookup) %>%
  ungroup() %>% group_by(Physician) %>% summarise(n2=sum(n))


MIG_Doses_BIG %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician == "NEUROLOGIST") %>%
  group_by(prov_unique) %>% count() #7389 unique neurologists

MIG_Doses_BIG %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician == "NEUROLOGIST")  %>% 
  filter(drug_group == "CGRP Injectable"| drug_group == "CGRP Oral") %>%
  group_by(prov_unique) %>% count() # 2,271


MIG_Doses_BIG %>% filter(drug_group == "CGRP Oral") %>% 
  left_join(Physicians_Vanguard_Lookup) %>% filter(Physician == "NEUROLOGIST") %>% 
  group_by(prov_unique) %>% summarise(n_scripts=n()) %>% arrange(-n_scripts) %>%
  mutate(percent_scripts=(n_scripts/4162)*100) %>%
  mutate(perc_physic=(1/7389)*100)%>%
  mutate(perc_physic_CUM=cumsum(perc_physic))%>%
  mutate(percent_scripts_CUM=cumsum(percent_scripts))%>%
  ggplot(aes(perc_physic_CUM, percent_scripts_CUM))+
  xlim(0,100)+
  geom_point(colour="midnightblue")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nProportion of Neurologists #")+ylab("Proportion of Oral CGRP Scripts\n")


MIG_Doses_BIG %>% filter(drug_class == "Triptan") %>% 
  left_join(Physicians_Vanguard_Lookup) %>% filter(Physician == "NEUROLOGIST") %>% 
  group_by(prov_unique) %>% summarise(n_scripts=n()) %>% arrange(-n_scripts) %>%
  mutate(percent_scripts=(n_scripts/32596)*100) %>%
  mutate(perc_physic=(1/7389)*100)%>%
  mutate(perc_physic_CUM=cumsum(perc_physic))%>%
  mutate(percent_scripts_CUM=cumsum(percent_scripts))%>%
  ggplot(aes(perc_physic_CUM, percent_scripts_CUM))+
  xlim(0,100)+
  geom_point(colour="darkseagreen")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nProportion of Neurologists #")+ylab("Proportion of Triptan Scripts\n")



# ----
# Pills per month Rimegepant --------------------------------------------------
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(prov_unique, prov_type, specialty, taxonomy1, taxonomy2, status, drug_group, drug_class, drug_id))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(generic_name=="Rimegepant")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(dayssup, generic_name))

MIG_Doses_BIG <- MIG_Doses_BIG %>% group_by(pat_id) %>% 
  mutate(lapsed_time = max(interval(from_dt, "2021-07-31") %/% months(1)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(months = format(as.Date(from_dt), "%Y-%m")) %>%
  mutate(N_months_treat = length(unique(months)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(lapsed_time = ifelse(lapsed_time==0,1,lapsed_time))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month = (max(row_number())*8)/lapsed_time)

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month_treated = (max(row_number())*8)/N_months_treat)

MIG_Doses_BIG <- MIG_Doses_BIG %>% ungroup() %>% select(pat_id, weight, lapsed_time, N_months_treat, pils_per_month, pils_per_month_treated) %>% distinct()

write.csv(MIG_Doses_BIG, "Rimegepant_MonthsTreated_vs_PillsPerMonth_UNGROUPED.csv")

summary_pills_per_month <- data.frame(MIG_Doses_BIG %>% ungroup() %>% mutate(bucket = round(pils_per_month_treated))%>% 
                                        group_by(N_months_treat, bucket) %>% summarise(n=sum(as.numeric(weight))))

write.csv(summary_pills_per_month, "Rimegepant_MonthsTreated_vs_PillsPerMonth.csv")

# ----

# Rimegepant Persistensy vs Visibility Total Duration  --------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "Rimegepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Rimegepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

# count (how many months) in each of these  periods
Rimegepant_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Rimegepant_Periods_MIG)[3] <- "Duration"

Rimegepant_Periods_MIG_VIZ <- Rimegepant_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                     select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Rimegepant_Periods_MIG_VIZ, "Rimegepant_Periods_MIG_VIZ.csv")


# Rimegepant_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
#                                        select(patient, weight, visibility), by=c("patient"="patient")) %>% 
#   distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
#   select(patient, weight, visibility, Total_duration) %>% distinct() %>% summarise(n=weighted.mean(Total_duration, weight))
# 3.26 mean
# 1.5 median

# ----
# Rimegepant Persistensy vs Visibility First Exposure --------------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "Rimegepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Rimegepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

# count (how many months) in each of these  periods
Rimegepant_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Rimegepant_Periods_MIG)[3] <- "Duration"

Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% filter(grp==min(grp))

Rimegepant_Periods_MIG_VIZ_First <- Rimegepant_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Rimegepant_Periods_MIG_VIZ_First, "Rimegepant_Periods_MIG_VIZ_First.csv")

Rimegepant_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                       select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% summarise(n=weighted.median(Total_duration, weight))


# ----
# Paths to Rimegepant -----------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, p1, p2, d1, d2, s1, s2)

Rimegepant_Patients <- MIG_Flows_Aux._Long %>% filter(grepl("135",d2) |grepl("135",d1)) %>% select(patient) %>% distinct()

Rimegepant_Patients <- Rimegepant_Patients %>% left_join(MIG_Drug_Histories)
Rimegepant_Patients <- Rimegepant_Patients %>% select(-c(disease))
Rimegepant_Patients <- gather(Rimegepant_Patients, Month, Treat, month1:month60, factor_key=TRUE)
Rimegepant_Patients <- separate_rows(Rimegepant_Patients, Treat, sep = ",", convert=T )
names(Rimegepant_Patients)[4] <- "molecule"

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
RIME_Ingredients <- RIME_Ingredients %>% select(molecule, drug_class)
Rimegepant_Patients<- Rimegepant_Patients %>% left_join(RIME_Ingredients) %>%  arrange(patient)

Rimegepant_Patients_first_48m <- Rimegepant_Patients %>% group_by(patient) %>% filter(Month == "month1" | Month == "month2" | Month == "month3" | Month == "month4" |
                                                                                        Month == "month5" | Month == "month6" | Month == "month7" | Month == "month8" |
                                                                                        Month == "month9" | Month == "month10" | Month == "month11" | Month == "month12" | 
                                                                                        Month == "month13" | Month == "month14" | Month == "month15" | Month == "month16" |
                                                                                        Month == "month17" | Month == "month18" | Month == "month19" | Month == "month20" |
                                                                                        Month == "month21" | Month == "month22" | Month == "month23" | Month == "month24" | 
                                                                                        Month == "month25" | Month == "month26" | Month == "month27" | Month == "month28" |
                                                                                        Month == "month29" | Month == "month30" | Month == "month31" | Month == "month32" |
                                                                                        Month == "month33" | Month == "month34" | Month == "month35" | Month == "month36" | 
                                                                                        Month == "month37" | Month == "month38" | Month == "month39" | Month == "month40" |
                                                                                        Month == "month41" | Month == "month42" | Month == "month43" | Month == "month44" |
                                                                                        Month == "month45" | Month == "month46" | Month == "month47" | Month == "month48") %>%
  filter(molecule == "135") %>% select(patient) %>% distinct()

Rimegepant_Patients <- Rimegepant_Patients %>% anti_join(Rimegepant_Patients_first_48m)

Rimegepant_Patients2 <- Rimegepant_Patients %>% group_by(patient, weight) %>% 
  slice(if(any(molecule == "135")) 1:which.max(molecule == "135") else row_number())   

Paths_to_Rimegepant <- Rimegepant_Patients2 %>% ungroup %>% select(-c(molecule)) %>% group_by(patient, Month, drug_class) %>% 
  distinct() %>% ungroup %>% group_by(patient, Month) %>% 
  mutate(paths_month = paste(drug_class, collapse=" + ")) %>% 
  ungroup() %>% group_by(patient, weight, paths_month) %>% 
  select(patient,weight, paths_month) %>% distinct() %>%
  select(patient, weight, paths_month) %>% ungroup()

Month_prior_to_Rimegepant_combos <- data.frame(Rimegepant_Patients2 %>% ungroup %>% select(-c(molecule)) %>% group_by(patient, Month, drug_class) %>% 
                                                 distinct() %>% ungroup %>% group_by(patient, Month) %>% 
                                                 mutate(paths_month = paste(drug_class, collapse=" + ")) %>% 
                                                 ungroup() %>% group_by(patient, weight, paths_month) %>% 
                                                 select(patient,weight, paths_month) %>% distinct() %>%
                                                 select(patient, weight, paths_month) %>% ungroup() %>% group_by(patient) %>% 
                                                 filter(row_number() == (n() - 1)) %>% ungroup() %>% group_by(paths_month) %>%
                                                 summarise(n = sum(as.numeric(weight))) %>% arrange(-n))


Rimegepant_Patients2_stocks <- Rimegepant_Patients2

Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month1", "1")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month2", "2")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month3", "3")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month4", "4")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month5", "5")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month6", "6")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month7", "7")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month8", "8")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month9", "9")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month10", "10")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month11", "11")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month12", "12")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month13", "13")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month14", "14")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month15", "15")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month16", "16")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month17", "17")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month18", "18")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month19", "19")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month20", "20")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month21", "21")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month22", "22")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month23", "23")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month24", "24")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month25", "25")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month26", "26")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month27", "27")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month28", "28")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month29", "29")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month30", "30")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month31", "31")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month32", "32")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month33", "33")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month34", "34")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month35", "35")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month36", "36")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month37", "37")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month38", "38")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month39", "39")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month40", "40")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month41", "41")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month42", "42")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month43", "43")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month44", "44")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month45", "45")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month46", "46")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month47", "47")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month48", "48")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month49", "49")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month50", "50")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month51", "51")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month52", "52")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month53", "53")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month54", "54")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month55", "55")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month56", "56")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month57", "57")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month58", "58")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month59", "59")
Rimegepant_Patients2_stocks$Month <- str_replace(Rimegepant_Patients2_stocks$Month, "month60", "60")

MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, p1, s1)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p1 ==1)
Rimegepant_Patients2_stocks <- Rimegepant_Patients2_stocks %>% left_join(MIG_Flows_Aux._Long, by = c("patient"="patient", "Month"="p1"))

MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, p2, s2)
Rimegepant_Patients2_stocks <- Rimegepant_Patients2_stocks %>% left_join(MIG_Flows_Aux._Long, by = c("patient"="patient", "Month"="p2"))

Rimegepant_Patients2_stocks <- Rimegepant_Patients2_stocks %>% mutate(s2 = ifelse(is.na(s2), s1, s2))
Rimegepant_Patients2_stocks <- Rimegepant_Patients2_stocks%>%select(-c(s1))
names(Rimegepant_Patients2_stocks)[6] <- "stock"

data.frame(Rimegepant_Patients2_stocks %>% ungroup %>% select(-c(molecule, drug_class)) %>% group_by(patient, weight, Month, stock) %>% 
             distinct() %>% ungroup %>% group_by(patient) %>% 
             filter(row_number() >= (n() - 12)) %>%
             ungroup() %>% group_by(patient) %>%
             mutate(num = row_number()) %>% ungroup() %>% group_by(num, stock) %>%
             summarise(total = sum(as.numeric(weight))))

Stocks_evol_to_Rimegepant_13m_each_box <- data.frame(Rimegepant_Patients2_stocks %>% ungroup %>% select(-c(molecule, drug_class)) %>% group_by(patient, weight, Month, stock) %>% 
                                                       distinct() %>% ungroup() %>% group_by(patient) %>% 
                                                       filter(row_number() >= (n() - 12)) %>%
                                                       ungroup() %>% 
                                                       select(patient, weight, stock) %>% 
                                                       group_by(patient, weight) %>% 
                                                       mutate(paths_stocks = paste(stock, collapse="->")) %>% 
                                                       ungroup() %>% select(-c(stock)) %>% distinct() %>% group_by(paths_stocks) %>%
                                                       summarise(total = sum(as.numeric(weight))) %>% arrange(-total))


Stocks_evol_to_Rimegepant_13m <- data.frame(Rimegepant_Patients2_stocks %>% ungroup %>% select(-c(molecule, drug_class)) %>% group_by(patient, weight, Month, stock) %>% 
                                              distinct() %>% ungroup() %>% group_by(patient) %>% 
                                              filter(row_number() >= (n() - 12)) %>%
                                              ungroup() %>% 
                                              select(patient, weight, stock) %>% 
                                              group_by(patient, weight) %>% 
                                              mutate(paths_stocks = paste(stock, collapse="->")) %>% 
                                              ungroup() %>% select(-c(stock)) %>% distinct() %>% group_by(paths_stocks) %>%
                                              summarise(total = sum(as.numeric(weight))) %>% arrange(-total))


Stocks_evol_to_Rimegepant_13m <- Stocks_evol_to_Rimegepant_13m %>% mutate(ID = row_number())

Stocks_evol_to_Rimegepant_13m <- separate_rows(Stocks_evol_to_Rimegepant_13m, paths_stocks, sep = "->", convert=T )

Stocks_evol_to_Rimegepant_13m <- data.frame(Stocks_evol_to_Rimegepant_13m %>% group_by(ID) %>% filter(row_number()==1 | lag(paths_stocks) != paths_stocks))

Stocks_evol_to_Rimegepant_13m <- Stocks_evol_to_Rimegepant_13m %>% group_by(ID) %>% mutate(paths_stocks = paste(paths_stocks, collapse="->")) %>% 
  ungroup() %>% group_by(ID, total, paths_stocks) %>% distinct()

data.frame(Stocks_evol_to_Rimegepant_13m)

Stocks_evol_to_Rimegepant_13m <- Stocks_evol_to_Rimegepant_13m %>% ungroup() %>% group_by(paths_stocks) %>% 
  summarise(total=sum(total)) %>% arrange(-total)

write.csv(Stocks_evol_to_Rimegepant_13m, "Stocks_evol_to_Rimegepant_13m.csv")

# ----
# Nr Lines v Stock --------------
drgDIA2 <- fread("MIG Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

data <- data.frame(drgDIA2, stringsAsFactors = F)

nrLines <- data[,c(1:3)] 
nrLines$month1 <- (data$month1 != "-")*1

for(i in 2:60){
  cat(i)
  nrLines[,i+3] <- apply(data[,(4:(i+3))], 1, function(x) length(unique(x[x!="-"])))
  names(nrLines)[i+3] <- paste0("month",i)
}

fwrite(nrLines,"MIG_nrLines_Histories.txt")


# ----
# How many patients in each stock across lines of therapy over time? 6 to 12 FILTERED ----------
# Import file
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month1", "1")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month2", "2")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month3", "3")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month4", "4")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month5", "5")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month6", "6")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month7", "7")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month8", "8")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month9", "9")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month10", "10")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month11", "11")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month12", "12")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month13", "13")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month14", "14")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month15", "15")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month16", "16")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month17", "17")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month18", "18")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month19", "19")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month20", "20")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month21", "21")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month22", "22")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month23", "23")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month24", "24")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month25", "25")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month26", "26")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month27", "27")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month28", "28")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month29", "29")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month30", "30")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month31", "31")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month32", "32")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month33", "33")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month34", "34")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month35", "35")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month36", "36")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month37", "37")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month38", "38")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month39", "39")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month40", "40")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month41", "41")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month42", "42")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month43", "43")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month44", "44")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month45", "45")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month46", "46")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month47", "47")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month48", "48")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month49", "49")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month50", "50")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month51", "51")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month52", "52")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month53", "53")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month54", "54")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month55", "55")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month56", "56")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month57", "57")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month58", "58")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month59", "59")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month60", "60")

MIG_nrLines_Histories_pats <- MIG_nrLines_Histories
MIG_nrLines_Histories_pats<- MIG_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 6 were on 0 Lines (lapsed/naive)
MIG_nrLines_Histories_pats_naive <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=6) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 6) %>% select(patient) %>% distinct()

#pats that after Month 6 were started on some line
MIG_nrLines_Histories_pats_start_6_to_12 <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >6 & Month <=12) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
MIG_nrLines_Histories_pats_track <- MIG_nrLines_Histories_pats_start_6_to_12  %>% inner_join(MIG_nrLines_Histories_pats_naive)

rm(MIG_nrLines_Histories_pats_start_6_to_12, MIG_nrLines_Histories_pats_naive, MIG_nrLines_Histories)

# lines of the selected patients
MIG_nrLines_Histories_pats <- MIG_nrLines_Histories_pats_track %>% left_join(MIG_nrLines_Histories_pats)
MIG_nrLines_Histories <- MIG_nrLines_Histories_pats
rm(MIG_nrLines_Histories_pats)
rm(MIG_nrLines_Histories_pats_track)
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Treat != "0")

###
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2)

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% left_join(MIG_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(MIG_nrLines_Histories)[5] <- "stock"

MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)

#recode line number
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Month_new <49)

MIG_nrLines_Histories_SUMMARY <- data.frame(MIG_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

MIG_nrLines_Histories_SUMMARY <- MIG_nrLines_Histories_SUMMARY %>% arrange(stock, Treat, Month_new)

MIG_nrLines_Histories_SUMMARY$Index <- paste(MIG_nrLines_Histories_SUMMARY$stock, MIG_nrLines_Histories_SUMMARY$Treat)

plots <- MIG_nrLines_Histories_SUMMARY %>% filter(stock == "O") %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                                             aes(x=Month_new, y=sum) + geom_area(fill="midnightblue") + xlim(0,48)+ ylim(0,450)+ ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
)

plots[[2]]

write.csv(MIG_nrLines_Histories_SUMMARY, "MIG_nrLines_Histories_SUMMARY_FILTERED_6_12.csv")

data.frame(MIG_nrLines_Histories_SUMMARY %>% filter(Month_new == 48) %>%group_by(Treat, stock) %>% 
             summarise(total = sum(sum)) %>% group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))


data.frame(MIG_nrLines_Histories_SUMMARY %>% filter(stock == "O") %>% 
             mutate(total=sum(sum)) %>% group_by(Treat) %>% mutate(n_line = sum(sum)) %>% mutate(percent=n_line/total) %>% select(percent) %>% distinct())

# ----
# How many patients in each stock across lines of therapy over time? 24 to 36 FILTERED ----------
# Import file
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month1", "1")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month2", "2")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month3", "3")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month4", "4")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month5", "5")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month6", "6")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month7", "7")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month8", "8")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month9", "9")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month10", "10")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month11", "11")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month12", "12")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month13", "13")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month14", "14")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month15", "15")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month16", "16")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month17", "17")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month18", "18")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month19", "19")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month20", "20")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month21", "21")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month22", "22")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month23", "23")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month24", "24")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month25", "25")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month26", "26")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month27", "27")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month28", "28")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month29", "29")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month30", "30")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month31", "31")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month32", "32")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month33", "33")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month34", "34")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month35", "35")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month36", "36")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month37", "37")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month38", "38")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month39", "39")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month40", "40")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month41", "41")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month42", "42")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month43", "43")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month44", "44")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month45", "45")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month46", "46")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month47", "47")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month48", "48")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month49", "49")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month50", "50")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month51", "51")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month52", "52")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month53", "53")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month54", "54")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month55", "55")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month56", "56")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month57", "57")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month58", "58")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month59", "59")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month60", "60")

MIG_nrLines_Histories_pats <- MIG_nrLines_Histories
MIG_nrLines_Histories_pats<- MIG_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 24 were on 0 Lines (lapsed/naive)
MIG_nrLines_Histories_pats_naive <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=24) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 24) %>% select(patient) %>% distinct()

#pats that after Month 24 were started on some line
MIG_nrLines_Histories_pats_start_24_to_36 <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >24 & Month <=36) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
MIG_nrLines_Histories_pats_track <- MIG_nrLines_Histories_pats_start_24_to_36  %>% inner_join(MIG_nrLines_Histories_pats_naive)

rm(MIG_nrLines_Histories_pats_start_24_to_36, MIG_nrLines_Histories_pats_naive, MIG_nrLines_Histories)

# lines of the selected patients
MIG_nrLines_Histories_pats <- MIG_nrLines_Histories_pats_track %>% left_join(MIG_nrLines_Histories_pats)
MIG_nrLines_Histories <- MIG_nrLines_Histories_pats
rm(MIG_nrLines_Histories_pats)
rm(MIG_nrLines_Histories_pats_track)
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Treat != "0")

###
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2)

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% left_join(MIG_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(MIG_nrLines_Histories)[5] <- "stock"

MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)

#recode line number
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Month_new <25)

MIG_nrLines_Histories_SUMMARY_24_to_36 <- data.frame(MIG_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

MIG_nrLines_Histories_SUMMARY_24_to_36 <- MIG_nrLines_Histories_SUMMARY_24_to_36 %>% arrange(stock, Treat, Month_new)

MIG_nrLines_Histories_SUMMARY_24_to_36$Index <- paste(MIG_nrLines_Histories_SUMMARY_24_to_36$stock, MIG_nrLines_Histories_SUMMARY_24_to_36$Treat)

plots <- MIG_nrLines_Histories_SUMMARY_24_to_36 %>% filter(stock == "O") %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                                                      aes(x=Month_new, y=sum) + geom_area(fill="midnightblue") + xlim(0,48)+ ylim(0,450)+ ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
)

plots[[2]]

write.csv(MIG_nrLines_Histories_SUMMARY_24_to_36, "MIG_nrLines_Histories_SUMMARY_FILTERED_24_36.csv")

data.frame(MIG_nrLines_Histories_SUMMARY %>% filter(Month_new == 24) %>%group_by(Treat, stock) %>% 
             summarise(total = sum(sum)) %>% group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))



data.frame(MIG_nrLines_Histories_SUMMARY_24_to_36 %>% filter(stock == "O") %>% 
             mutate(total=sum(sum)) %>% group_by(Treat) %>% mutate(n_line = sum(sum)) %>% mutate(percent=n_line/total) %>% select(percent) %>% distinct())





# ----
# How many patients in each stock across lines of therapy over time? 36 to 48 FILTERED ----------
# Import file
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month1", "1")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month2", "2")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month3", "3")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month4", "4")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month5", "5")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month6", "6")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month7", "7")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month8", "8")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month9", "9")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month10", "10")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month11", "11")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month12", "12")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month13", "13")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month14", "14")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month15", "15")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month16", "16")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month17", "17")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month18", "18")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month19", "19")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month20", "20")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month21", "21")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month22", "22")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month23", "23")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month24", "24")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month25", "25")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month26", "26")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month27", "27")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month28", "28")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month29", "29")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month30", "30")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month31", "31")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month32", "32")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month33", "33")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month34", "34")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month35", "35")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month36", "36")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month37", "37")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month38", "38")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month39", "39")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month40", "40")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month41", "41")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month42", "42")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month43", "43")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month44", "44")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month45", "45")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month46", "46")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month47", "47")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month48", "48")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month49", "49")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month50", "50")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month51", "51")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month52", "52")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month53", "53")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month54", "54")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month55", "55")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month56", "56")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month57", "57")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month58", "58")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month59", "59")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month60", "60")

MIG_nrLines_Histories_pats <- MIG_nrLines_Histories
MIG_nrLines_Histories_pats<- MIG_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 36 were on 0 Lines (lapsed/naive)
MIG_nrLines_Histories_pats_naive <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=36) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 36) %>% select(patient) %>% distinct()

#pats that after Month 36 were started on some line
MIG_nrLines_Histories_pats_start_36_to_48 <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >36 & Month <=48) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()


#pats to track
MIG_nrLines_Histories_pats_track <- MIG_nrLines_Histories_pats_start_36_to_48  %>% inner_join(MIG_nrLines_Histories_pats_naive)

rm(MIG_nrLines_Histories_pats_start_36_to_48, MIG_nrLines_Histories_pats_naive, MIG_nrLines_Histories)

# lines of the selected patients
MIG_nrLines_Histories_pats <- MIG_nrLines_Histories_pats_track %>% left_join(MIG_nrLines_Histories_pats)
MIG_nrLines_Histories <- MIG_nrLines_Histories_pats
rm(MIG_nrLines_Histories_pats)
rm(MIG_nrLines_Histories_pats_track)
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Treat != "0")

###
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2)

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% left_join(MIG_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(MIG_nrLines_Histories)[5] <- "stock"

MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)

#recode line number
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Month_new <12)

MIG_nrLines_Histories_SUMMARY_36_to_48 <- data.frame(MIG_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

MIG_nrLines_Histories_SUMMARY_36_to_48 <- MIG_nrLines_Histories_SUMMARY_36_to_48 %>% arrange(stock, Treat, Month_new)

MIG_nrLines_Histories_SUMMARY_36_to_48$Index <- paste(MIG_nrLines_Histories_SUMMARY_36_to_48$stock, MIG_nrLines_Histories_SUMMARY_36_to_48$Treat)

plots <- MIG_nrLines_Histories_SUMMARY_36_to_48 %>% filter(stock == "O") %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                                                      aes(x=Month_new, y=sum) + geom_area(fill="midnightblue") + xlim(0,48)+ ylim(0,450)+ ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
)

plots[[2]]

write.csv(MIG_nrLines_Histories_SUMMARY_36_to_48, "MIG_nrLines_Histories_SUMMARY_36_to_48.csv")

data.frame(MIG_nrLines_Histories_SUMMARY_36_to_48 %>% filter(Month_new == 12) %>%group_by(Treat, stock) %>% 
             summarise(total = sum(sum)) %>% group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))



# ----
# Rimegepant Durations 1month vs 3+ months type pf pat  -------------------
#age and gender
# vector with Rimegepant patients and first duration
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "Rimegepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Rimegepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

# count (how many months) in each of these  periods
Rimegepant_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Rimegepant_Periods_MIG)[3] <- "Duration"

Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% filter(grp==min(grp))
Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% select(-c(grp))
Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% mutate(Duration_bucket = ifelse(Duration == 1, "1month", 
                                                                                     ifelse(Duration>=3,"+3months", "2months")))
Rimegepant_Periods_MIG_2 <- Rimegepant_Periods_MIG


# Age and gender
library(spatstat)
RIME_Demographics <- 
  read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

RIME_Demographics <- RIME_Demographics %>% select(patid, weight, gender, age)

Rimegepant_Periods_MIG_2 <- Rimegepant_Periods_MIG_2 %>% left_join(RIME_Demographics, by=c("patient"="patid"))

Rimegepant_Periods_MIG_2 %>% ungroup() %>% group_by(Duration_bucket) %>% summarise(mean_age = weighted.mean(as.numeric(age), as.numeric(weight)))

                                                         
Rimegepant_Periods_MIG_2 %>% ungroup() %>% group_by(Duration_bucket) %>% summarise(median_age = weighted.median(as.numeric(age), as.numeric(weight)))

Rimegepant_Periods_MIG_2 %>% ungroup() %>% group_by(Duration_bucket, gender) %>% summarise(pats = sum(as.numeric(weight)))


                                                         
# nr of lines
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "Rimegepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Rimegepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% slice(1)
Month_First_start <- MIG_Drug_Histories %>% select(patient, Month)

Month_First_start$Month <- str_replace(Month_First_start$Month, "month1", "1")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month2", "2")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month3", "3")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month4", "4")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month5", "5")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month6", "6")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month7", "7")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month8", "8")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month9", "9")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month10", "10")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month11", "11")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month12", "12")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month13", "13")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month14", "14")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month15", "15")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month16", "16")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month17", "17")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month18", "18")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month19", "19")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month20", "20")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month21", "21")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month22", "22")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month23", "23")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month24", "24")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month25", "25")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month26", "26")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month27", "27")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month28", "28")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month29", "29")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month30", "30")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month31", "31")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month32", "32")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month33", "33")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month34", "34")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month35", "35")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month36", "36")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month37", "37")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month38", "38")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month39", "39")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month40", "40")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month41", "41")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month42", "42")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month43", "43")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month44", "44")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month45", "45")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month46", "46")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month47", "47")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month48", "48")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month49", "49")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month50", "50")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month51", "51")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month52", "52")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month53", "53")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month54", "54")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month55", "55")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month56", "56")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month57", "57")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month58", "58")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month59", "59")
Month_First_start$Month <- str_replace(Month_First_start$Month, "month60", "60")

Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% left_join(Month_First_start)

# N of lines over time
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month1", "1")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month2", "2")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month3", "3")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month4", "4")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month5", "5")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month6", "6")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month7", "7")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month8", "8")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month9", "9")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month10", "10")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month11", "11")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month12", "12")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month13", "13")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month14", "14")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month15", "15")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month16", "16")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month17", "17")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month18", "18")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month19", "19")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month20", "20")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month21", "21")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month22", "22")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month23", "23")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month24", "24")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month25", "25")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month26", "26")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month27", "27")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month28", "28")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month29", "29")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month30", "30")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month31", "31")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month32", "32")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month33", "33")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month34", "34")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month35", "35")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month36", "36")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month37", "37")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month38", "38")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month39", "39")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month40", "40")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month41", "41")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month42", "42")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month43", "43")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month44", "44")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month45", "45")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month46", "46")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month47", "47")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month48", "48")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month49", "49")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month50", "50")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month51", "51")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month52", "52")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month53", "53")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month54", "54")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month55", "55")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month56", "56")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month57", "57")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month58", "58")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month59", "59")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month60", "60")

Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG %>% left_join(MIG_nrLines_Histories %>% select(patient, Month, weight, Treat), by=c("patient"="patient", "Month"="Month"))

Rimegepant_Periods_MIG_3$Treat <- as.numeric(Rimegepant_Periods_MIG_3$Treat)
Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>% ungroup()
Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.mean(Treat, as.numeric(weight)))

                                                         
Rimegepant_Periods_MIG_3  %>% group_by(Duration_bucket) %>% summarise(n=weighted.median(Treat, as.numeric(weight)))

                                                         


# Number of Triptans or CGRPs or Preventives ever tried per patient on each stock 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_class)

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories %>% filter(drug_class=="Triptan")
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories %>% filter(drug_class=="CGRP Oral" | drug_class=="CGRP Injectable" )

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% distinct()
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% distinct()

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% select(patient, weight, grp)
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient, weight) %>% summarize(across(everything(), max))
names(MIG_Drug_Histories_Triptans)[3] <- "N_Triptans"

MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% select(patient, weight, grp)
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% group_by(patient, weight) %>% summarize(across(everything(), max))
names(MIG_Drug_Histories_CGRPs)[3] <- "N_CGRPs"


Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>% left_join(MIG_Drug_Histories_Triptans, by=c("patient"="patient")) %>% 
  left_join(MIG_Drug_Histories_CGRPs, by=c("patient"="patient")) %>% select(-c(weight.y)) %>% replace(is.na(.), 0)


Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.mean(N_Triptans, as.numeric(weight)))

                                                         
Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.median(N_Triptans, as.numeric(weight)))

                                                         
Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.mean(N_CGRPs, as.numeric(weight)))

                                                         
Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.median(N_CGRPs, as.numeric(weight)))

                                                         

# Nr of preventives
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_group))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_group)

MIG_Drug_Histories_Preventives <- MIG_Drug_Histories %>% filter(drug_group=="Preventative")

MIG_Drug_Histories_Preventives <- MIG_Drug_Histories_Preventives %>% distinct()

MIG_Drug_Histories_Preventives <- MIG_Drug_Histories_Preventives %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories_Preventives <- MIG_Drug_Histories_Preventives %>% select(patient, weight, grp)
MIG_Drug_Histories_Preventives <- MIG_Drug_Histories_Preventives %>% group_by(patient, weight) %>% summarize(across(everything(), max))
names(MIG_Drug_Histories_Preventives)[3] <- "N_Preventives"


Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>% left_join(MIG_Drug_Histories_Preventives, by=c("patient"="patient")) %>% select(-c(weight.y))
Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>% select(-c(weight.x.x))
Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>%  replace(is.na(.), 0)


Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.mean(N_Preventives, as.numeric(weight.x)))

                                                         
Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.median(N_Preventives, as.numeric(weight.x)))

                                                         




# Nr of Acutes
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_group))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_group)

MIG_Drug_Histories_Acutes <- MIG_Drug_Histories %>% filter(drug_group=="Acute")

MIG_Drug_Histories_Acutes <- MIG_Drug_Histories_Acutes %>% distinct()

MIG_Drug_Histories_Acutes <- MIG_Drug_Histories_Acutes %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories_Acutes <- MIG_Drug_Histories_Acutes %>% select(patient, weight, grp)
MIG_Drug_Histories_Acutes <- MIG_Drug_Histories_Acutes %>% group_by(patient, weight) %>% summarize(across(everything(), max))
names(MIG_Drug_Histories_Acutes)[3] <- "N_Acutes"


Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>% left_join(MIG_Drug_Histories_Acutes, by=c("patient"="patient")) %>% select(-c(weight))
Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>%  replace(is.na(.), 0)


Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.mean(N_Acutes, as.numeric(weight.x)))

                                                         
Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.median(N_Acutes, as.numeric(weight.x)))

                                                         



# Nr of Symptomatics
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_group))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_group)

MIG_Drug_Histories_Symptomatics <- MIG_Drug_Histories %>% filter(drug_group=="Symptomatic")

MIG_Drug_Histories_Symptomatics <- MIG_Drug_Histories_Symptomatics %>% distinct()

MIG_Drug_Histories_Symptomatics <- MIG_Drug_Histories_Symptomatics %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories_Symptomatics <- MIG_Drug_Histories_Symptomatics %>% select(patient, weight, grp)
MIG_Drug_Histories_Symptomatics <- MIG_Drug_Histories_Symptomatics %>% group_by(patient, weight) %>% summarize(across(everything(), max))
names(MIG_Drug_Histories_Symptomatics)[3] <- "N_Symptomatics"


Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>% left_join(MIG_Drug_Histories_Symptomatics, by=c("patient"="patient")) %>% select(-c(weight))
Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>%  replace(is.na(.), 0)


Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.mean(N_Symptomatics, as.numeric(weight.x)))

                                                         
Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.median(N_Symptomatics, as.numeric(weight.x)))

                                                         


# Duration ON Triptans 
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(70{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(70{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(71{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(71{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(72{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(72{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(73{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(73{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(74{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(74{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(75{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(75{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(76{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(76{1})(\\D|$)', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

Triptan_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Triptan_Periods_MIG)[3] <- "Duration"

Triptan_Periods_MIG <- Triptan_Periods_MIG %>% left_join(MIG_Drug_Histories %>% select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct()


Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>% left_join(Triptan_Periods_MIG, by=c("patient"="patient")) %>% select(-c(weight))
Rimegepant_Periods_MIG_3 <- Rimegepant_Periods_MIG_3 %>%  replace(is.na(.), 0)



Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.mean(Total_duration, as.numeric(weight.x)))

                                                         
Rimegepant_Periods_MIG_3 %>% group_by(Duration_bucket) %>% summarise(n=weighted.median(Total_duration, as.numeric(weight.x)))

                                                         

# ----
# Pats Injectable CGRP Duration vs Nr Lines at start ---------------------

MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month1", "1")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month2", "2")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month3", "3")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month4", "4")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month5", "5")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month6", "6")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month7", "7")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month8", "8")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month9", "9")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month10", "10")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month11", "11")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month12", "12")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month13", "13")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month14", "14")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month15", "15")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month16", "16")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month17", "17")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month18", "18")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month19", "19")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month20", "20")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month21", "21")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month22", "22")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month23", "23")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month24", "24")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month25", "25")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month26", "26")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month27", "27")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month28", "28")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month29", "29")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month30", "30")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month31", "31")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month32", "32")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month33", "33")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month34", "34")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month35", "35")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month36", "36")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month37", "37")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month38", "38")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month39", "39")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month40", "40")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month41", "41")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month42", "42")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month43", "43")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month44", "44")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month45", "45")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month46", "46")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month47", "47")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month48", "48")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month49", "49")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month50", "50")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month51", "51")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month52", "52")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month53", "53")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month54", "54")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month55", "55")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month56", "56")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month57", "57")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month58", "58")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month59", "59")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month60", "60")

MIG_nrLines_Histories$Month <- as.numeric(MIG_nrLines_Histories$Month)


# Durations on Inj CGRP
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(137{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(137{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(138{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(138{1})(\\D|$)', .), "CGRP Injectable"))%>%
  mutate_if(grepl('(^|\\D)(139{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(139{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(140{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(140{1})(\\D|$)', .), "CGRP Injectable"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Injectable",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month1", "1")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month2", "2")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month3", "3")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month4", "4")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month5", "5")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month6", "6")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month7", "7")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month8", "8")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month9", "9")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month10", "10")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month11", "11")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month12", "12")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month13", "13")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month14", "14")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month15", "15")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month16", "16")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month17", "17")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month18", "18")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month19", "19")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month20", "20")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month21", "21")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month22", "22")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month23", "23")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month24", "24")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month25", "25")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month26", "26")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month27", "27")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month28", "28")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month29", "29")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month30", "30")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month31", "31")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month32", "32")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month33", "33")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month34", "34")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month35", "35")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month36", "36")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month37", "37")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month38", "38")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month39", "39")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month40", "40")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month41", "41")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month42", "42")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month43", "43")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month44", "44")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month45", "45")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month46", "46")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month47", "47")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month48", "48")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month49", "49")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month50", "50")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month51", "51")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month52", "52")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month53", "53")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month54", "54")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month55", "55")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month56", "56")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month57", "57")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month58", "58")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month59", "59")
MIG_Drug_Histories$Month <- str_replace(MIG_Drug_Histories$Month, "month60", "60")

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

Month_First_start <- MIG_Drug_Histories %>% group_by(patient) %>% slice(1)
Month_First_start <- Month_First_start %>% select(patient, Month)
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% inner_join(Month_First_start) %>% select(-c(disease))

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

CGRP_Injectable_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(CGRP_Injectable_Periods_MIG)[3] <- "Duration"

CGRP_Injectable_Periods_MIG_VIZ <- CGRP_Injectable_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1)

CGRP_Injectable_Periods_MIG_VIZ <- CGRP_Injectable_Periods_MIG_VIZ %>% full_join(MIG_nrLines_Histories, by=c("patient"="patient")) %>% select(-c(weight.y))

CGRP_Injectable_Periods_MIG_VIZ <- CGRP_Injectable_Periods_MIG_VIZ %>% mutate(percent_treat = Total_duration/visibility)

CGRP_Injectable_Periods_MIG_VIZ %>% mutate(Treat = as.numeric(Treat)) %>%
  mutate(percent_treat = percent_treat*100)%>%
  ggplot(aes(Treat, percent_treat))+
  geom_point(colour = "deepskyblue4", alpha=0.5)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n# Therapy Lines at Start of Injectable CGRP")+
  ylab("% of Time ON Treatment\n(Months of Treatment / Visibility)\n")


CGRP_Injectable_Periods_MIG_VIZ %>% mutate(Treat = as.numeric(Treat)) %>%
  ggplot(aes(Treat, Total_duration))+
  geom_jitter(colour = "deepskyblue4", alpha=0.5)+
  geom_smooth(colour="firebrick", fill="firebrick")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\n# Therapy Lines at Start of Injectable CGRP")+
  ylab("Total Duration ON Treatment\n")



Nr_lines_vs_Duration <- CGRP_Injectable_Periods_MIG_VIZ %>% mutate(Treat = as.numeric(Treat)) %>% filter(Treat<=20) %>%
  select(patient, weight.x, Treat, Total_duration) %>% distinct() %>% 
  group_by(Treat, Total_duration) %>% summarise(total= sum(weight.x)) %>%
  spread(key = Treat, value = total)

write.csv(Nr_lines_vs_Duration, "Nr_lines_vs_Duration.csv")




Nr_lines_vs_Duration_all_lines <- CGRP_Injectable_Periods_MIG_VIZ %>% mutate(Treat = as.numeric(Treat)) %>% 
  select(patient, weight.x, Treat, Total_duration) %>% distinct() %>% 
  group_by(Treat, Total_duration) %>% summarise(total= sum(weight.x)) %>%
  spread(key = Treat, value = total)

write.csv(Nr_lines_vs_Duration_all_lines, "Nr_lines_vs_Duration_all_lines.csv")

# ------
# Stocks over time --------------------------

stocks_summary_over_time <-read.csv("stocks_summary_over_time.csv")

stocks_summary_over_time <-stocks_summary_over_time %>% filter(type=="pats") %>% 
  select(entity, M01, M24, M36, M60)

# ----

# Durations on Oral CGRP vs preventive status -------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(135{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(135{1})(\\D|$)', .), "CGRP Oral"))%>% 
  mutate_if(grepl('(^|\\D)(136{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(136{1})(\\D|$)', .), "CGRP oral"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Oral",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)


MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

#MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Month_First_start <- MIG_Drug_Histories %>% group_by(patient) %>% slice(1)
Month_First_start <- Month_First_start %>% select(patient, Month)
names(Month_First_start)[2] <- "Month_Start"

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) 

CGRP_Oral_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(CGRP_Oral_Periods_MIG)[3] <- "Duration"

CGRP_Oral_Periods_MIG_VIZ <- CGRP_Oral_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                   select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct() 


CGRP_Oral_Periods_MIG_VIZ <- CGRP_Oral_Periods_MIG_VIZ %>% left_join(Month_First_start)





MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(2, 4:63)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

CGRP_Oral_Periods_MIG_VIZ <- CGRP_Oral_Periods_MIG_VIZ %>% left_join(MIG_Drug_Histories, by=c("patient"="patient", "Month_Start"="Month"))

# Create Look-ups
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
string_Preventative <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Preventative"], collapse = "|"),")\\b")


CGRP_Oral_Periods_MIG_VIZ <- CGRP_Oral_Periods_MIG_VIZ %>% mutate(preventive=ifelse(str_detect(Treat, string_Preventative),"Preventive","NO_Prev"))

CGRP_Oral_Periods_MIG_VIZ %>% group_by(preventive) %>% summarise(n=weighted.mean(Total_duration, weight))


CGRP_Oral_Periods_MIG_VIZ %>% group_by(preventive) %>% summarise(n=weighted.mean(Total_duration, weight))

# -----
# Durations on Injectable CGRP vs preventive status -------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(137{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(137{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(138{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(138{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(139{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(139{1})(\\D|$)', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('(^|\\D)(140{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(140{1})(\\D|$)', .), "CGRP Injectable"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Injectable",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

Month_First_start <- MIG_Drug_Histories %>% group_by(patient) %>% slice(1)
Month_First_start <- Month_First_start %>% select(patient, Month)
names(Month_First_start)[2] <- "Month_Start"

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

CGRP_Injectable_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(CGRP_Injectable_Periods_MIG)[3] <- "Duration"

CGRP_Injectable_Periods_MIG_VIZ <- CGRP_Injectable_Periods_MIG %>% left_join(MIG_Drug_Histories %>% 
                                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1)


CCGRP_Injectable_Periods_MIG_VIZ <- CGRP_Injectable_Periods_MIG_VIZ %>% left_join(Month_First_start)


MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(2, 4:63)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

CCGRP_Injectable_Periods_MIG_VIZ <- CCGRP_Injectable_Periods_MIG_VIZ %>% left_join(MIG_Drug_Histories, by=c("patient"="patient", "Month_Start"="Month"))


RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
string_Preventative <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Preventative"], collapse = "|"),")\\b")


CCGRP_Injectable_Periods_MIG_VIZ <- CCGRP_Injectable_Periods_MIG_VIZ %>% mutate(preventive=ifelse(str_detect(Treat, string_Preventative),"Preventive","NO_Prev"))

CCGRP_Injectable_Periods_MIG_VIZ %>% group_by(preventive) %>% summarise(n=weighted.median(Total_duration, weight))


CCGRP_Injectable_Periods_MIG_VIZ %>% group_by(preventive) %>% summarise(n=weighted.mean(Total_duration, weight))

# ----
# Share of CGRPs Year 4 to year 5 by NEW/OLD Physicians -----------------------------------------------------------------
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, dayssup, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))

#year4
MIG_Doses_BIG_Year4 <- MIG_Doses_BIG %>%filter(from_dt >= "2019-08-01" & from_dt <= "2020-07-31") 
Physicians_Year4 <- MIG_Doses_BIG_Year4 %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") %>% select(prov_unique) %>% distinct() #3432
MIG_Doses_BIG_Year4 <- MIG_Doses_BIG_Year4 %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") #30071

#year5
MIG_Doses_BIG_Year5 <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 
Physicians_Year5 <- MIG_Doses_BIG_Year5 %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") %>% select(prov_unique) %>% distinct() #4716
MIG_Doses_BIG_Year5 <-MIG_Doses_BIG_Year5 %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") #43816

#all of them, plus number of scripts in each year
Summary_Year4to5 <- MIG_Doses_BIG_Year4 %>% select(prov_unique) %>% group_by(prov_unique) %>% summarise(n=n()) %>% rename(year4=n) %>%
  full_join(MIG_Doses_BIG_Year5 %>% select(prov_unique) %>% group_by(prov_unique) %>% summarise(n2=n()) %>% rename(year5=n2)) %>%
  replace(is.na(.), 0) %>% filter(prov_unique!="")%>% mutate(Diff_4to5=year5-year4) %>%
  mutate(Physician_type=ifelse(year4==0, "NEW",
                               ifelse(year5==0, "STOP",
                                      ifelse(year5>year4, "INCREASE",
                                             ifelse(year4>year5, "DECREASE", 
                                                    ifelse(year4==year5, "MAINTAIN", "NONE"))))))

sum(Summary_Year4to5$year4) #
sum(Summary_Year4to5$year5) #
sum(Summary_Year4to5$Diff_4to5) #

Summary_Year4to5 %>% filter(year4==0) %>% summarise(n=sum(Diff_4to5)) #
Summary_Year4to5 %>% filter(year5==0) %>% summarise(n=sum(Diff_4to5)) #-
Summary_Year4to5 %>% filter(year4!=0)  %>% filter(year5!=0) %>% filter(year5>year4) %>% summarise(n=sum(Diff_4to5)) #12137
Summary_Year4to5 %>% filter(year4!=0)  %>% filter(year5!=0) %>% filter(year5<year4) %>% summarise(n=sum(Diff_4to5)) #-4515

Summary_Year4to5 %>% group_by(Physician_type) %>% summarise(n=sum(Diff_4to5))
Summary_Year4to5 %>% group_by(Physician_type) %>% count()

Summary_Year4to5 %>% filter(year4!=0) %>% select(prov_unique, Physician_type) %>% left_join(MIG_Doses_BIG_Year4) %>%
  select(prov_unique, pat_id) %>% distinct() %>% group_by(prov_unique) %>% summarise(n=n()) %>% summarise(n2=mean(n)) #1.59

Summary_Year4to5 %>% filter(year4!=0) %>% select(prov_unique, Physician_type) %>% left_join(MIG_Doses_BIG_Year4) %>%
  select(Physician_type, prov_unique, pat_id) %>% distinct() %>% group_by(Physician_type, prov_unique) %>% summarise(n=n()) %>% summarise(n2=mean(n))
# 


Summary_Year4to5 %>% filter(year5!=0) %>% select(prov_unique, Physician_type) %>% left_join(MIG_Doses_BIG_Year5) %>%
  select(prov_unique, pat_id) %>% distinct() %>% group_by(prov_unique) %>% summarise(n=n()) %>% summarise(n2=mean(n)) #1.65


Summary_Year4to5 %>% filter(year5!=0) %>% select(prov_unique, Physician_type) %>% left_join(MIG_Doses_BIG_Year5) %>%
  select(Physician_type, prov_unique, pat_id) %>% distinct() %>% group_by(Physician_type, prov_unique) %>% summarise(n=n()) %>% summarise(n2=mean(n)) 


# -----
# Share of CGRPs of All CGRPs Physicians prescribe -----------------------------------------------------------------
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, dayssup, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))

MIG_Doses_BIG_Last_Year <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 
MIG_Doses_BIG_Last_Year <- MIG_Doses_BIG_Last_Year %>% filter(drug_group == "CGRP Injectable" | drug_group == "CGRP Oral") 

MIG_Doses_BIG_Last_Year <- MIG_Doses_BIG_Last_Year %>% group_by(prov_unique) %>% mutate(total_scripts=n()) %>% filter(total_scripts!=890)

MIG_Doses_BIG_Last_Year <- MIG_Doses_BIG_Last_Year %>% group_by(prov_unique, generic_name) %>% mutate(molecule_scripts=n())

# MIG_Doses_BIG_Last_Year %>% filter(generic_name=="Rimegepant") %>% select(prov_unique) %>% distinct() #492 doctors prescribing rimegepant last year

MIG_Doses_BIG_Last_Year_Sum <- MIG_Doses_BIG_Last_Year %>% ungroup() %>%select(generic_name, prov_unique, total_scripts, molecule_scripts) %>% distinct()

Rimegepant_Last_Year_Sum <- MIG_Doses_BIG_Last_Year_Sum %>% filter(generic_name=="Rimegepant")

Rimegepant_Last_Year_Sum <- Rimegepant_Last_Year_Sum %>% mutate(rimegepant_share = (molecule_scripts/total_scripts)*100) %>%
  mutate(bucket= ifelse(rimegepant_share <= 9, "0-9%",
                        ifelse(rimegepant_share <= 19, "10-19%",
                               ifelse(rimegepant_share <= 29, "20-29%",
                                      ifelse(rimegepant_share <= 39, "30-39%",
                                             ifelse(rimegepant_share <= 49, "40-49%",
                                                    ifelse(rimegepant_share <= 59, "50-59%",
                                                           ifelse(rimegepant_share <= 69, "60-69%",
                                                                  ifelse(rimegepant_share <= 79, "70-79%",
                                                                         ifelse(rimegepant_share <= 89, "80-89%", "+90%"))))))))))


Rimegepant_Last_Year_Sum %>% summarise(n=mean(rimegepant_share))
Rimegepant_Last_Year_Sum %>% group_by(bucket) %>% summarise(n=n()) %>% mutate(percent=(n/492)*100)
Rimegepant_Last_Year_Sum %>% group_by(bucket) %>% summarise(n=mean(molecule_scripts))


MIG_Doses_BIG_Last_Year %>% filter(generic_name=="Rimegepant") %>% left_join(Rimegepant_Last_Year_Sum, by=c("prov_unique"="prov_unique")) %>%
  group_by(bucket) %>% count() %>% mutate(share = (n /1486)*100)

Rimegepant_Last_Year_Sum %>% group_by(bucket) %>% summarise(n=mean(total_scripts))


# Repeat for physicians seeing more than 1 patient
Physicains_more_1pat <- Rimegepant_Last_Year_Sum %>% left_join(MIG_Doses_BIG_Last_Year, by=c("prov_unique"="prov_unique")) %>%
  select(prov_unique, pat_id) %>% distinct() %>% group_by(prov_unique) %>% count() %>% ungroup() %>% group_by(n) %>%
  ungroup() %>% filter(n!=1) %>% select(prov_unique)

Rimegepant_Last_Year_Sum_more1pat <- Physicains_more_1pat %>% left_join(Rimegepant_Last_Year_Sum)

Rimegepant_Last_Year_Sum_more1pat %>% summarise(n=mean(rimegepant_share))
Rimegepant_Last_Year_Sum_more1pat %>% group_by(bucket) %>% summarise(n=n()) %>% mutate(percent=(n/244)*100)
Rimegepant_Last_Year_Sum_more1pat %>% group_by(bucket) %>% summarise(n=mean(molecule_scripts))
Rimegepant_Last_Year_Sum_more1pat %>% group_by(bucket) %>% summarise(n=mean(total_scripts))

MIG_Doses_BIG_Last_Year %>% filter(generic_name=="Rimegepant") %>% left_join(Rimegepant_Last_Year_Sum_more1pat, by=c("prov_unique"="prov_unique")) %>%
  group_by(bucket) %>% count() %>% mutate(share = (n /894)*100)



# ----
# Repeat Segmentation with short preventive list  !  -----------------------------------------------------------------------
library("data.table")
library("tidyverse")


RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )

MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup() %>% 
  filter(!str_detect(Treat, "78|79|80|81|82|83|84|85|86|87|88|89|90|91|94|96|97|98|99|100|101|103|104|105|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122|123|124|125|126|127|128|129|130|131|133|134"))

MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup %>% group_by(patient, weight, Month) %>% mutate(Treat = paste(Treat, collapse=","))

MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup %>% group_by(patient, weight, Month) %>% distinct()

MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup %>% group_by(patient, weight) %>%
  spread(key = Month, value = Treat)

MIG_Drug_Histories <- MIG_Drug_Histories %>% replace(is.na(.), "-")

MIG_Drug_Histories_newPrv <- MIG_Drug_Histories

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient)

MIG_Drug_Histories_newPrv <- MIG_Drug_Histories %>% left_join(MIG_Drug_Histories_newPrv)

MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv %>% replace(is.na(.), "-")

fwrite(MIG_Drug_Histories_newPrv, "MIG_Drug_Histories_newPrv.txt", sep="\t")



RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories_newPrv <- read.table("MIG_Drug_Histories_newPrv.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# 1.A # NEW STARTS. No Rx until the current month --------------
MIG_Drug_Histories_newPrv <- read.table("MIG_Drug_Histories_newPrv.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv[,3:62]

MIG_Drug_Histories_newPrv[MIG_Drug_Histories_newPrv=="-"]<-0
MIG_Drug_Histories_newPrv[MIG_Drug_Histories_newPrv!=0]<-1

MIG_Drug_Histories_newPrv[] <- lapply(MIG_Drug_Histories_newPrv, as.numeric)

no_Rx_till_current <- function(x) ifelse(x == 0, ifelse(cumsum(x==1)>=1, NA, "Lapsed_Till_Current"), NA)

MIG_Drug_Histories_newPrv_1A <- apply(MIG_Drug_Histories_newPrv,1,no_Rx_till_current)
MIG_Drug_Histories_newPrv_1A = t(MIG_Drug_Histories_newPrv_1A)

MIG_Drug_Histories_newPrv_1A <- as.data.frame(MIG_Drug_Histories_newPrv_1A)

# 1.B	# Lapsed in Dec-19 (month 41) for 6+ months, re-starting after Jan-20 (month 42) directly to Oral CGRP ----
MIG_Drug_Histories_newPrv <- read.table("MIG_Drug_Histories_newPrv.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv[,3:62]

MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv %>% 
  mutate(month42 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month42 != "-"  & month41 == "-" , "START", month42))%>%
  mutate(month43 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month43 != "-"   & month42 == "-", "START", month43))%>%
  mutate(month44 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month44 != "-"  & month43 == "-", "START", month44))%>%
  mutate(month45 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month45 != "-"  & month44 == "-", "START", month45))%>%
  mutate(month46 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month46 != "-"  & month45 == "-", "START", month46))%>%
  mutate(month47 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month47 != "-"  & month46 == "-", "START", month47))%>%
  mutate(month48 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month48 != "-"  & month47 == "-", "START", month48))%>%
  mutate(month49 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month49 != "-"  & month48 == "-", "START", month49))%>%
  mutate(month50 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month50 != "-"  & month49 == "-", "START", month50))%>%
  mutate(month51 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month51 != "-"  & month50 == "-", "START", month51))%>%
  mutate(month52 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month52 != "-"  & month51 == "-", "START", month52))%>%
  mutate(month53 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month53 != "-"  & month52 == "-", "START", month53))%>%
  mutate(month54 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month54 != "-"  & month53 == "-", "START", month54))%>%
  mutate(month55 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month55 != "-"  & month54 == "-", "START", month55))%>%
  mutate(month56 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month56 != "-"  & month55 == "-", "START", month56))%>%
  mutate(month57 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month57 != "-"  & month56 == "-", "START", month57))%>%
  mutate(month58 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month58 != "-"  & month57 == "-", "START", month58))%>%
  mutate(month59 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month59 != "-"  & month58 == "-", "START", month59))%>%
  mutate(month60 = ifelse(month36 == "-" & month37 == "-" & month38 == "-" & month39 == "-" & month40 == "-"  & month41 == "-"  & month60 != "-"  & month59 == "-", "START", month60))

Oral_starts_m42 <- function(x) ifelse(cumsum(x=="START")>=1, "START_Post_Jan20", NA)
MIG_Drug_Histories_newPrv_1B <- apply(MIG_Drug_Histories_newPrv,1,Oral_starts_m42)
MIG_Drug_Histories_newPrv_1B = t(MIG_Drug_Histories_newPrv_1B)
MIG_Drug_Histories_newPrv_1B <- as.data.frame(MIG_Drug_Histories_newPrv_1B)

# 2.A # HIGH INTENSITY. Patients with at least one Rx of a CGRP Injectable previously ----------
MIG_Drug_Histories_newPrv <- read.table("MIG_Drug_Histories_newPrv.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv[,3:62]

string_CGRP_Inj <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")

MIG_Drug_Histories_newPrv[] <- lapply(MIG_Drug_Histories_newPrv, function(x) ifelse(str_detect(x,string_CGRP_Inj),1,0))

Inj_CGRP_previously <- function(x) ifelse(x == 0, ifelse(cumsum(x==1)>=1, "CGRP_Inj", NA), "CGRP_Inj")

MIG_Drug_Histories_newPrv_2A <- apply(MIG_Drug_Histories_newPrv,1,Inj_CGRP_previously)
MIG_Drug_Histories_newPrv_2A = t(MIG_Drug_Histories_newPrv_2A)
MIG_Drug_Histories_newPrv_2A <- as.data.frame(MIG_Drug_Histories_newPrv_2A)

# 2.B # HIGH SEVERETY Patients with at least one Rx of a CGRP Oral previously ---------------
MIG_Drug_Histories_newPrv <- read.table("MIG_Drug_Histories_newPrv.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv[,3:62]

string_CGRP_Oral <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "CGRP Oral"], collapse = "|"),")\\b")

MIG_Drug_Histories_newPrv[] <- lapply(MIG_Drug_Histories_newPrv, function(x) ifelse(str_detect(x,string_CGRP_Oral),1,0))

Oral_CGRP_previously <- function(x) ifelse(x == 0, ifelse(cumsum(x==1)>=1, "CGRP_Oral", NA), "CGRP_Oral")

MIG_Drug_Histories_newPrv_2B <- apply(MIG_Drug_Histories_newPrv,1,Oral_CGRP_previously)
MIG_Drug_Histories_newPrv_2B = t(MIG_Drug_Histories_newPrv_2B)
MIG_Drug_Histories_newPrv_2B <- as.data.frame(MIG_Drug_Histories_newPrv_2B)

# 3. # LOW SEVERITY - Patients with no history of preventive OR CGRPs drugs------
MIG_Drug_Histories_newPrv <- read.table("MIG_Drug_Histories_newPrv.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv[,3:62]

string_Preventative_AND_CGRP <- "\\b(92|93|95|102|106|132|135|136|137|138|139|140)\\b"

MIG_Drug_Histories_newPrv[] <- lapply(MIG_Drug_Histories_newPrv, function(x) ifelse(str_detect(x,string_Preventative_AND_CGRP),1,0))

Preventative_AND_CGRP_previously <- function(x) ifelse(x == 0, ifelse(cumsum(x==1)==0, "Low_Severity", NA), NA)

MIG_Drug_Histories_newPrv_3 <- apply(MIG_Drug_Histories_newPrv,1,Preventative_AND_CGRP_previously)
MIG_Drug_Histories_newPrv_3 = t(MIG_Drug_Histories_newPrv_3)
MIG_Drug_Histories_newPrv_3 <- as.data.frame(MIG_Drug_Histories_newPrv_3)

# 4A. # MODERATE SEVERITY-Patients currently on Preventive + Acute or Preventive + Symptomatic which are not on any of the other groups-----
MIG_Drug_Histories_newPrv <- read.table("MIG_Drug_Histories_newPrv.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv[,3:62]

string_sympt_acute <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Symptomatic" | RIME_Ingredients$drug_group == "Acute"], collapse = "|"),")\\b")

MIG_Drug_Histories_newPrv[] <- lapply(MIG_Drug_Histories_newPrv, function(x) ifelse(str_detect(x,"\\b(92|93|95|102|106|132)\\b") & str_detect(x,string_sympt_acute),1,0))

current_combo_prev <- function(x) ifelse(x == 1, "Combo_Preventive", NA)

MIG_Drug_Histories_newPrv_4A <- apply(MIG_Drug_Histories_newPrv,1,current_combo_prev)
MIG_Drug_Histories_newPrv_4A = t(MIG_Drug_Histories_newPrv_4A)
MIG_Drug_Histories_newPrv_4A <- as.data.frame(MIG_Drug_Histories_newPrv_4A)



# ---------

MIG_Drug_Histories_newPrv <- read.table("MIG_Drug_Histories_newPrv.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_newPrv <- MIG_Drug_Histories_newPrv %>% select(patient)

MIG_Drug_Histories_newPrv_1A <- MIG_Drug_Histories_newPrv %>% bind_cols(MIG_Drug_Histories_newPrv_1A)
MIG_Drug_Histories_newPrv_1B <- MIG_Drug_Histories_newPrv %>% bind_cols(MIG_Drug_Histories_newPrv_1B)
MIG_Drug_Histories_newPrv_2A <- MIG_Drug_Histories_newPrv %>% bind_cols(MIG_Drug_Histories_newPrv_2A)
MIG_Drug_Histories_newPrv_2B <- MIG_Drug_Histories_newPrv %>% bind_cols(MIG_Drug_Histories_newPrv_2B)
MIG_Drug_Histories_newPrv_3 <- MIG_Drug_Histories_newPrv %>% bind_cols(MIG_Drug_Histories_newPrv_3)
MIG_Drug_Histories_newPrv_4A <- MIG_Drug_Histories_newPrv %>% bind_cols(MIG_Drug_Histories_newPrv_4A)


temp <- MIG_Drug_Histories_newPrv_1A %>% bind_rows(MIG_Drug_Histories_newPrv_1B) %>% bind_rows(MIG_Drug_Histories_newPrv_2A) %>%
  bind_rows(MIG_Drug_Histories_newPrv_2B) %>% bind_rows(MIG_Drug_Histories_newPrv_3) %>% bind_rows(MIG_Drug_Histories_newPrv_4A) %>%
  arrange(patient)

temp[] <- lapply(temp, as.character)

temp[2:61][is.na(temp[2:61])] <- 0

temp[temp=="Lapsed_Till_Current"]<-6
temp[temp=="START_Post_Jan20"]<-5
temp[temp=="CGRP_Inj"]<-4
temp[temp=="CGRP_Oral"]<-3
temp[temp=="Low_Severity"]<-2
temp[temp=="Combo_Preventive"]<-1

temp2 <- temp %>% group_by(patient) %>% summarise(across(everything(),max))

temp2 %>% group_by(month60) %>% summarise(n=n())


fwrite(temp2,"Decision_resourcing_boxes_temp2.txt", sep="\t")



# Segmentation Classification Boxes ----------------------
Decision_resourcing_boxes <- read.table("Decision_resourcing_boxes_temp2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)


Decision_resourcing_boxes <- gather(Decision_resourcing_boxes, Month, Treat, month1:month60, factor_key=TRUE)

Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month1", "1")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month2", "2")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month3", "3")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month4", "4")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month5", "5")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month6", "6")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month7", "7")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month8", "8")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month9", "9")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month10", "10")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month11", "11")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month12", "12")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month13", "13")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month14", "14")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month15", "15")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month16", "16")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month17", "17")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month18", "18")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month19", "19")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month20", "20")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month21", "21")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month22", "22")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month23", "23")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month24", "24")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month25", "25")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month26", "26")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month27", "27")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month28", "28")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month29", "29")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month30", "30")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month31", "31")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month32", "32")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month33", "33")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month34", "34")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month35", "35")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month36", "36")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month37", "37")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month38", "38")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month39", "39")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month40", "40")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month41", "41")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month42", "42")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month43", "43")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month44", "44")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month45", "45")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month46", "46")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month47", "47")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month48", "48")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month49", "49")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month50", "50")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month51", "51")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month52", "52")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month53", "53")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month54", "54")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month55", "55")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month56", "56")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month57", "57")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month58", "58")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month59", "59")
Decision_resourcing_boxes$Month <- str_replace(Decision_resourcing_boxes$Month, "month60", "60")

"Lapsed_Till_Current"<-6
"START_Post_Jan20"<-5
"CGRP_Inj"<-4
"CGRP_Oral"<-3
"Low_Severity"<-2
"Combo_Preventive"<-1
"Other (mod sev, 4b)"<-0

# Classification before and after Rimegepant Start
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2=as.numeric(p2))

#pats with Rimegepant after m48
MIG_Rimegepant_After_48 <- MIG_Flows_Aux._Long %>% filter(p1 >=48)%>%
  filter(grepl("135",d2)) %>% select(patient) %>% distinct()
#pats with Rimegepant before m48
MIG_Rimegepant_Before_48 <- MIG_Flows_Aux._Long %>% filter(p1 < 48) %>%
  filter(grepl("135",d2) | grepl("135",d1)) %>% select(patient) %>% distinct()
#pats withRimegepant after m48 but not before m48 (i.e. naive)
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% anti_join(MIG_Rimegepant_Before_48)
rm(MIG_Rimegepant_Before_48)
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% left_join(MIG_Flows_Aux._Long)

# first occurence of Rimegepant, month before and after as well
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% group_by(patient) %>% filter(grepl("135",d2)) %>% slice(1) %>%
  select(patient, weight, p2) %>% mutate(before=p2-1)
# collpase month to single column
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% select(-c(weight)) %>% pivot_longer(!patient,  names_to = "month", values_to = "n") %>% select(-c(month))
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% arrange(patient, n)
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% group_by(patient) %>% filter(n==min(n))
MIG_Rimegepant_After_48$n <- as.character(MIG_Rimegepant_After_48$n)

MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% left_join(Decision_resourcing_boxes) %>% filter(n==Month) %>% select(-Month)
names(MIG_Rimegepant_After_48)[3] <- "Segmentation"
MIG_Rimegepant_After_48 <- MIG_Rimegepant_After_48 %>% left_join(MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct())

MIG_Rimegepant_After_48 %>% group_by(Segmentation) %>% summarise(n=sum(as.numeric(weight)))


                                      
# ----
# FLows to Rimegepant with Cardiovascular comorbidities -----
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p1 >=48)




MIG_Comorbidities <- read.table("MIG Comorbidities.txt", header = T, sep="\t", 
                                colClasses = "character", stringsAsFactors = FALSE)

MIG_Comorbidities <- MIG_Comorbidities %>% select(2,21:24)


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% left_join(MIG_Comorbidities)


MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("135",d1)) %>% 
  filter(grepl("135",d2)) %>% group_by(s1, cardiovascular_comorbidity) %>% summarise(pats=sum(as.numeric(weight)))
# #Inflows

                                      
MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(grepl("135",d1)) %>% 
  filter(!grepl("135",d2)) %>%  group_by(s2, cardiovascular_comorbidity) %>% summarise(pats=sum(as.numeric(weight)))

                                      
# ----
# Pills per month Rimegepant Q1, Q2, Q3, Q4 --------------------------------------------------
# (from_dt >= "2020-08-01" & from_dt <= "2020-10-31")
# (from_dt >= "2020-11-01" & from_dt < "2021-01-31")
# (from_dt >= "2021-02-01" & from_dt <= "2021-04-31")
# (from_dt >= "2021-05-01" & from_dt <= "2021-07-31")


MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(prov_unique, prov_type, specialty, taxonomy1, taxonomy2, status, drug_group, drug_class, drug_id))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))

MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2020-10-31")
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(generic_name=="Rimegepant")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(dayssup, generic_name))

MIG_Doses_BIG <- MIG_Doses_BIG %>% group_by(pat_id) %>% 
  mutate(lapsed_time = max(interval(from_dt, "2020-10-31") %/% months(1)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(months = format(as.Date(from_dt), "%Y-%m")) %>%
  mutate(N_months_treat = length(unique(months)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(lapsed_time = ifelse(lapsed_time==0,1,lapsed_time))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month = (max(row_number())*8)/lapsed_time)

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month_treated = (max(row_number())*8)/N_months_treat)

MIG_Doses_BIG <- MIG_Doses_BIG %>% ungroup() %>% select(pat_id, weight, lapsed_time, N_months_treat, pils_per_month, pils_per_month_treated) %>% distinct()

Q1 <- data.frame(MIG_Doses_BIG %>% ungroup() %>% mutate(bucket = round(pils_per_month_treated))%>% 
                   group_by(N_months_treat, bucket) %>% summarise(n=sum(as.numeric(weight))))


MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(prov_unique, prov_type, specialty, taxonomy1, taxonomy2, status, drug_group, drug_class, drug_id))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))

MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-11-01" & from_dt <= "2021-01-31")
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(generic_name=="Rimegepant")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(dayssup, generic_name))

MIG_Doses_BIG <- MIG_Doses_BIG %>% group_by(pat_id) %>% 
  mutate(lapsed_time = max(interval(from_dt, "2021-01-31") %/% months(1)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(months = format(as.Date(from_dt), "%Y-%m")) %>%
  mutate(N_months_treat = length(unique(months)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(lapsed_time = ifelse(lapsed_time==0,1,lapsed_time))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month = (max(row_number())*8)/lapsed_time)

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month_treated = (max(row_number())*8)/N_months_treat)

MIG_Doses_BIG <- MIG_Doses_BIG %>% ungroup() %>% select(pat_id, weight, lapsed_time, N_months_treat, pils_per_month, pils_per_month_treated) %>% distinct()

Q2 <- data.frame(MIG_Doses_BIG %>% ungroup() %>% mutate(bucket = round(pils_per_month_treated))%>% 
                   group_by(N_months_treat, bucket) %>% summarise(n=sum(as.numeric(weight))))




MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(prov_unique, prov_type, specialty, taxonomy1, taxonomy2, status, drug_group, drug_class, drug_id))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))

MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2021-02-01" & from_dt < "2021-05-01")
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(generic_name=="Rimegepant")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(dayssup, generic_name))

MIG_Doses_BIG <- MIG_Doses_BIG %>% group_by(pat_id) %>% 
  mutate(lapsed_time = max(interval(from_dt, "2021-04-31") %/% months(1)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(months = format(as.Date(from_dt), "%Y-%m")) %>%
  mutate(N_months_treat = length(unique(months)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(lapsed_time = ifelse(lapsed_time==0,1,lapsed_time))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month = (max(row_number())*8)/lapsed_time)

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month_treated = (max(row_number())*8)/N_months_treat)

MIG_Doses_BIG <- MIG_Doses_BIG %>% ungroup() %>% select(pat_id, weight, lapsed_time, N_months_treat, pils_per_month, pils_per_month_treated) %>% distinct()

Q3 <- data.frame(MIG_Doses_BIG %>% ungroup() %>% mutate(bucket = round(pils_per_month_treated))%>% 
                   group_by(N_months_treat, bucket) %>% summarise(n=sum(as.numeric(weight))))





MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(prov_unique, prov_type, specialty, taxonomy1, taxonomy2, status, drug_group, drug_class, drug_id))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))

MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2021-05-01" & from_dt <= "2021-07-31")
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(generic_name=="Rimegepant")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(dayssup, generic_name))

MIG_Doses_BIG <- MIG_Doses_BIG %>% group_by(pat_id) %>% 
  mutate(lapsed_time = max(interval(from_dt, "2021-07-31") %/% months(1)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(months = format(as.Date(from_dt), "%Y-%m")) %>%
  mutate(N_months_treat = length(unique(months)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(lapsed_time = ifelse(lapsed_time==0,1,lapsed_time))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month = (max(row_number())*8)/lapsed_time)

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(pils_per_month_treated = (max(row_number())*8)/N_months_treat)

MIG_Doses_BIG <- MIG_Doses_BIG %>% ungroup() %>% select(pat_id, weight, lapsed_time, N_months_treat, pils_per_month, pils_per_month_treated) %>% distinct()

Q4 <- data.frame(MIG_Doses_BIG %>% ungroup() %>% mutate(bucket = round(pils_per_month_treated))%>% 
                   group_by(N_months_treat, bucket) %>% summarise(n=sum(as.numeric(weight))))





Q1 %>% bind_rows(Q2) %>% bind_rows(Q3) %>% bind_rows(Q4) %>% summarise(total=sum(n))

# ----
# How many patients in each stock across lines of therapy over time? 6 to 12 FILTERED Last12months ----------
# Import file
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month1", "1")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month2", "2")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month3", "3")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month4", "4")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month5", "5")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month6", "6")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month7", "7")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month8", "8")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month9", "9")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month10", "10")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month11", "11")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month12", "12")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month13", "13")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month14", "14")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month15", "15")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month16", "16")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month17", "17")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month18", "18")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month19", "19")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month20", "20")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month21", "21")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month22", "22")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month23", "23")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month24", "24")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month25", "25")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month26", "26")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month27", "27")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month28", "28")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month29", "29")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month30", "30")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month31", "31")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month32", "32")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month33", "33")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month34", "34")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month35", "35")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month36", "36")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month37", "37")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month38", "38")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month39", "39")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month40", "40")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month41", "41")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month42", "42")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month43", "43")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month44", "44")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month45", "45")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month46", "46")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month47", "47")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month48", "48")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month49", "49")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month50", "50")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month51", "51")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month52", "52")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month53", "53")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month54", "54")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month55", "55")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month56", "56")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month57", "57")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month58", "58")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month59", "59")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month60", "60")

MIG_nrLines_Histories_pats <- MIG_nrLines_Histories
MIG_nrLines_Histories_pats<- MIG_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 6 were on 0 Lines (lapsed/naive)
MIG_nrLines_Histories_pats_naive <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=6) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 6) %>% select(patient) %>% distinct()

#pats that after Month 6 were started on some line
MIG_nrLines_Histories_pats_start_6_to_12 <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >6 & Month <=12) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
MIG_nrLines_Histories_pats_track <- MIG_nrLines_Histories_pats_start_6_to_12  %>% inner_join(MIG_nrLines_Histories_pats_naive)

rm(MIG_nrLines_Histories_pats_start_6_to_12, MIG_nrLines_Histories_pats_naive, MIG_nrLines_Histories)

# lines of the selected patients
MIG_nrLines_Histories_pats <- MIG_nrLines_Histories_pats_track %>% left_join(MIG_nrLines_Histories_pats)
MIG_nrLines_Histories <- MIG_nrLines_Histories_pats
rm(MIG_nrLines_Histories_pats)
rm(MIG_nrLines_Histories_pats_track)
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Treat != "0")

###
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2)

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% left_join(MIG_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(MIG_nrLines_Histories)[5] <- "stock"

MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)

#recode line number
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>%  filter(Month_new > (max(Month_new)-12))

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

MIG_nrLines_Histories_SUMMARY <- data.frame(MIG_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

MIG_nrLines_Histories_SUMMARY <- MIG_nrLines_Histories_SUMMARY %>% arrange(stock, Treat, Month_new)

MIG_nrLines_Histories_SUMMARY$Index <- paste(MIG_nrLines_Histories_SUMMARY$stock, MIG_nrLines_Histories_SUMMARY$Treat)

MIG_nrLines_Histories_SUMMARY <- MIG_nrLines_Histories_SUMMARY %>% filter(stock=="O")

MIG_nrLines_Histories_SUMMARY %>% group_by(Treat) %>% summarise(n=sum(as.numeric(sum)))



# ----
# How many patients in each stock across lines of therapy over time? 24 to 36 FILTERED Last12months ----------
# Import file
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month1", "1")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month2", "2")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month3", "3")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month4", "4")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month5", "5")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month6", "6")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month7", "7")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month8", "8")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month9", "9")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month10", "10")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month11", "11")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month12", "12")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month13", "13")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month14", "14")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month15", "15")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month16", "16")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month17", "17")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month18", "18")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month19", "19")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month20", "20")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month21", "21")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month22", "22")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month23", "23")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month24", "24")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month25", "25")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month26", "26")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month27", "27")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month28", "28")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month29", "29")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month30", "30")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month31", "31")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month32", "32")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month33", "33")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month34", "34")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month35", "35")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month36", "36")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month37", "37")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month38", "38")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month39", "39")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month40", "40")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month41", "41")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month42", "42")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month43", "43")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month44", "44")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month45", "45")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month46", "46")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month47", "47")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month48", "48")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month49", "49")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month50", "50")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month51", "51")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month52", "52")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month53", "53")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month54", "54")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month55", "55")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month56", "56")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month57", "57")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month58", "58")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month59", "59")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month60", "60")

MIG_nrLines_Histories_pats <- MIG_nrLines_Histories
MIG_nrLines_Histories_pats<- MIG_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 24 were on 0 Lines (lapsed/naive)
MIG_nrLines_Histories_pats_naive <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=24) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 24) %>% select(patient) %>% distinct()

#pats that after Month 24 were started on some line
MIG_nrLines_Histories_pats_start_24_to_36 <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >24 & Month <=36) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
MIG_nrLines_Histories_pats_track <- MIG_nrLines_Histories_pats_start_24_to_36  %>% inner_join(MIG_nrLines_Histories_pats_naive)

rm(MIG_nrLines_Histories_pats_start_24_to_36, MIG_nrLines_Histories_pats_naive, MIG_nrLines_Histories)

# lines of the selected patients
MIG_nrLines_Histories_pats <- MIG_nrLines_Histories_pats_track %>% left_join(MIG_nrLines_Histories_pats)
MIG_nrLines_Histories <- MIG_nrLines_Histories_pats
rm(MIG_nrLines_Histories_pats)
rm(MIG_nrLines_Histories_pats_track)
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Treat != "0")

###
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2)

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% left_join(MIG_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(MIG_nrLines_Histories)[5] <- "stock"

MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)

#recode line number
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>%  filter(Month_new > (max(Month_new)-12))

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())


MIG_nrLines_Histories_SUMMARY_24_to_36 <- data.frame(MIG_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

MIG_nrLines_Histories_SUMMARY_24_to_36 <- MIG_nrLines_Histories_SUMMARY_24_to_36 %>% arrange(stock, Treat, Month_new)

MIG_nrLines_Histories_SUMMARY_24_to_36$Index <- paste(MIG_nrLines_Histories_SUMMARY_24_to_36$stock, MIG_nrLines_Histories_SUMMARY_24_to_36$Treat)

MIG_nrLines_Histories_SUMMARY_24_to_36 <-MIG_nrLines_Histories_SUMMARY_24_to_36 %>% filter(stock == "O")

MIG_nrLines_Histories_SUMMARY_24_to_36 %>% group_by(Treat) %>% summarise(n=sum(as.numeric(sum)))



# ----
# How many patients in each stock across lines of therapy over time? 36 to 48 FILTERED Last12months ----------
# Import file
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month1", "1")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month2", "2")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month3", "3")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month4", "4")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month5", "5")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month6", "6")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month7", "7")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month8", "8")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month9", "9")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month10", "10")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month11", "11")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month12", "12")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month13", "13")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month14", "14")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month15", "15")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month16", "16")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month17", "17")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month18", "18")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month19", "19")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month20", "20")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month21", "21")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month22", "22")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month23", "23")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month24", "24")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month25", "25")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month26", "26")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month27", "27")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month28", "28")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month29", "29")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month30", "30")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month31", "31")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month32", "32")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month33", "33")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month34", "34")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month35", "35")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month36", "36")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month37", "37")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month38", "38")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month39", "39")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month40", "40")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month41", "41")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month42", "42")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month43", "43")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month44", "44")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month45", "45")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month46", "46")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month47", "47")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month48", "48")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month49", "49")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month50", "50")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month51", "51")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month52", "52")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month53", "53")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month54", "54")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month55", "55")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month56", "56")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month57", "57")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month58", "58")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month59", "59")
MIG_nrLines_Histories$Month <- str_replace(MIG_nrLines_Histories$Month, "month60", "60")

MIG_nrLines_Histories_pats <- MIG_nrLines_Histories
MIG_nrLines_Histories_pats<- MIG_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 36 were on 0 Lines (lapsed/naive)
MIG_nrLines_Histories_pats_naive <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=36) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 36) %>% select(patient) %>% distinct()

#pats that after Month 36 were started on some line
MIG_nrLines_Histories_pats_start_36_to_48 <- MIG_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >36 & Month <=48) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()


#pats to track
MIG_nrLines_Histories_pats_track <- MIG_nrLines_Histories_pats_start_36_to_48  %>% inner_join(MIG_nrLines_Histories_pats_naive)

rm(MIG_nrLines_Histories_pats_start_36_to_48, MIG_nrLines_Histories_pats_naive, MIG_nrLines_Histories)

# lines of the selected patients
MIG_nrLines_Histories_pats <- MIG_nrLines_Histories_pats_track %>% left_join(MIG_nrLines_Histories_pats)
MIG_nrLines_Histories <- MIG_nrLines_Histories_pats
rm(MIG_nrLines_Histories_pats)
rm(MIG_nrLines_Histories_pats_track)
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% filter(Treat != "0")

###
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2)

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% left_join(MIG_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(MIG_nrLines_Histories)[5] <- "stock"

MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)

#recode line number
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))


MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>%  filter(Month_new > (max(Month_new)-12))

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

MIG_nrLines_Histories_SUMMARY_36_to_48 <- data.frame(MIG_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

MIG_nrLines_Histories_SUMMARY_36_to_48 <- MIG_nrLines_Histories_SUMMARY_36_to_48 %>% arrange(stock, Treat, Month_new)

MIG_nrLines_Histories_SUMMARY_36_to_48$Index <- paste(MIG_nrLines_Histories_SUMMARY_36_to_48$stock, MIG_nrLines_Histories_SUMMARY_36_to_48$Treat)


MIG_nrLines_Histories_SUMMARY_36_to_48 <- MIG_nrLines_Histories_SUMMARY_36_to_48 %>% filter(stock=="O")

MIG_nrLines_Histories_SUMMARY_36_to_48 %>% group_by(Treat) %>% summarise(n=sum(as.numeric(sum)))



# ----
# Water falltype of flows mutually exclusive ------------------------------------------------------------------------
# Prepare tabels with % CGRp Experienced and totla flows last year
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"
MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_class)
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories %>% filter(drug_class=="CGRP Oral" | drug_class=="CGRP Injectable" )
MIG_Drug_Histories_CGRPs <- MIG_Drug_Histories_CGRPs %>% distinct()
##### CGRP Experienced 
CGRP_Experienced <- MIG_Drug_Histories_CGRPs %>% select(patient) %>% distinct()


MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p1 >=48)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(stops !="1")

Total_Flows_Last_year <- MIG_Flows_Aux._Long %>% select(patient, weight, flow) %>% mutate(flow=as.numeric(flow)) %>% group_by(patient) %>%
  mutate(total_flows = sum(flow)) %>% select(patient, weight, total_flows) %>% distinct()


##### Values for Waterfal
# Injectable CGRP to Oral
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2=="O") %>% summarise(n=sum(as.numeric(weight))) #11473.87
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2=="O") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #11229.29
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2=="O") %>% select(patient, weight) %>% distinct() %>% 
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #11229.29 (100%)
Inj_to_Oral <- MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2=="O") %>% select(patient, weight) %>% distinct()
Inj_to_Oral %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #7.556027
pats_to_remove <- Inj_to_Oral


# Injectable CGRP Intraflows
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #1854127
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #439453.8
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #439453.8 (100%)
Inj_Intraflow <- MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Inj_Intraflow %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #6.352881
pats_to_remove <- pats_to_remove %>% bind_rows(Inj_Intraflow) %>% distinct()


# Injectable CGRP to No CGRPP
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2!="I" & s2!="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #69097.13
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2!="I" & s2!="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #62751.09
MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2!="I" & s2!="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #62751.09 (100%)
Inj_to_No_CGRP <- MIG_Flows_Aux._Long %>% filter(s1 == "I" & s2!="I" & s2!="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Inj_to_No_CGRP %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #5.436125
pats_to_remove <- pats_to_remove %>% bind_rows(Inj_to_No_CGRP) %>% distinct()


# Any preventive to Injectable CGRP 
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #28596.68
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #28596.68
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced) %>% summarise(n=sum(as.numeric(weight))) #28596.68 (100%)
Any_Prev_to_InjCGRP <- MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Any_Prev_to_InjCGRP %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #4.9919
pats_to_remove <- pats_to_remove %>% bind_rows(Any_Prev_to_InjCGRP) %>% distinct()


# Lapsed/Symptomatic/Acute to Injectable CGRP 
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #22686.96
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="I")%>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #21642.73
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="I") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #21642.73 (100%)
Any_Early_to_InjCGRP <- MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="I")  %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Any_Early_to_InjCGRP %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #2.876888
pats_to_remove <- pats_to_remove %>% bind_rows(Any_Early_to_InjCGRP) %>% distinct()


# Oral CGRP to no CGRP 
MIG_Flows_Aux._Long %>% filter(s1 == "O" & s2!="I" & s2!="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #71575.07
MIG_Flows_Aux._Long %>% filter(s1 == "O" & s2!="I" & s2!="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #60452.32
MIG_Flows_Aux._Long %>% filter(s1 == "O" & s2!="I" & s2!="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #60452.32 (100%)
Oral_CGRP_to_NO_CGRP <- MIG_Flows_Aux._Long %>% filter(s1 == "O" & s2!="I" & s2!="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Oral_CGRP_to_NO_CGRP %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #6.789356
pats_to_remove <- pats_to_remove %>% bind_rows(Oral_CGRP_to_NO_CGRP) %>% distinct()


# Any preventive to oral CGRP 
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #43611
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #43611
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #43611 (100%)
Any_Prev_to_oral_CGRP <- MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Any_Prev_to_oral_CGRP %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #5.916202
pats_to_remove <- pats_to_remove %>% bind_rows(Any_Prev_to_oral_CGRP) %>% distinct()

#  Lapsed/Symptomatic/Acute  to oral CGRP 
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #34742.42
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #31439.98
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #31439.98 (100%)
Any_Early_to_oral_CGRP <- MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="O") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Any_Early_to_oral_CGRP %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #3.15893
pats_to_remove <- pats_to_remove %>% bind_rows(Any_Early_to_oral_CGRP) %>% distinct()

# CGRP to Prev+Acute
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="D") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #0
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="D") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #0
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="D") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #0 (0%)
CGRP_to_PrevANDAcute <- MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="D")  %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
CGRP_to_PrevANDAcute %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #0
pats_to_remove <- pats_to_remove %>% bind_rows(CGRP_to_PrevANDAcute) %>% distinct()


# CGRP to Prev+Sympt
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="d") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #0
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="d") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #0
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="d") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #0 (0%)
CGRP_to_PrevANDSympt <- MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="d")  %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
CGRP_to_PrevANDSympt %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #0
pats_to_remove <- pats_to_remove %>% bind_rows(CGRP_to_PrevANDSympt) %>% distinct()


# CGRP to Acute
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="A") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #0
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="A") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #0
MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="A") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #0 (0%)
CGRP_to_Acute <- MIG_Flows_Aux._Long %>% filter((s1 == "O" | s1 == "I") & s2=="A")  %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
CGRP_to_Acute %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #0
pats_to_remove <- pats_to_remove %>% bind_rows(CGRP_to_Acute) %>% distinct()


# Any Prev to Prev+Acute
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="D") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #3862451
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="D") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #1647096
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="D") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #35385.85 (2%)
Any_Prev_to_PrevANDAcute <- MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="D")  %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Any_Prev_to_PrevANDAcute %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #5.497355
pats_to_remove <- pats_to_remove %>% bind_rows(Any_Prev_to_PrevANDAcute) %>% distinct()

# Any Prev to Prev+Symptomatic
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="d") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #11979663
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="d") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #4076652
MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="d") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #36722.74 (1%)
Any_Prev_to_PrevANDSympt <- MIG_Flows_Aux._Long %>% filter((s1 == "p" | s1 == "d" | s1 == "D") & s2=="d")  %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Any_Prev_to_PrevANDSympt %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #4.933582
pats_to_remove <- pats_to_remove %>% bind_rows(Any_Prev_to_PrevANDSympt) %>% distinct()

# Any Early to Prev+Acute
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="D") %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% summarise(n=sum(as.numeric(weight))) #314021.6
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="D")  %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>% 
  summarise(n=sum(as.numeric(weight))) #290207.9
MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="D")  %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct() %>%  
  inner_join(CGRP_Experienced)  %>% summarise(n=sum(as.numeric(weight))) #2899.44 (1%)
Any_Ealy_to_PrevANDAcute <- MIG_Flows_Aux._Long %>% filter((s1 == "x" | s1 == "a" | s1 == "A") & s2=="D")   %>% filter(flow=="1") %>% anti_join(pats_to_remove) %>% select(patient, weight) %>% distinct()
Any_Ealy_to_PrevANDAcute %>% left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #3.097007
pats_to_remove <- pats_to_remove %>% bind_rows(Any_Ealy_to_PrevANDAcute) %>% distinct()





# All Flows (exc STOPS)
MIG_Flows_Aux._Long %>% filter(flow=="1")  %>% summarise(n=sum(as.numeric(weight))) #47304014
MIG_Flows_Aux._Long %>% filter(flow=="1")  %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #13208163
MIG_Flows_Aux._Long %>% filter(flow=="1")  %>% select(patient, weight) %>% distinct() %>% inner_join(CGRP_Experienced) %>% summarise(n=sum(as.numeric(weight))) # (806949.5/13208163) 0.06109476
MIG_Flows_Aux._Long %>% filter(flow=="1")  %>% select(patient, weight) %>% distinct() %>%  left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) #3.581423

# All target Flows (pats maybe have had non target as well)
pats_to_remove %>% left_join(MIG_Flows_Aux._Long) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #6713133
pats_to_remove %>% left_join(MIG_Flows_Aux._Long) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #6713133
pats_to_remove %>% left_join(MIG_Flows_Aux._Long) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>% 
  inner_join(CGRP_Experienced) %>% summarise(n=sum(as.numeric(weight))) #0.1153239
pats_to_remove %>% left_join(MIG_Flows_Aux._Long) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>% 
  left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight)))  #5.102906

# Non target flows only
MIG_Flows_Aux._Long %>% anti_join(pats_to_remove) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #6495030
MIG_Flows_Aux._Long %>% anti_join(pats_to_remove) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>% 
  inner_join(CGRP_Experienced) %>% summarise(n=sum(as.numeric(weight))) #32764.56/6495030  0.005044559
MIG_Flows_Aux._Long %>% anti_join(pats_to_remove) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>% 
  left_join(Total_Flows_Last_year) %>% summarise(n=weighted.mean(total_flows, as.numeric(weight))) 









# % seen by neurologists 
Physicians_Vanguard_Lookup <- read.csv("Physicians_Vanguard_Lookup.csv", colClasses = "character", stringsAsFactors = FALSE)
removeQuotes <- function(x) gsub("\'", "", x)
Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% mutate_if(is.character, removeQuotes)
names(Physicians_Vanguard_Lookup)[1] <- "specialty"

MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 

Patients_with_Neurologist <- MIG_Doses_BIG %>% select(pat_id, specialty) %>% distinct() %>% left_join(Physicians_Vanguard_Lookup) %>%
  filter(Physician=="NEUROLOGIST")

Patients_with_Neurologist <- Patients_with_Neurologist %>% select(1,3) %>% distinct()


Inj_to_Oral %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))



Inj_Intraflow %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))


Inj_to_No_CGRP %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Any_Prev_to_InjCGRP %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))


Any_Early_to_InjCGRP %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Oral_CGRP_to_NO_CGRP %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Any_Prev_to_oral_CGRP %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Any_Early_to_oral_CGRP %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Any_Prev_to_PrevANDAcute %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Any_Prev_to_PrevANDSympt %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Any_Ealy_to_PrevANDAcute %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))



# ALL
MIG_Flows_Aux._Long %>% filter(flow=="1")  %>% select(patient, weight) %>% distinct() %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))
#TARGET
pats_to_remove %>% left_join(MIG_Flows_Aux._Long) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>%  left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

#NON TARGET
MIG_Flows_Aux._Long %>% anti_join(pats_to_remove) %>% filter(flow=="1") %>% select(patient, weight) %>% distinct() %>% left_join(Patients_with_Neurologist, by=c("patient"="pat_id")) %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))





# Number tritans first 48 months
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Month <= 48)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"
MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, generic_name, drug_class)
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories %>% filter(drug_class=="Triptan")
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% distinct()
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient) %>% mutate(grp = rle(generic_name)$lengths %>% {rep(seq(length(.)), .)})
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% select(patient, weight, grp)
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% group_by(patient, weight) %>% summarize(across(everything(), max))

Number_triptans_lines_m48 <- MIG_Drug_Histories_Triptans
names(Number_triptans_lines_m48)[3] <- "Number_Triptans"

# Any Triptan seen last year?
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Month > 48)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"
MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories %>% filter(drug_class=="Triptan")
MIG_Drug_Histories_Triptans <- MIG_Drug_Histories_Triptans %>% distinct()

Patients_triptan_after_48 <- MIG_Drug_Histories_Triptans %>% select(patient)
Patients_triptan_after_48 <- Patients_triptan_after_48 %>% mutate(triptan_after_48 = "YES")

Number_triptans_lines_m48 <- Number_triptans_lines_m48 %>% left_join(Patients_triptan_after_48) %>%
  mutate(triptan_after_48=ifelse(is.na(triptan_after_48), "NO", triptan_after_48))

Number_triptans_lines_m48 %>% group_by(Number_Triptans, triptan_after_48) %>%
  summarise(n=sum(as.numeric(weight)))

# Any flow last year?
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% filter(p1 >=48) %>% select(patient, weight, s2, flow)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(flow=="1") %>% select(patient) %>% distinct() %>% mutate(flows="FLOW")

Number_triptans_lines_m48 <- Number_triptans_lines_m48 %>% left_join(MIG_Flows_Aux._Long) %>%
  mutate(flows=ifelse(is.na(flows), "NO", flows))


data.frame(Number_triptans_lines_m48 %>% group_by(Number_Triptans, triptan_after_48) %>%
             summarise(n=sum(as.numeric(weight))))

fwrite(Number_triptans_lines_m48, "Number_triptans_lines_m48.txt")



#Flows no Triptan to No-Triptan (Higher vs Same vs Lower stock)
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(s1 = ifelse(s1=="x",0,
                                                                  ifelse(s1=="a",1,
                                                                         ifelse(s1=="A",2,
                                                                                ifelse(s1=="p",3,
                                                                                       ifelse(s1=="d",4,
                                                                                              ifelse(s1=="D",5,
                                                                                                     ifelse(s1=="O",6,7)))))))) %>%
  mutate(s2 = ifelse(s2=="x",0,
                     ifelse(s2=="a",1,
                            ifelse(s2=="A",2,
                                   ifelse(s2=="p",3,
                                          ifelse(s2=="d",4,
                                                 ifelse(s2=="D",5,
                                                        ifelse(s2=="O",6,7))))))))


RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

string_Triptan <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Triptan"], collapse = "|"),")\\b")

MIG_Flows_Aux._Long %>% filter(grepl(string_Triptan,d1) & !grepl(string_Triptan,d2)) %>% filter(s2>s1) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #452725.1

MIG_Flows_Aux._Long %>% filter(grepl(string_Triptan,d1) & !grepl(string_Triptan,d2)) %>% filter(s2==s1) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #254570.1

MIG_Flows_Aux._Long %>% filter(grepl(string_Triptan,d1) & !grepl(string_Triptan,d2)) %>% filter(s2<=s1) %>% 
  select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) #7112878

MIG_Flows_Aux._Long %>% filter(grepl(string_Triptan,d1) & !grepl(string_Triptan,d2)) %>% filter(s2>s1) %>% 
  summarise(n=sum(as.numeric(weight))) #481928.2

MIG_Flows_Aux._Long %>% filter(grepl(string_Triptan,d1) & !grepl(string_Triptan,d2)) %>% filter(s2==s1) %>% 
  summarise(n=sum(as.numeric(weight))) #415170.7

MIG_Flows_Aux._Long %>% filter(grepl(string_Triptan,d1) & !grepl(string_Triptan,d2)) %>% filter(s2<=s1) %>% 
  summarise(n=sum(as.numeric(weight))) #19614383
# ----
# Silent Suferers (Pats with Dx Last 2 years but no "\\b(a|A|p|d|D|O|I)\\b" ) -------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight)

RIME_Events <- read.table("RIME Events.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
RIME_Events <- RIME_Events %>% mutate(claimed = as.Date(claimed))

RIME_Events <- MIG_Drug_Histories %>% left_join(RIME_Events, by=c("patient"="patid", "weight"="weight"))
RIME_Events <- RIME_Events %>% group_by(patient) %>% filter(claimed>="2019-08-01")
Pats_Dc_Last_year <- RIME_Events %>% select(patient, weight) %>% distinct()

MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,52:63)
MIG_Box_Histories <- Pats_Dc_Last_year %>% left_join(MIG_Box_Histories)
MIG_Box_Histories <- MIG_Box_Histories %>% ungroup%>%  select(-c(1,2))

string_no_X <- "\\b(a|A|p|d|D|O|I)\\b"
MIG_Box_Histories <- MIG_Box_Histories %>% mutate_all(function(x) ifelse(grepl(string_no_X,x),1,0))
MIG_Box_Histories$SUM <- rowSums(MIG_Box_Histories)

#0 = x, a , #1 = any  "\\b(a|A|p|d|D|O|I)\\b"
Pats_Dc_Last_year <- Pats_Dc_Last_year %>% bind_cols(MIG_Box_Histories)
Pats_Dc_Last_year <- Pats_Dc_Last_year %>% left_join( MIG_Drug_Histories %>% select(patient, weight))

# Pats with Dx Last 2 years but no "\\b(a|A|p|d|D|O|I)\\b"   ->  #1592553
Pats_Dc_Last_year %>% ungroup() %>% filter(SUM==0) %>% summarise(n=sum(as.numeric(weight))) 

# of these, how many were treatment experinced?
Silent_Sufferers <- Pats_Dc_Last_year %>% ungroup() %>% filter(SUM==0) 




RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

Pats_ever_treated <- MIG_Drug_Histories %>% select(patient) %>% distinct()

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)

Pats_ever_Triptan <- MIG_Drug_Histories %>% filter(drug_class=="Triptan") %>% select(patient) %>% distinct()

Silent_Sufferers %>% summarise(n=sum(as.numeric(weight))) #1592553

Silent_Sufferers %>% inner_join(Pats_ever_treated) %>% summarise(n=sum(as.numeric(weight))) #1314721

Silent_Sufferers %>% inner_join(Pats_ever_Triptan) %>% summarise(n=sum(as.numeric(weight))) #380119


# ---- 
# How many patients lapsed on month60 have had drugs on months 58 or 59? ----------------------------
MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

Pats_to_keep <- MIG_Box_Histories %>% select(c(2,3))

MIG_Box_Histories <- MIG_Box_Histories %>% select(c(4:63)) %>% mutate_all(function(x) x=str_sub(x, 2L, 2L)) %>%
  mutate(month60_NEW = ifelse(month58 !="x" &  month59 != "x", month59,
                              ifelse(month58!="x" &  month59=="x",month58,month60)))

MIG_Box_Histories <- Pats_to_keep %>% bind_cols(MIG_Box_Histories)

MIG_Box_Histories %>% group_by(month60) %>% summarise(n=sum(as.numeric(weight))) %>% mutate(percent=(n/19043067)*100)




MIG_Box_Histories %>% group_by(month60_NEW) %>% summarise(n=sum(as.numeric(weight))) %>% mutate(percent=(n/19043067)*100)month60_NEW        n percent


# ----
# Who has comorbidities @ stock m60 ?
MIG_Comorbidities <- read.table("MIG Comorbidities.txt", header = T, sep="\t", 
                                colClasses = "character", stringsAsFactors = FALSE)
MIG_Comorbidities <- MIG_Comorbidities %>% select(2,21:24)

MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(c(2,3,63))
MIG_Box_Histories <- Pats_ever_treated  %>% left_join(MIG_Box_Histories)
MIG_Box_Histories <- MIG_Box_Histories%>% mutate_at(c("month60"), function(x) x=str_sub(x, 2L, 2L))
MIG_Box_Histories <- MIG_Box_Histories %>% left_join(MIG_Comorbidities) 

MIG_Box_Histories %>% group_by(month60) %>% mutate(total=sum(as.numeric(weight))) %>% group_by(month60, total, cardiovascular_comorbidity) %>% 
  summarise(n=sum(as.numeric(weight))) %>% mutate(percent=n/total)




MIG_Box_Histories %>% group_by(month60) %>% mutate(total=sum(as.numeric(weight))) %>% group_by(month60, total, epileptic_comorbidity) %>% 
  summarise(n=sum(as.numeric(weight))) %>% mutate(percent=n/total)



MIG_Box_Histories %>% group_by(month60) %>% mutate(total=sum(as.numeric(weight))) %>% group_by(month60, total, pain_comorbidity) %>% 
  summarise(n=sum(as.numeric(weight))) %>% mutate(percent=n/total)

MIG_Box_Histories %>% group_by(month60) %>% mutate(total=sum(as.numeric(weight))) %>% group_by(month60, total, psychiatric_comorbidity) %>% 
  summarise(n=sum(as.numeric(weight))) %>% mutate(percent=n/total)

# Age distribution entire cohort -------------------

RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Demographics <- RIME_Demographics %>% select(patid, weight, age)
names(RIME_Demographics)[1] <- "patient"

MIG_Drug_Histories <- Pats_ever_treated %>% left_join(RIME_Demographics, by=c("patient"="patient"))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(age = as.numeric(age))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(age_group = ifelse(age>=18 & age<30,"18_to_29", 
                                                                       ifelse(age>=30 & age<40, "30_to_39", 
                                                                              ifelse(age>=40 & age<50, "40_to_49",
                                                                                     ifelse(age>=50 & age<60, "50_to_59", 
                                                                                            ifelse(age>=60 & age<70,"60_to_69",
                                                                                                   ifelse(age>=70 & age<80, "70_to_79", "+80")))))))



MIG_Drug_Histories %>% group_by(age_group) %>% summarise(n=sum(as.numeric(weight)))

# ----
# Type pf migraine in patients with Oral CGRP vs no ORAL CGRP month 60-------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)
Pats_ON_Oral_CGRP <- MIG_Drug_Histories %>% filter(grepl("135",month60)|grepl("136",month60)) %>% select(patient, weight) %>% distinct()
Pats_ON_Oral_CGRP %>% summarise(n=sum(as.numeric(weight))) # 113762.2
Pats_without_Oral_CGRP <- MIG_Drug_Histories %>% anti_join(Pats_ON_Oral_CGRP) %>% select(patient, weight) %>% distinct()
Pats_without_Oral_CGRP %>% summarise(n=sum(as.numeric(weight))) # 18929305

RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)

RIME_Demographics <- RIME_Demographics  %>% select(patid, weight, diagnosis)
names(RIME_Demographics)[1] <- "patient"


RIME_Demographics %>% inner_join(Pats_ON_Oral_CGRP, by=c("patient"="patient")) %>% filter(grepl("Intractable",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #71823.22

RIME_Demographics %>% inner_join(Pats_ON_Oral_CGRP, by=c("patient"="patient")) %>% filter(grepl("Aura",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #53129.19

RIME_Demographics %>% inner_join(Pats_ON_Oral_CGRP, by=c("patient"="patient"))%>% filter(grepl("Severe",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #43880.14

RIME_Demographics %>% inner_join(Pats_ON_Oral_CGRP, by=c("patient"="patient")) %>% filter(grepl("Chronic",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #75872.55

RIME_Demographics %>% inner_join(Pats_ON_Oral_CGRP, by=c("patient"="patient")) %>% filter(grepl("Targeted Therapy",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #0




RIME_Demographics %>% inner_join(Pats_without_Oral_CGRP, by=c("patient"="patient")) %>% filter(grepl("Intractable",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #3641293

RIME_Demographics %>% inner_join(Pats_without_Oral_CGRP, by=c("patient"="patient")) %>% filter(grepl("Aura",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #4896667

RIME_Demographics %>% inner_join(Pats_without_Oral_CGRP, by=c("patient"="patient"))%>% filter(grepl("Severe",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #2166391

RIME_Demographics %>% inner_join(Pats_without_Oral_CGRP, by=c("patient"="patient")) %>% filter(grepl("Chronic",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #2824173

RIME_Demographics %>% inner_join(Pats_without_Oral_CGRP, by=c("patient"="patient")) %>% filter(grepl("Targeted Therapy",diagnosis)) %>% 
  summarise(pop = sum(as.numeric(weight.x))) #7465768

# ----
# Triptan Failues / Intolorants -----------------------------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
string_Triptan <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Triptan"], collapse = "|"),")\\b")

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

All_Patients <- MIG_Drug_Histories %>% select(c(2,3))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(1,2,3))
MIG_Drug_Histories_triptan <- MIG_Drug_Histories %>% mutate_all(function(x) ifelse(grepl(string_Triptan,x),1,0))

MIG_Drug_Histories_triptan$Triptan_Exp <- rowSums(MIG_Drug_Histories_triptan)

MIG_Drug_Histories_triptan <- All_Patients %>% bind_cols(MIG_Drug_Histories_triptan)

MIG_Drug_Histories_triptan <- MIG_Drug_Histories_triptan %>% mutate(Tripan_Intolorant = ifelse(Triptan_Exp == 1 & month60 == 0, "YES", "NO"))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

Pats_Lapsed3months <- MIG_Drug_Histories %>% mutate(Lapsed3months = ifelse(month58=="-" & month59=="-" & month60=="-", "Lapsed", "Treat")) %>%
  select(patient, Lapsed3months)

MIG_Drug_Histories_triptan <- MIG_Drug_Histories_triptan %>% left_join(Pats_Lapsed3months)

MIG_Drug_Histories_triptan <- MIG_Drug_Histories_triptan %>% 
  mutate(Triptan_fail = 
           ifelse(Triptan_Exp!=0 & Tripan_Intolorant=="NO" & month58==0 & month59==0 & month60==0 & Lapsed3months=="Treat", "Fail", "none"))

MIG_Drug_Histories_triptan %>% summarise(n=sum(as.numeric(weight))) 

MIG_Drug_Histories_triptan %>% filter(Triptan_Exp == 0) %>% summarise(n=sum(as.numeric(weight))) 

MIG_Drug_Histories_triptan %>% filter(Triptan_Exp != 0) %>% filter(Tripan_Intolorant == "YES")  %>% summarise(n=sum(as.numeric(weight))) 

MIG_Drug_Histories_triptan %>% filter(Triptan_Exp != 0) %>% filter(Tripan_Intolorant == "NO")  %>% summarise(n=sum(as.numeric(weight))) 

MIG_Drug_Histories_triptan %>% filter(Triptan_Exp != 0) %>% filter(Tripan_Intolorant == "NO") %>% filter(Triptan_fail == "Fail")  %>% summarise(n=sum(as.numeric(weight))) 

MIG_Drug_Histories_triptan %>% filter(Triptan_Exp != 0) %>% filter(Tripan_Intolorant == "NO") %>% filter(Triptan_fail == "none")  %>% summarise(n=sum(as.numeric(weight))) 
# -----
# Rimegepant Outflows vs 1 or 3months first rimegapnt exp ------
# Rimegepant Durations 1month vs 3+ months type pf pat  -------------------
# vector with Rimegepant patients and first duration
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "Rimegepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Rimegepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- as.numeric(MIG_Drug_Histories$Month)

# count (how many months) in each of these  periods
Rimegepant_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Rimegepant_Periods_MIG)[3] <- "Duration"

Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% filter(grp==min(grp))
Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% select(-c(grp))
Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% mutate(Duration_bucket = ifelse(Duration == 1, "1month", 
                                                                                     ifelse(Duration>=3,"+3months", "2months")))
Rimegepant_Periods_MIG_2 <- Rimegepant_Periods_MIG

One_month_Rim_Patients <- Rimegepant_Periods_MIG_2 %>% filter(Duration_bucket == "1month")
One_month_Rim_Patients <- One_month_Rim_Patients  %>% select(1)
Three_months_Rim_Patients <- Rimegepant_Periods_MIG_2 %>% filter(Duration_bucket == "+3months")
Three_months_Rim_Patients <- Three_months_Rim_Patients  %>% select(1)



MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))


MIG_Flows_Aux._Long %>% filter(p1 >=48) %>%  inner_join(One_month_Rim_Patients) %>% filter(grepl("135",d1)) %>% 
  filter(!grepl("135",d2)) %>%  group_by(s2) %>% summarise(pats=sum(as.numeric(weight)))



MIG_Flows_Aux._Long %>% filter(p1 >=48) %>%  inner_join(Three_months_Rim_Patients) %>% filter(grepl("135",d1)) %>% 
  filter(!grepl("135",d2)) %>%  group_by(s2) %>% summarise(pats=sum(as.numeric(weight)))



# ----
# Persistency ON different triptan formulations -------------------------------------------------
# Select Formulations, drug_ids for each of the 3 Triptans
RIME_Medications <- fread("RIME Medications.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Medications <- RIME_Medications %>% filter(generic_name == "Sumatriptan" | generic_name == "Zolmitriptan" | generic_name == "Rizatriptan")
RIME_Medications <- RIME_Medications %>% select(drug_id, generic_name, med_format)

library(zoo)
library(spatstat)

# Select all scripts, filter for the 60months and for those 3 drugs only
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(drug_class == "Triptan")
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(generic_name =="Sumatriptan" | generic_name == "Zolmitriptan" | generic_name == "Rizatriptan")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(drug_id, generic_name, pat_id, weight, from_dt)
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(from_dt <= "2021-07-31")
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(from_dt > "2016-07-31")

# Lookup for formulations
MIG_Doses_BIG <- MIG_Doses_BIG %>% left_join(RIME_Medications, by = c("drug_id"="drug_id", "generic_name"="generic_name"))

# Split into 3 tables with drugs of interest
MIG_Doses_BIG_Sumatriptan <- MIG_Doses_BIG %>% filter(generic_name =="Sumatriptan")
MIG_Doses_BIG_Zolmitriptan <- MIG_Doses_BIG %>% filter(generic_name =="Zolmitriptan")
MIG_Doses_BIG_Rizatriptan <- MIG_Doses_BIG %>% filter(generic_name =="Rizatriptan")

# Extract month and year
MIG_Doses_BIG_Sumatriptan <- MIG_Doses_BIG_Sumatriptan %>% mutate(month = as.yearmon(from_dt))
MIG_Doses_BIG_Zolmitriptan <- MIG_Doses_BIG_Zolmitriptan %>% mutate(month = as.yearmon(from_dt))
MIG_Doses_BIG_Rizatriptan <- MIG_Doses_BIG_Rizatriptan %>% mutate(month = as.yearmon(from_dt))

# remove exact date
MIG_Doses_BIG_Sumatriptan <- MIG_Doses_BIG_Sumatriptan %>% select(-c(from_dt))
MIG_Doses_BIG_Zolmitriptan <- MIG_Doses_BIG_Zolmitriptan %>% select(-c(from_dt))
MIG_Doses_BIG_Rizatriptan <- MIG_Doses_BIG_Rizatriptan %>%  select(-c(from_dt))

# remove dupplicates (i.e. the same patient having the same thing more than once per month)
MIG_Doses_BIG_Sumatriptan <- MIG_Doses_BIG_Sumatriptan %>% distinct()
MIG_Doses_BIG_Zolmitriptan <- MIG_Doses_BIG_Zolmitriptan %>%  distinct()
MIG_Doses_BIG_Rizatriptan <- MIG_Doses_BIG_Rizatriptan %>%   distinct()

# arrange by pat and month
MIG_Doses_BIG_Sumatriptan <- MIG_Doses_BIG_Sumatriptan %>% arrange(pat_id, month)
MIG_Doses_BIG_Zolmitriptan <- MIG_Doses_BIG_Zolmitriptan %>%  arrange(pat_id, month)
MIG_Doses_BIG_Rizatriptan <- MIG_Doses_BIG_Rizatriptan %>%   arrange(pat_id, month)

#select only pat, formulaiton, month
MIG_Doses_BIG_Sumatriptan <- MIG_Doses_BIG_Sumatriptan %>% select(pat_id, weight, med_format, month) %>% distinct()
MIG_Doses_BIG_Zolmitriptan <- MIG_Doses_BIG_Zolmitriptan %>% select(pat_id, weight, med_format, month) %>% distinct()
MIG_Doses_BIG_Rizatriptan <- MIG_Doses_BIG_Rizatriptan %>%  select(pat_id, weight, med_format, month) %>% distinct()



# It's OKAY for the comparisons, visibility for each drug ON the market is the same for the comparisions of interest
# MIG_Doses_BIG_Sumatriptan %>% select(med_format, month) %>% distinct() %>% group_by(med_format) %>% filter(month == min(month))

# 
# MIG_Doses_BIG_Zolmitriptan %>% select(med_format, month) %>% distinct() %>% group_by(med_format) %>% filter(month == min(month))

# MIG_Doses_BIG_Rizatriptan %>% select(med_format, month) %>% distinct() %>% group_by(med_format) %>% filter(month == min(month))


# How many unique months each pat was on each formulation
MIG_Doses_BIG_Sumatriptan <- MIG_Doses_BIG_Sumatriptan %>% group_by(pat_id, weight, med_format) %>% count()
MIG_Doses_BIG_Zolmitriptan <- MIG_Doses_BIG_Zolmitriptan %>% group_by(pat_id, weight, med_format) %>% count()
MIG_Doses_BIG_Rizatriptan <- MIG_Doses_BIG_Rizatriptan %>% group_by(pat_id, weight, med_format) %>% count()


### SUMATRIPTAN ###
MIG_Doses_BIG_Sumatriptan %>% group_by(med_format) %>% summarise(n = weighted.mean(n, as.numeric(weight)))



MIG_Doses_BIG_Sumatriptan %>% group_by(med_format) %>% summarise(n = weighted.median(n, as.numeric(weight)))


### ZOLMATRIPTAN ###
MIG_Doses_BIG_Zolmitriptan %>% group_by(med_format) %>% summarise(n = weighted.mean(n, as.numeric(weight)))

MIG_Doses_BIG_Zolmitriptan %>% group_by(med_format) %>% summarise(n = weighted.median(n, as.numeric(weight)))


### RIZATRIPTAN ###
MIG_Doses_BIG_Rizatriptan %>% group_by(med_format) %>% summarise(n = weighted.mean(n, as.numeric(weight)))

MIG_Doses_BIG_Rizatriptan %>% group_by(med_format) %>% summarise(n = weighted.median(n, as.numeric(weight)))




# Check if supply days are different

MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(drug_class == "Triptan")
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(generic_name =="Sumatriptan" | generic_name == "Zolmitriptan" | generic_name == "Rizatriptan")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(drug_id, generic_name, pat_id,  dayssup, weight, from_dt)
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(from_dt <= "2021-07-31")
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(from_dt > "2016-07-31")

# Lookup for formulations
MIG_Doses_BIG <- MIG_Doses_BIG %>% left_join(RIME_Medications, by = c("drug_id"="drug_id", "generic_name"="generic_name"))

# Split into 3 tables with drugs of interest
MIG_Doses_BIG_Sumatriptan <- MIG_Doses_BIG %>% filter(generic_name =="Sumatriptan")
MIG_Doses_BIG_Zolmitriptan <- MIG_Doses_BIG %>% filter(generic_name =="Zolmitriptan")
MIG_Doses_BIG_Rizatriptan <- MIG_Doses_BIG %>% filter(generic_name =="Rizatriptan")

MIG_Doses_BIG_Sumatriptan %>% group_by(med_format) %>% summarise(n = weighted.mean(as.numeric(dayssup), as.numeric(weight)))


MIG_Doses_BIG_Zolmitriptan %>% group_by(med_format) %>% summarise(n = weighted.mean(as.numeric(dayssup), as.numeric(weight)))

MIG_Doses_BIG_Rizatriptan %>% group_by(med_format) %>% summarise(n = weighted.mean(as.numeric(dayssup), as.numeric(weight)))

# ----
# Subdivide intraflows by type of accompaining flows ------------------------------------------------

# # # # using INDIVIDUAL CLASSES # # # # 

library(data.table)
library(tidyverse)

# Ingredients and Lookups
RIME_Ingredients <- fread("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients$drug_id <- unlist(lapply(RIME_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))

# Class lookups
string_NSAID <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "NSAID"], collapse = "|"),")\\b")
string_Analgesic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Analgesic"], collapse = "|"),")\\b")
string_WeakOpioid <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Weak Opioid"], collapse = "|"),")\\b")
string_StrongOpioid <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Strong Opioid"], collapse = "|"),")\\b")
string_Antiemetic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Antiemetic"], collapse = "|"),")\\b")
string_Steroid <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Steroid"], collapse = "|"),")\\b")
string_Sedative <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Sedative"], collapse = "|"),")\\b")
string_Antipsychotic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Antipsychotic"], collapse = "|"),")\\b")
string_Hospitalization <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Hospitalization"], collapse = "|"),")\\b")
string_Ergot <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Ergot"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Triptan"], collapse = "|"),")\\b")
string_Ditan <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Ditan"], collapse = "|"),")\\b")
string_MuscleRelaxant <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Muscle Relaxant"], collapse = "|"),")\\b")
string_Antiepileptic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Antiepileptic"], collapse = "|"),")\\b")
string_Cardiovascular <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Cardiovascular"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Beta Blocker"], collapse = "|"),")\\b")
string_CalciumBlocker <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Calcium Blocker"], collapse = "|"),")\\b")
string_Tricyclic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Tricyclic"], collapse = "|"),")\\b")
string_SSRI <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "SSRI"], collapse = "|"),")\\b")
string_SNRI <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "SNRI"], collapse = "|"),")\\b")
string_Neural <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Neural"], collapse = "|"),")\\b")
string_CGRPOral <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRPInjectable <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")

#Flows table
MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

data <- MIG_Flows_Aux._Long[flow == 1 & s1 == s2,.(patient,weight,p1,p2,d1,d2,s1,s2,flow)]

# NSAID Therapy class - flags
data <- data[, NSAID_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_NSAID), str_c(unlist(str_extract_all(x, string_NSAID)), collapse = ","),"")))]
data <- data[, NSAID_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_NSAID), str_c(unlist(str_extract_all(x, string_NSAID)), collapse = ","),"")))]
data <- data[, nr_NSAID_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_NSAID)*1), str_split(x,","))))]
data <- data[, nr_NSAID_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_NSAID)*1), str_split(x,","))))]
data <- data[, nr_NSAIDUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("NSAID_d1","NSAID_d2")] 
data <- data[, NSAID_flow_type := ifelse(nr_NSAID_d2 < nr_NSAID_d1 & nr_NSAIDUnq_d1d2 > nr_NSAID_d1, "D+S", 
                                         ifelse(nr_NSAID_d2 > nr_NSAID_d1 & nr_NSAIDUnq_d1d2 > nr_NSAID_d2, "A+S",
                                                ifelse(nr_NSAID_d2 < nr_NSAID_d1, "D", 
                                                       ifelse(nr_NSAID_d2 > nr_NSAID_d1, "A", 
                                                              ifelse(nr_NSAID_d2 == nr_NSAID_d1 & NSAID_d2 != NSAID_d1, "S","-")))))] 


# Analgesic Therapy class - flags
data <- data[, Analgesic_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Analgesic), str_c(unlist(str_extract_all(x, string_Analgesic)), collapse = ","),"")))]
data <- data[, Analgesic_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Analgesic), str_c(unlist(str_extract_all(x, string_Analgesic)), collapse = ","),"")))]
data <- data[, nr_Analgesic_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Analgesic)*1), str_split(x,","))))]
data <- data[, nr_Analgesic_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Analgesic)*1), str_split(x,","))))]
data <- data[, nr_AnalgesicUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Analgesic_d1","Analgesic_d2")] 
data <- data[, Analgesic_flow_type := ifelse(nr_Analgesic_d2 < nr_Analgesic_d1 & nr_AnalgesicUnq_d1d2 > nr_Analgesic_d1, "D+S", 
                                             ifelse(nr_Analgesic_d2 > nr_Analgesic_d1 & nr_AnalgesicUnq_d1d2 > nr_Analgesic_d2, "A+S",
                                                    ifelse(nr_Analgesic_d2 < nr_Analgesic_d1, "D", 
                                                           ifelse(nr_Analgesic_d2 > nr_Analgesic_d1, "A", 
                                                                  ifelse(nr_Analgesic_d2 == nr_Analgesic_d1 & Analgesic_d2 != Analgesic_d1, "S","-")))))] 


# WeakOpioid Therapy class - flags
data <- data[, WeakOpioid_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_WeakOpioid), str_c(unlist(str_extract_all(x, string_WeakOpioid)), collapse = ","),"")))]
data <- data[, WeakOpioid_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_WeakOpioid), str_c(unlist(str_extract_all(x, string_WeakOpioid)), collapse = ","),"")))]
data <- data[, nr_WeakOpioid_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_WeakOpioid)*1), str_split(x,","))))]
data <- data[, nr_WeakOpioid_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_WeakOpioid)*1), str_split(x,","))))]
data <- data[, nr_WeakOpioidUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("WeakOpioid_d1","WeakOpioid_d2")] 
data <- data[, WeakOpioid_flow_type := ifelse(nr_WeakOpioid_d2 < nr_WeakOpioid_d1 & nr_WeakOpioidUnq_d1d2 > nr_WeakOpioid_d1, "D+S", 
                                              ifelse(nr_WeakOpioid_d2 > nr_WeakOpioid_d1 & nr_WeakOpioidUnq_d1d2 > nr_WeakOpioid_d2, "A+S",
                                                     ifelse(nr_WeakOpioid_d2 < nr_WeakOpioid_d1, "D", 
                                                            ifelse(nr_WeakOpioid_d2 > nr_WeakOpioid_d1, "A", 
                                                                   ifelse(nr_WeakOpioid_d2 == nr_WeakOpioid_d1 & WeakOpioid_d2 != WeakOpioid_d1, "S","-")))))] 


# StrongOpioid Therapy class - flags
data <- data[, StrongOpioid_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_StrongOpioid), str_c(unlist(str_extract_all(x, string_StrongOpioid)), collapse = ","),"")))]
data <- data[, StrongOpioid_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_StrongOpioid), str_c(unlist(str_extract_all(x, string_StrongOpioid)), collapse = ","),"")))]
data <- data[, nr_StrongOpioid_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_StrongOpioid)*1), str_split(x,","))))]
data <- data[, nr_StrongOpioid_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_StrongOpioid)*1), str_split(x,","))))]
data <- data[, nr_StrongOpioidUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("StrongOpioid_d1","StrongOpioid_d2")] 
data <- data[, StrongOpioid_flow_type := ifelse(nr_StrongOpioid_d2 < nr_StrongOpioid_d1 & nr_StrongOpioidUnq_d1d2 > nr_StrongOpioid_d1, "D+S", 
                                                ifelse(nr_StrongOpioid_d2 > nr_StrongOpioid_d1 & nr_StrongOpioidUnq_d1d2 > nr_StrongOpioid_d2, "A+S",
                                                       ifelse(nr_StrongOpioid_d2 < nr_StrongOpioid_d1, "D", 
                                                              ifelse(nr_StrongOpioid_d2 > nr_StrongOpioid_d1, "A", 
                                                                     ifelse(nr_StrongOpioid_d2 == nr_StrongOpioid_d1 & StrongOpioid_d2 != StrongOpioid_d1, "S","-")))))] 


# Antiemetic Therapy class - flags
data <- data[, Antiemetic_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Antiemetic), str_c(unlist(str_extract_all(x, string_Antiemetic)), collapse = ","),"")))]
data <- data[, Antiemetic_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Antiemetic), str_c(unlist(str_extract_all(x, string_Antiemetic)), collapse = ","),"")))]
data <- data[, nr_Antiemetic_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Antiemetic)*1), str_split(x,","))))]
data <- data[, nr_Antiemetic_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Antiemetic)*1), str_split(x,","))))]
data <- data[, nr_AntiemeticUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Antiemetic_d1","Antiemetic_d2")] 
data <- data[, Antiemetic_flow_type := ifelse(nr_Antiemetic_d2 < nr_Antiemetic_d1 & nr_AntiemeticUnq_d1d2 > nr_Antiemetic_d1, "D+S", 
                                              ifelse(nr_Antiemetic_d2 > nr_Antiemetic_d1 & nr_AntiemeticUnq_d1d2 > nr_Antiemetic_d2, "A+S",
                                                     ifelse(nr_Antiemetic_d2 < nr_Antiemetic_d1, "D", 
                                                            ifelse(nr_Antiemetic_d2 > nr_Antiemetic_d1, "A", 
                                                                   ifelse(nr_Antiemetic_d2 == nr_Antiemetic_d1 & Antiemetic_d2 != Antiemetic_d1, "S","-")))))] 


# Steroid Therapy class - flags
data <- data[, Steroid_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Steroid), str_c(unlist(str_extract_all(x, string_Steroid)), collapse = ","),"")))]
data <- data[, Steroid_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Steroid), str_c(unlist(str_extract_all(x, string_Steroid)), collapse = ","),"")))]
data <- data[, nr_Steroid_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Steroid)*1), str_split(x,","))))]
data <- data[, nr_Steroid_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Steroid)*1), str_split(x,","))))]
data <- data[, nr_SteroidUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Steroid_d1","Steroid_d2")] 
data <- data[, Steroid_flow_type := ifelse(nr_Steroid_d2 < nr_Steroid_d1 & nr_SteroidUnq_d1d2 > nr_Steroid_d1, "D+S", 
                                           ifelse(nr_Steroid_d2 > nr_Steroid_d1 & nr_SteroidUnq_d1d2 > nr_Steroid_d2, "A+S",
                                                  ifelse(nr_Steroid_d2 < nr_Steroid_d1, "D", 
                                                         ifelse(nr_Steroid_d2 > nr_Steroid_d1, "A", 
                                                                ifelse(nr_Steroid_d2 == nr_Steroid_d1 & Steroid_d2 != Steroid_d1, "S","-")))))] 


# Sedative Therapy class - flags
data <- data[, Sedative_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Sedative), str_c(unlist(str_extract_all(x, string_Sedative)), collapse = ","),"")))]
data <- data[, Sedative_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Sedative), str_c(unlist(str_extract_all(x, string_Sedative)), collapse = ","),"")))]
data <- data[, nr_Sedative_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Sedative)*1), str_split(x,","))))]
data <- data[, nr_Sedative_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Sedative)*1), str_split(x,","))))]
data <- data[, nr_SedativeUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Sedative_d1","Sedative_d2")] 
data <- data[, Sedative_flow_type := ifelse(nr_Sedative_d2 < nr_Sedative_d1 & nr_SedativeUnq_d1d2 > nr_Sedative_d1, "D+S", 
                                            ifelse(nr_Sedative_d2 > nr_Sedative_d1 & nr_SedativeUnq_d1d2 > nr_Sedative_d2, "A+S",
                                                   ifelse(nr_Sedative_d2 < nr_Sedative_d1, "D", 
                                                          ifelse(nr_Sedative_d2 > nr_Sedative_d1, "A", 
                                                                 ifelse(nr_Sedative_d2 == nr_Sedative_d1 & Sedative_d2 != Sedative_d1, "S","-")))))] 



# Antipsychotic Therapy class - flags
data <- data[, Antipsychotic_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Antipsychotic), str_c(unlist(str_extract_all(x, string_Antipsychotic)), collapse = ","),"")))]
data <- data[, Antipsychotic_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Antipsychotic), str_c(unlist(str_extract_all(x, string_Antipsychotic)), collapse = ","),"")))]
data <- data[, nr_Antipsychotic_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Antipsychotic)*1), str_split(x,","))))]
data <- data[, nr_Antipsychotic_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Antipsychotic)*1), str_split(x,","))))]
data <- data[, nr_AntipsychoticUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Antipsychotic_d1","Antipsychotic_d2")] 
data <- data[, Antipsychotic_flow_type := ifelse(nr_Antipsychotic_d2 < nr_Antipsychotic_d1 & nr_AntipsychoticUnq_d1d2 > nr_Antipsychotic_d1, "D+S", 
                                                 ifelse(nr_Antipsychotic_d2 > nr_Antipsychotic_d1 & nr_AntipsychoticUnq_d1d2 > nr_Antipsychotic_d2, "A+S",
                                                        ifelse(nr_Antipsychotic_d2 < nr_Antipsychotic_d1, "D", 
                                                               ifelse(nr_Antipsychotic_d2 > nr_Antipsychotic_d1, "A", 
                                                                      ifelse(nr_Antipsychotic_d2 == nr_Antipsychotic_d1 & Antipsychotic_d2 != Antipsychotic_d1, "S","-")))))] 


# Hospitalization Therapy class - flags
data <- data[, Hospitalization_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Hospitalization), str_c(unlist(str_extract_all(x, string_Hospitalization)), collapse = ","),"")))]
data <- data[, Hospitalization_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Hospitalization), str_c(unlist(str_extract_all(x, string_Hospitalization)), collapse = ","),"")))]
data <- data[, nr_Hospitalization_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Hospitalization)*1), str_split(x,","))))]
data <- data[, nr_Hospitalization_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Hospitalization)*1), str_split(x,","))))]
data <- data[, nr_HospitalizationUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Hospitalization_d1","Hospitalization_d2")] 
data <- data[, Hospitalization_flow_type := ifelse(nr_Hospitalization_d2 < nr_Hospitalization_d1 & nr_HospitalizationUnq_d1d2 > nr_Hospitalization_d1, "D+S", 
                                                   ifelse(nr_Hospitalization_d2 > nr_Hospitalization_d1 & nr_HospitalizationUnq_d1d2 > nr_Hospitalization_d2, "A+S",
                                                          ifelse(nr_Hospitalization_d2 < nr_Hospitalization_d1, "D", 
                                                                 ifelse(nr_Hospitalization_d2 > nr_Hospitalization_d1, "A", 
                                                                        ifelse(nr_Hospitalization_d2 == nr_Hospitalization_d1 & Hospitalization_d2 != Hospitalization_d1, "S","-")))))] 



# Ergot Therapy class - flags
data <- data[, Ergot_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Ergot), str_c(unlist(str_extract_all(x, string_Ergot)), collapse = ","),"")))]
data <- data[, Ergot_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Ergot), str_c(unlist(str_extract_all(x, string_Ergot)), collapse = ","),"")))]
data <- data[, nr_Ergot_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Ergot)*1), str_split(x,","))))]
data <- data[, nr_Ergot_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Ergot)*1), str_split(x,","))))]
data <- data[, nr_ErgotUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Ergot_d1","Ergot_d2")] 
data <- data[, Ergot_flow_type := ifelse(nr_Ergot_d2 < nr_Ergot_d1 & nr_ErgotUnq_d1d2 > nr_Ergot_d1, "D+S", 
                                         ifelse(nr_Ergot_d2 > nr_Ergot_d1 & nr_ErgotUnq_d1d2 > nr_Ergot_d2, "A+S",
                                                ifelse(nr_Ergot_d2 < nr_Ergot_d1, "D", 
                                                       ifelse(nr_Ergot_d2 > nr_Ergot_d1, "A", 
                                                              ifelse(nr_Ergot_d2 == nr_Ergot_d1 & Ergot_d2 != Ergot_d1, "S","-")))))] 


# Triptan Therapy class - flags
data <- data[, Triptan_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Triptan), str_c(unlist(str_extract_all(x, string_Triptan)), collapse = ","),"")))]
data <- data[, Triptan_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Triptan), str_c(unlist(str_extract_all(x, string_Triptan)), collapse = ","),"")))]
data <- data[, nr_Triptan_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Triptan)*1), str_split(x,","))))]
data <- data[, nr_Triptan_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Triptan)*1), str_split(x,","))))]
data <- data[, nr_TriptanUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Triptan_d1","Triptan_d2")] 
data <- data[, Triptan_flow_type := ifelse(nr_Triptan_d2 < nr_Triptan_d1 & nr_TriptanUnq_d1d2 > nr_Triptan_d1, "D+S", 
                                           ifelse(nr_Triptan_d2 > nr_Triptan_d1 & nr_TriptanUnq_d1d2 > nr_Triptan_d2, "A+S",
                                                  ifelse(nr_Triptan_d2 < nr_Triptan_d1, "D", 
                                                         ifelse(nr_Triptan_d2 > nr_Triptan_d1, "A", 
                                                                ifelse(nr_Triptan_d2 == nr_Triptan_d1 & Triptan_d2 != Triptan_d1, "S","-")))))] 


# Ditan Therapy class - flags
data <- data[, Ditan_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Ditan), str_c(unlist(str_extract_all(x, string_Ditan)), collapse = ","),"")))]
data <- data[, Ditan_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Ditan), str_c(unlist(str_extract_all(x, string_Ditan)), collapse = ","),"")))]
data <- data[, nr_Ditan_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Ditan)*1), str_split(x,","))))]
data <- data[, nr_Ditan_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Ditan)*1), str_split(x,","))))]
data <- data[, nr_DitanUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Ditan_d1","Ditan_d2")] 
data <- data[, Ditan_flow_type := ifelse(nr_Ditan_d2 < nr_Ditan_d1 & nr_DitanUnq_d1d2 > nr_Ditan_d1, "D+S", 
                                         ifelse(nr_Ditan_d2 > nr_Ditan_d1 & nr_DitanUnq_d1d2 > nr_Ditan_d2, "A+S",
                                                ifelse(nr_Ditan_d2 < nr_Ditan_d1, "D", 
                                                       ifelse(nr_Ditan_d2 > nr_Ditan_d1, "A", 
                                                              ifelse(nr_Ditan_d2 == nr_Ditan_d1 & Ditan_d2 != Ditan_d1, "S","-")))))] 


# MuscleRelaxant Therapy class - flags
data <- data[, MuscleRelaxant_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_MuscleRelaxant), str_c(unlist(str_extract_all(x, string_MuscleRelaxant)), collapse = ","),"")))]
data <- data[, MuscleRelaxant_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_MuscleRelaxant), str_c(unlist(str_extract_all(x, string_MuscleRelaxant)), collapse = ","),"")))]
data <- data[, nr_MuscleRelaxant_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_MuscleRelaxant)*1), str_split(x,","))))]
data <- data[, nr_MuscleRelaxant_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_MuscleRelaxant)*1), str_split(x,","))))]
data <- data[, nr_MuscleRelaxantUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("MuscleRelaxant_d1","MuscleRelaxant_d2")] 
data <- data[, MuscleRelaxant_flow_type := ifelse(nr_MuscleRelaxant_d2 < nr_MuscleRelaxant_d1 & nr_MuscleRelaxantUnq_d1d2 > nr_MuscleRelaxant_d1, "D+S", 
                                                  ifelse(nr_MuscleRelaxant_d2 > nr_MuscleRelaxant_d1 & nr_MuscleRelaxantUnq_d1d2 > nr_MuscleRelaxant_d2, "A+S",
                                                         ifelse(nr_MuscleRelaxant_d2 < nr_MuscleRelaxant_d1, "D", 
                                                                ifelse(nr_MuscleRelaxant_d2 > nr_MuscleRelaxant_d1, "A", 
                                                                       ifelse(nr_MuscleRelaxant_d2 == nr_MuscleRelaxant_d1 & MuscleRelaxant_d2 != MuscleRelaxant_d1, "S","-")))))] 


# Antiepileptic Therapy class - flags
data <- data[, Antiepileptic_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Antiepileptic), str_c(unlist(str_extract_all(x, string_Antiepileptic)), collapse = ","),"")))]
data <- data[, Antiepileptic_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Antiepileptic), str_c(unlist(str_extract_all(x, string_Antiepileptic)), collapse = ","),"")))]
data <- data[, nr_Antiepileptic_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Antiepileptic)*1), str_split(x,","))))]
data <- data[, nr_Antiepileptic_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Antiepileptic)*1), str_split(x,","))))]
data <- data[, nr_AntiepilepticUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Antiepileptic_d1","Antiepileptic_d2")] 
data <- data[, Antiepileptic_flow_type := ifelse(nr_Antiepileptic_d2 < nr_Antiepileptic_d1 & nr_AntiepilepticUnq_d1d2 > nr_Antiepileptic_d1, "D+S", 
                                                 ifelse(nr_Antiepileptic_d2 > nr_Antiepileptic_d1 & nr_AntiepilepticUnq_d1d2 > nr_Antiepileptic_d2, "A+S",
                                                        ifelse(nr_Antiepileptic_d2 < nr_Antiepileptic_d1, "D", 
                                                               ifelse(nr_Antiepileptic_d2 > nr_Antiepileptic_d1, "A", 
                                                                      ifelse(nr_Antiepileptic_d2 == nr_Antiepileptic_d1 & Antiepileptic_d2 != Antiepileptic_d1, "S","-")))))] 



# Cardiovascular Therapy class - flags
data <- data[, Cardiovascular_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Cardiovascular), str_c(unlist(str_extract_all(x, string_Cardiovascular)), collapse = ","),"")))]
data <- data[, Cardiovascular_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Cardiovascular), str_c(unlist(str_extract_all(x, string_Cardiovascular)), collapse = ","),"")))]
data <- data[, nr_Cardiovascular_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Cardiovascular)*1), str_split(x,","))))]
data <- data[, nr_Cardiovascular_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Cardiovascular)*1), str_split(x,","))))]
data <- data[, nr_CardiovascularUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Cardiovascular_d1","Cardiovascular_d2")] 
data <- data[, Cardiovascular_flow_type := ifelse(nr_Cardiovascular_d2 < nr_Cardiovascular_d1 & nr_CardiovascularUnq_d1d2 > nr_Cardiovascular_d1, "D+S", 
                                                  ifelse(nr_Cardiovascular_d2 > nr_Cardiovascular_d1 & nr_CardiovascularUnq_d1d2 > nr_Cardiovascular_d2, "A+S",
                                                         ifelse(nr_Cardiovascular_d2 < nr_Cardiovascular_d1, "D", 
                                                                ifelse(nr_Cardiovascular_d2 > nr_Cardiovascular_d1, "A", 
                                                                       ifelse(nr_Cardiovascular_d2 == nr_Cardiovascular_d1 & Cardiovascular_d2 != Cardiovascular_d1, "S","-")))))] 



# BetaBlocker Therapy class - flags
data <- data[, BetaBlocker_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_BetaBlocker), str_c(unlist(str_extract_all(x, string_BetaBlocker)), collapse = ","),"")))]
data <- data[, BetaBlocker_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_BetaBlocker), str_c(unlist(str_extract_all(x, string_BetaBlocker)), collapse = ","),"")))]
data <- data[, nr_BetaBlocker_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_BetaBlocker)*1), str_split(x,","))))]
data <- data[, nr_BetaBlocker_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_BetaBlocker)*1), str_split(x,","))))]
data <- data[, nr_BetaBlockerUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("BetaBlocker_d1","BetaBlocker_d2")] 
data <- data[, BetaBlocker_flow_type := ifelse(nr_BetaBlocker_d2 < nr_BetaBlocker_d1 & nr_BetaBlockerUnq_d1d2 > nr_BetaBlocker_d1, "D+S", 
                                               ifelse(nr_BetaBlocker_d2 > nr_BetaBlocker_d1 & nr_BetaBlockerUnq_d1d2 > nr_BetaBlocker_d2, "A+S",
                                                      ifelse(nr_BetaBlocker_d2 < nr_BetaBlocker_d1, "D", 
                                                             ifelse(nr_BetaBlocker_d2 > nr_BetaBlocker_d1, "A", 
                                                                    ifelse(nr_BetaBlocker_d2 == nr_BetaBlocker_d1 & BetaBlocker_d2 != BetaBlocker_d1, "S","-")))))] 



# CalciumBlocker Therapy class - flags
data <- data[, CalciumBlocker_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_CalciumBlocker), str_c(unlist(str_extract_all(x, string_CalciumBlocker)), collapse = ","),"")))]
data <- data[, CalciumBlocker_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_CalciumBlocker), str_c(unlist(str_extract_all(x, string_CalciumBlocker)), collapse = ","),"")))]
data <- data[, nr_CalciumBlocker_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_CalciumBlocker)*1), str_split(x,","))))]
data <- data[, nr_CalciumBlocker_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_CalciumBlocker)*1), str_split(x,","))))]
data <- data[, nr_CalciumBlockerUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("CalciumBlocker_d1","CalciumBlocker_d2")] 
data <- data[, CalciumBlocker_flow_type := ifelse(nr_CalciumBlocker_d2 < nr_CalciumBlocker_d1 & nr_CalciumBlockerUnq_d1d2 > nr_CalciumBlocker_d1, "D+S", 
                                                  ifelse(nr_CalciumBlocker_d2 > nr_CalciumBlocker_d1 & nr_CalciumBlockerUnq_d1d2 > nr_CalciumBlocker_d2, "A+S",
                                                         ifelse(nr_CalciumBlocker_d2 < nr_CalciumBlocker_d1, "D", 
                                                                ifelse(nr_CalciumBlocker_d2 > nr_CalciumBlocker_d1, "A", 
                                                                       ifelse(nr_CalciumBlocker_d2 == nr_CalciumBlocker_d1 & CalciumBlocker_d2 != CalciumBlocker_d1, "S","-")))))] 



# Tricyclic Therapy class - flags
data <- data[, Tricyclic_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Tricyclic), str_c(unlist(str_extract_all(x, string_Tricyclic)), collapse = ","),"")))]
data <- data[, Tricyclic_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Tricyclic), str_c(unlist(str_extract_all(x, string_Tricyclic)), collapse = ","),"")))]
data <- data[, nr_Tricyclic_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Tricyclic)*1), str_split(x,","))))]
data <- data[, nr_Tricyclic_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Tricyclic)*1), str_split(x,","))))]
data <- data[, nr_TricyclicUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Tricyclic_d1","Tricyclic_d2")] 
data <- data[, Tricyclic_flow_type := ifelse(nr_Tricyclic_d2 < nr_Tricyclic_d1 & nr_TricyclicUnq_d1d2 > nr_Tricyclic_d1, "D+S", 
                                             ifelse(nr_Tricyclic_d2 > nr_Tricyclic_d1 & nr_TricyclicUnq_d1d2 > nr_Tricyclic_d2, "A+S",
                                                    ifelse(nr_Tricyclic_d2 < nr_Tricyclic_d1, "D", 
                                                           ifelse(nr_Tricyclic_d2 > nr_Tricyclic_d1, "A", 
                                                                  ifelse(nr_Tricyclic_d2 == nr_Tricyclic_d1 & Tricyclic_d2 != Tricyclic_d1, "S","-")))))] 



# SSRI Therapy class - flags
data <- data[, SSRI_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_SSRI), str_c(unlist(str_extract_all(x, string_SSRI)), collapse = ","),"")))]
data <- data[, SSRI_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_SSRI), str_c(unlist(str_extract_all(x, string_SSRI)), collapse = ","),"")))]
data <- data[, nr_SSRI_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_SSRI)*1), str_split(x,","))))]
data <- data[, nr_SSRI_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_SSRI)*1), str_split(x,","))))]
data <- data[, nr_SSRIUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("SSRI_d1","SSRI_d2")] 
data <- data[, SSRI_flow_type := ifelse(nr_SSRI_d2 < nr_SSRI_d1 & nr_SSRIUnq_d1d2 > nr_SSRI_d1, "D+S", 
                                        ifelse(nr_SSRI_d2 > nr_SSRI_d1 & nr_SSRIUnq_d1d2 > nr_SSRI_d2, "A+S",
                                               ifelse(nr_SSRI_d2 < nr_SSRI_d1, "D", 
                                                      ifelse(nr_SSRI_d2 > nr_SSRI_d1, "A", 
                                                             ifelse(nr_SSRI_d2 == nr_SSRI_d1 & SSRI_d2 != SSRI_d1, "S","-")))))] 


# SNRI Therapy class - flags
data <- data[, SNRI_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_SNRI), str_c(unlist(str_extract_all(x, string_SNRI)), collapse = ","),"")))]
data <- data[, SNRI_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_SNRI), str_c(unlist(str_extract_all(x, string_SNRI)), collapse = ","),"")))]
data <- data[, nr_SNRI_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_SNRI)*1), str_split(x,","))))]
data <- data[, nr_SNRI_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_SNRI)*1), str_split(x,","))))]
data <- data[, nr_SNRIUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("SNRI_d1","SNRI_d2")] 
data <- data[, SNRI_flow_type := ifelse(nr_SNRI_d2 < nr_SNRI_d1 & nr_SNRIUnq_d1d2 > nr_SNRI_d1, "D+S", 
                                        ifelse(nr_SNRI_d2 > nr_SNRI_d1 & nr_SNRIUnq_d1d2 > nr_SNRI_d2, "A+S",
                                               ifelse(nr_SNRI_d2 < nr_SNRI_d1, "D", 
                                                      ifelse(nr_SNRI_d2 > nr_SNRI_d1, "A", 
                                                             ifelse(nr_SNRI_d2 == nr_SNRI_d1 & SNRI_d2 != SNRI_d1, "S","-")))))] 


# Neural Therapy class - flags
data <- data[, Neural_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Neural), str_c(unlist(str_extract_all(x, string_Neural)), collapse = ","),"")))]
data <- data[, Neural_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Neural), str_c(unlist(str_extract_all(x, string_Neural)), collapse = ","),"")))]
data <- data[, nr_Neural_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Neural)*1), str_split(x,","))))]
data <- data[, nr_Neural_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Neural)*1), str_split(x,","))))]
data <- data[, nr_NeuralUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Neural_d1","Neural_d2")] 
data <- data[, Neural_flow_type := ifelse(nr_Neural_d2 < nr_Neural_d1 & nr_NeuralUnq_d1d2 > nr_Neural_d1, "D+S", 
                                          ifelse(nr_Neural_d2 > nr_Neural_d1 & nr_NeuralUnq_d1d2 > nr_Neural_d2, "A+S",
                                                 ifelse(nr_Neural_d2 < nr_Neural_d1, "D", 
                                                        ifelse(nr_Neural_d2 > nr_Neural_d1, "A", 
                                                               ifelse(nr_Neural_d2 == nr_Neural_d1 & Neural_d2 != Neural_d1, "S","-")))))] 


# CGRPOral Therapy class - flags
data <- data[, CGRPOral_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_CGRPOral), str_c(unlist(str_extract_all(x, string_CGRPOral)), collapse = ","),"")))]
data <- data[, CGRPOral_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_CGRPOral), str_c(unlist(str_extract_all(x, string_CGRPOral)), collapse = ","),"")))]
data <- data[, nr_CGRPOral_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_CGRPOral)*1), str_split(x,","))))]
data <- data[, nr_CGRPOral_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_CGRPOral)*1), str_split(x,","))))]
data <- data[, nr_CGRPOralUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("CGRPOral_d1","CGRPOral_d2")] 
data <- data[, CGRPOral_flow_type := ifelse(nr_CGRPOral_d2 < nr_CGRPOral_d1 & nr_CGRPOralUnq_d1d2 > nr_CGRPOral_d1, "D+S", 
                                            ifelse(nr_CGRPOral_d2 > nr_CGRPOral_d1 & nr_CGRPOralUnq_d1d2 > nr_CGRPOral_d2, "A+S",
                                                   ifelse(nr_CGRPOral_d2 < nr_CGRPOral_d1, "D", 
                                                          ifelse(nr_CGRPOral_d2 > nr_CGRPOral_d1, "A", 
                                                                 ifelse(nr_CGRPOral_d2 == nr_CGRPOral_d1 & CGRPOral_d2 != CGRPOral_d1, "S","-")))))] 


# CGRPInjectable Therapy class - flags
data <- data[, CGRPInjectable_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_CGRPInjectable), str_c(unlist(str_extract_all(x, string_CGRPInjectable)), collapse = ","),"")))]
data <- data[, CGRPInjectable_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_CGRPInjectable), str_c(unlist(str_extract_all(x, string_CGRPInjectable)), collapse = ","),"")))]
data <- data[, nr_CGRPInjectable_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_CGRPInjectable)*1), str_split(x,","))))]
data <- data[, nr_CGRPInjectable_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_CGRPInjectable)*1), str_split(x,","))))]
data <- data[, nr_CGRPInjectableUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("CGRPInjectable_d1","CGRPInjectable_d2")] 
data <- data[, CGRPInjectable_flow_type := ifelse(nr_CGRPInjectable_d2 < nr_CGRPInjectable_d1 & nr_CGRPInjectableUnq_d1d2 > nr_CGRPInjectable_d1, "D+S", 
                                                  ifelse(nr_CGRPInjectable_d2 > nr_CGRPInjectable_d1 & nr_CGRPInjectableUnq_d1d2 > nr_CGRPInjectable_d2, "A+S",
                                                         ifelse(nr_CGRPInjectable_d2 < nr_CGRPInjectable_d1, "D", 
                                                                ifelse(nr_CGRPInjectable_d2 > nr_CGRPInjectable_d1, "A", 
                                                                       ifelse(nr_CGRPInjectable_d2 == nr_CGRPInjectable_d1 & CGRPInjectable_d2 != CGRPInjectable_d1, "S","-")))))] 


fwrite(data, "Flows_long_HighGranularity.csv")

data <- data %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))

data %>% filter(p1>=48) %>% filter(s2=="d") %>% group_by(NSAID_flow_type) %>% summarise(n=sum(as.numeric(weight)))
data %>% filter(p1>=48) %>% filter(s2=="d") %>% group_by(WeakOpioid_flow_type) %>% summarise(n=sum(as.numeric(weight)))
data %>% filter(p1>=48) %>% filter(s2=="d") %>% group_by(StrongOpioid_flow_type) %>% summarise(n=sum(as.numeric(weight)))

data %>% filter(p1>=48) %>% filter(s2=="D") %>% group_by(NSAID_flow_type) %>% summarise(n=sum(as.numeric(weight)))
data %>% filter(p1>=48) %>% filter(s2=="D") %>% group_by(WeakOpioid_flow_type) %>% summarise(n=sum(as.numeric(weight)))
data %>% filter(p1>=48) %>% filter(s2=="D") %>% group_by(StrongOpioid_flow_type) %>% summarise(n=sum(as.numeric(weight)))





data <- fread("Flows_long_HighGranularity.csv")

# # # #   Using DRUG GROUPS (instead of individual classes)   # # # #

#Flows table
MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

data2 <- MIG_Flows_Aux._Long[flow == 1 & s1 == s2,.(patient,weight,p1,p2,d1,d2,s1,s2,flow)]

RIME_Ingredients <- fread("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients$drug_id <- unlist(lapply(RIME_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))

string_Sympt <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_group == "Acute"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_group == "Preventative"], collapse = "|"),")\\b")


# Symptomatic Therapy class - flags
data2 <- data2[, Sympt_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Sympt), str_c(unlist(str_extract_all(x, string_Sympt)), collapse = ","),"")))]
data2 <- data2[, Sympt_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Sympt), str_c(unlist(str_extract_all(x, string_Sympt)), collapse = ","),"")))]
data2 <- data2[, nr_Sympt_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Sympt)*1), str_split(x,","))))]
data2 <- data2[, nr_Sympt_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Sympt)*1), str_split(x,","))))]
data2 <- data2[, nr_SymptUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Sympt_d1","Sympt_d2")] 
data2 <- data2[, Sympt_flow_type := ifelse(nr_Sympt_d2 < nr_Sympt_d1 & nr_SymptUnq_d1d2 > nr_Sympt_d1, "D+S", 
                                           ifelse(nr_Sympt_d2 > nr_Sympt_d1 & nr_SymptUnq_d1d2 > nr_Sympt_d2, "A+S",
                                                  ifelse(nr_Sympt_d2 < nr_Sympt_d1, "D", 
                                                         ifelse(nr_Sympt_d2 > nr_Sympt_d1, "A", 
                                                                ifelse(nr_Sympt_d2 == nr_Sympt_d1 & Sympt_d2 != Sympt_d1, "S","-")))))] 


# Acute Therapy class - flags
data2 <- data2[, Acute_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Acute), str_c(unlist(str_extract_all(x, string_Acute)), collapse = ","),"")))]
data2 <- data2[, Acute_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Acute), str_c(unlist(str_extract_all(x, string_Acute)), collapse = ","),"")))]
data2 <- data2[, nr_Acute_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Acute)*1), str_split(x,","))))]
data2 <- data2[, nr_Acute_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Acute)*1), str_split(x,","))))]
data2 <- data2[, nr_AcuteUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Acute_d1","Acute_d2")] 
data2 <- data2[, Acute_flow_type := ifelse(nr_Acute_d2 < nr_Acute_d1 & nr_AcuteUnq_d1d2 > nr_Acute_d1, "D+S", 
                                           ifelse(nr_Acute_d2 > nr_Acute_d1 & nr_AcuteUnq_d1d2 > nr_Acute_d2, "A+S",
                                                  ifelse(nr_Acute_d2 < nr_Acute_d1, "D", 
                                                         ifelse(nr_Acute_d2 > nr_Acute_d1, "A", 
                                                                ifelse(nr_Acute_d2 == nr_Acute_d1 & Acute_d2 != Acute_d1, "S","-")))))] 


# Preventive Therapy class - flags
data2 <- data2[, Prev_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Prev), str_c(unlist(str_extract_all(x, string_Prev)), collapse = ","),"")))]
data2 <- data2[, Prev_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Prev), str_c(unlist(str_extract_all(x, string_Prev)), collapse = ","),"")))]
data2 <- data2[, nr_Prev_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Prev)*1), str_split(x,","))))]
data2 <- data2[, nr_Prev_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Prev)*1), str_split(x,","))))]
data2 <- data2[, nr_PrevUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Prev_d1","Prev_d2")] 
data2 <- data2[, Prev_flow_type := ifelse(nr_Prev_d2 < nr_Prev_d1 & nr_PrevUnq_d1d2 > nr_Prev_d1, "D+S", 
                                          ifelse(nr_Prev_d2 > nr_Prev_d1 & nr_PrevUnq_d1d2 > nr_Prev_d2, "A+S",
                                                 ifelse(nr_Prev_d2 < nr_Prev_d1, "D", 
                                                        ifelse(nr_Prev_d2 > nr_Prev_d1, "A", 
                                                               ifelse(nr_Prev_d2 == nr_Prev_d1 & Prev_d2 != Prev_d1, "S","-")))))] 


fwrite(data2, "Flows_long_HighGranularity_DrugGroups.csv")

data2 <- data2 %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))

data2 %>% filter(p1>=48) %>% filter(s2=="d") %>% group_by(Prev_flow_type) %>% summarise(n=sum(as.numeric(weight)))
data2 %>% filter(p1>=48) %>% filter(s2=="d") %>% group_by(Sympt_flow_type) %>% summarise(n=sum(as.numeric(weight)))


data2 %>% filter(p1>=48) %>% filter(s2=="D") %>% group_by(Prev_flow_type) %>% summarise(n=sum(as.numeric(weight)))
data2 %>% filter(p1>=48) %>% filter(s2=="D") %>% group_by(Acute_flow_type) %>% summarise(n=sum(as.numeric(weight)))
data2 %>% filter(p1>=48) %>% filter(s2=="D") %>% group_by(Sympt_flow_type) %>% summarise(n=sum(as.numeric(weight)))

# ----
#  Persistency Rimegepant vs Triptans (ignore first 3 months since begining of Rimegepant at 45) ---------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "Rimegepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Rimegepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Month >= 48)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these  periods
Rimegepant_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Rimegepant_Periods_MIG)[3] <- "Duration"

Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% left_join(MIG_Drug_Histories %>%  select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct()

library(spatstat)

weighted.mean(Rimegepant_Periods_MIG$Total_duration, Rimegepant_Periods_MIG$weight)  #3.259441
weighted.median(Rimegepant_Periods_MIG$Total_duration, Rimegepant_Periods_MIG$weight)  #1.5


Rimegepant_Periods_MIG %>% group_by(Total_duration) %>% summarise(n = sum(weight))


MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(70{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(70{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(71{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(71{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(72{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(72{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(73{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(73{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(74{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(74{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(75{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(75{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(76{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(76{1})(\\D|$)', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Month >= 48)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these  periods
Triptan_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Triptan_Periods_MIG)[3] <- "Duration"

Triptan_Periods_MIG <- Triptan_Periods_MIG %>% left_join(MIG_Drug_Histories %>%  select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct()

library(spatstat)

weighted.mean(Triptan_Periods_MIG$Total_duration, Triptan_Periods_MIG$weight)  #4.692544
weighted.median(Triptan_Periods_MIG$Total_duration, Triptan_Periods_MIG$weight)  #2.5

Triptan_Periods_MIG %>% group_by(Total_duration) %>% summarise(n = sum(weight))

# ------
#  Persistency Rimegepant vs Triptans (ignore first 3 months since begining of Rimegepant at 51 6months) ---------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "Rimegepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Rimegepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Month >= 51)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these  periods
Rimegepant_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Rimegepant_Periods_MIG)[3] <- "Duration"

Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% left_join(MIG_Drug_Histories %>%  select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct()

library(spatstat)

weighted.mean(Rimegepant_Periods_MIG$Total_duration, Rimegepant_Periods_MIG$weight)  #3.162204
weighted.median(Rimegepant_Periods_MIG$Total_duration, Rimegepant_Periods_MIG$weight)  #1.5


Rimegepant_Periods_MIG %>% group_by(Total_duration) %>% summarise(n = sum(weight))





MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(70{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(70{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(71{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(71{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(72{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(72{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(73{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(73{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(74{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(74{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(75{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(75{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(76{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(76{1})(\\D|$)', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)
 
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Month >= 51)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these  periods
Triptan_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Triptan_Periods_MIG)[3] <- "Duration"

Triptan_Periods_MIG <- Triptan_Periods_MIG %>% left_join(MIG_Drug_Histories %>%  select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct()

library(spatstat)

weighted.mean(Triptan_Periods_MIG$Total_duration, Triptan_Periods_MIG$weight)  #4.016503
weighted.median(Triptan_Periods_MIG$Total_duration, Triptan_Periods_MIG$weight)  #2.5

Triptan_Periods_MIG %>% group_by(Total_duration) %>% summarise(n = sum(weight))

                                                         
# ----
#  Persistency Ubrogepant vs Triptans ---------------------------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('136',.), ~replace(., grepl('136', .), "Ubrogepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ubrogepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Month >= 46)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these  periods
Ubrogepant_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Ubrogepant_Periods_MIG)[3] <- "Duration"

Ubrogepant_Periods_MIG <- Ubrogepant_Periods_MIG %>% left_join(MIG_Drug_Histories %>%  select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct()

library(spatstat)

weighted.mean(Ubrogepant_Periods_MIG$Total_duration, Ubrogepant_Periods_MIG$weight)  #3.663693 (3.659399)
weighted.median(Ubrogepant_Periods_MIG$Total_duration, Ubrogepant_Periods_MIG$weight)  #1.5  (1.5)

Ubrogepant_Periods_MIG %>% group_by(Total_duration) %>% summarise(n = sum(weight))

                                                         

                                                         

# Triptans
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(70{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(70{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(71{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(71{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(72{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(72{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(73{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(73{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(74{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(74{1})(\\D|$)', .), "Triptan"))%>% 
  mutate_if(grepl('(^|\\D)(75{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(75{1})(\\D|$)', .), "Triptan"))%>%
  mutate_if(grepl('(^|\\D)(76{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(76{1})(\\D|$)', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)

MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Month >= 46)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these  periods
Triptan_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Triptan_Periods_MIG)[3] <- "Duration"

Triptan_Periods_MIG <- Triptan_Periods_MIG %>% left_join(MIG_Drug_Histories %>%  select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct()

library(spatstat)

weighted.mean(Triptan_Periods_MIG$Total_duration, Triptan_Periods_MIG$weight)  #5.716839  (5.110806)
weighted.median(Triptan_Periods_MIG$Total_duration, Triptan_Periods_MIG$weight)  #2.5  (2.5)

Triptan_Periods_MIG %>% group_by(Total_duration) %>% summarise(n = sum(weight))

# -----
# Most frequent drugs per patient -----------------
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T)
names(MIG_Drug_Histories)[4] <- "molecule"

# dictionary withh drug groups
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
RIME_Ingredients$molecule <- as.numeric(RIME_Ingredients$molecule) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))

length(unique(MIG_Drug_Histories$patient)) # 198438

Nr_treat_months <- MIG_Drug_Histories %>% select(patient, weight, Month) %>% group_by(patient, weight, Month) %>% distinct() %>% ungroup() %>% group_by(patient, weight) %>% count()
names(Nr_treat_months)[3] <- "Total_treated"

Nr_months_mol <- MIG_Drug_Histories %>% select(patient, weight, molecule, generic_name) %>% group_by(patient, weight, molecule, generic_name) %>% count()
names(Nr_months_mol)[5] <- "Months_Mol"

Months_Molecule_Total <- data.frame(Nr_months_mol %>% left_join(Nr_treat_months))

Months_Molecule_Total <- Months_Molecule_Total %>% mutate(percent = Months_Mol/Total_treated)

Months_Molecule_Total <- data.frame(Months_Molecule_Total %>% group_by(patient) %>% arrange(patient, desc(percent))) 

fwrite(Months_Molecule_Total, "Months_Molecule_Total.txt", sep="\t")

Months_Molecule_Total %>% group_by(patient) %>% slice(1) %>% ungroup() %>% select(percent) %>%
  ggplot(aes(percent))+
  geom_histogram()


data.frame(Months_Molecule_Total %>% group_by(patient) %>% slice(1) %>% filter(percent>=0.5) %>% ungroup() %>% group_by(generic_name) %>%
             mutate(mean_dur = weighted.mean(percent, as.numeric(weight))) %>% group_by(generic_name, mean_dur) %>%
             summarise(pats = sum(as.numeric(weight))) %>% arrange(-pats)) 


data.frame(Months_Molecule_Total %>% group_by(patient) %>% slice(1) %>% filter(percent<0.5) %>% ungroup() %>% group_by(generic_name) %>%
             mutate(mean_dur = weighted.mean(percent, as.numeric(weight))) %>% group_by(generic_name, mean_dur) %>%
             summarise(pats = sum(as.numeric(weight))) %>% arrange(-pats)) 


#QAs
Months_Molecule_Total <- fread("Months_Molecule_Total.txt")
MIG_Comorbiditites <- fread("MIG Comorbidities.txt")
MIG_Comorbiditites <- MIG_Comorbiditites %>% select(patient, weight, cardiovascular_comorbidity, epileptic_comorbidity, pain_comorbidity, psychiatric_comorbidity)

#How many patients use a drug >50% of the time they are treated?
#Which molecules are those?

Months_Molecule_Total_50perc <- Months_Molecule_Total %>% filter(percent>=0.5)

# 15446093 pats use at least 1 drug 50% of the time they are On treatment, supporting the idea of a base therapy
Months_Molecule_Total_50perc %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) #15446093

Months_Molecule_Total_50perc <- Months_Molecule_Total_50perc %>% left_join(MIG_Comorbiditites)

RIME_Ingredients_disease_lookup <- fread("RIME Ingredients disease lookup.txt")

Months_Molecule_Total_50perc <- Months_Molecule_Total_50perc %>% left_join(RIME_Ingredients_disease_lookup)

Months_Molecule_Total_50perc <- Months_Molecule_Total_50perc %>% filter( !(cardiovascular_comorbidity==1 & Comorbidities=="Cardiovascular") )
Months_Molecule_Total_50perc <- Months_Molecule_Total_50perc %>% filter( !(epileptic_comorbidity==1 & Comorbidities=="Epilepsy") )
Months_Molecule_Total_50perc <- Months_Molecule_Total_50perc %>% filter( !(psychiatric_comorbidity==1 & Comorbidities=="Psychiatric") )

length(unique(Months_Molecule_Total_50perc$patient))
Months_Molecule_Total_50perc %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) #10669812


data.frame(Months_Molecule_Total_50perc %>% group_by(generic_name) %>% summarise(pats=sum(weight)) %>% arrange(-pats) %>% 
  left_join(Months_Molecule_Total_50perc %>% group_by(generic_name) %>% 
              summarise(mean_dur=weighted.mean(percent, weight))))

# ----
# Restarts (same stock, same drug, etc) --------------
MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character")

# 9945082 re_starts in the last year
# MIG_Flows_Aux._Long %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48)%>% 
#   filter(re_starts=="1") %>% summarise(pats=sum(as.numeric(weight)))


# Pats with restarts in the last year
Pats_keep <- MIG_Flows_Aux._Long %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% filter(re_starts=="1") %>% select(patient) %>% distinct()

MIG_Flows_Aux._Long <- Pats_keep %>% left_join(MIG_Flows_Aux._Long)

# keep everything until last re_start
MIG_Flows_Aux._Long_2<-MIG_Flows_Aux._Long %>% 
  group_by(patient)%>% slice(1:max(which(re_starts == "1")))

#9945082
# MIG_Flows_Aux._Long_2 %>% ungroup() %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% 
#   filter(re_starts=="1") %>% summarise(pats=sum(as.numeric(weight)))

#pick months of stops and restarts
MIG_Flows_Aux._Long_2 <- MIG_Flows_Aux._Long_2 %>% group_by(patient) %>% filter(stops=="1" | re_starts=="1")

# remove unnecessary cols
MIG_Flows_Aux._Long_2 <- MIG_Flows_Aux._Long_2  %>% select(-c(disease, flow, p1_RxExp, starts))

MIG_Flows_Aux._Long_2 <- MIG_Flows_Aux._Long_2 %>% group_by(patient) %>% 
  mutate(Stock_comp = ifelse(re_starts=="1" & s2==lag(s1), "Same", "diff"))

MIG_Flows_Aux._Long_2 <- MIG_Flows_Aux._Long_2 %>% group_by(patient) %>% 
  mutate(Drugs_comp = ifelse(re_starts=="1" & d2==lag(d1), "Same", "diff"))

MIG_Flows_Aux._Long_2 %>% ungroup() %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% filter(re_starts=="1") %>% 
  group_by(Stock_comp, Drugs_comp) %>% summarise(pats=sum(as.numeric(weight)))


data.frame(MIG_Flows_Aux._Long_2 %>% ungroup() %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% filter(re_starts=="1") %>% 
    group_by(s2, Stock_comp, Drugs_comp) %>% summarise(pats=sum(as.numeric(weight))))


# ----



# Put patients on the highest Box tried over the past 12 months as a proxy of disease severity -------

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0 
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

Ever_treated <- MIG_Drug_Histories_LONG %>% filter(SUM != 0)%>% select(patient)

MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,51:63)
MIG_Box_Histories <- gather(MIG_Box_Histories, Month, Treat, month48:month60, factor_key=TRUE)
MIG_Box_Histories <- MIG_Box_Histories %>% mutate(Treat = str_sub(Treat, 2L, 2L))
names(MIG_Box_Histories)[4] <- "Box"

MIG_Box_Histories <- MIG_Box_Histories %>% mutate(Box = ifelse(Box=="x", 0,
                                                               ifelse(Box=="a",1,
                                                                      ifelse(Box=="A",2,
                                                                             ifelse(Box=="p",3,
                                                                                    ifelse(Box=="d",4,
                                                                                           ifelse(Box=="D",5,
                                                                                                  ifelse(Box=="O",6,7))))))))

MIG_Box_Histories <- MIG_Box_Histories %>% select(-c(Month)) %>% group_by(patient, weight) %>% summarise(across(everything(),max))

Ever_treated %>% inner_join(MIG_Box_Histories) %>% group_by(Box) %>% summarise(pats=sum(as.numeric(weight)))





# Each year, of all the patient-months, how many patient-months do we have for the CGRP class? ------------

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- Ever_treated %>% inner_join(MIG_Drug_Histories)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(Year = ifelse(Month >=1 & Month <=12, 1, 
                                                                  ifelse(Month>=13 & Month <= 24, 2,
                                                                         ifelse(Month>=25 & Month <= 36,3,
                                                                                ifelse(Month >=37 & Month <= 48,4,5)))))

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")


string_NSAID <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "NSAID"], collapse = "|"),")\\b")
string_Analgesic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Analgesic"], collapse = "|"),")\\b")
string_WeakOpioid <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Weak Opioid"], collapse = "|"),")\\b")
string_StrongOpioid <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Strong Opioid"], collapse = "|"),")\\b")
string_Antiemetic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Antiemetic"], collapse = "|"),")\\b")
string_Steroid <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Steroid"], collapse = "|"),")\\b")
string_Sedative <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Sedative"], collapse = "|"),")\\b")
string_Antipsychotic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Antipsychotic"], collapse = "|"),")\\b")
string_Hospitalization <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Hospitalization"], collapse = "|"),")\\b")
string_Ergot <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Ergot"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Triptan"], collapse = "|"),")\\b")
string_Ditan <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Ditan"], collapse = "|"),")\\b")
string_MuscleRelaxant <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Muscle Relaxant"], collapse = "|"),")\\b")
string_Antiepileptic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Antiepileptic"], collapse = "|"),")\\b")
string_Cardiovascular <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Cardiovascular"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Beta Blocker"], collapse = "|"),")\\b")
string_CalciumBlocker <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Calcium Blocker"], collapse = "|"),")\\b")
string_Tricyclic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Tricyclic"], collapse = "|"),")\\b")
string_SSRI <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "SSRI"], collapse = "|"),")\\b")
string_SNRI <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "SNRI"], collapse = "|"),")\\b")
string_Neural <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Neural"], collapse = "|"),")\\b")
string_CGRPOral <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRPInjectable <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")


MIG_Drug_Histories %>% group_by(Year) %>% summarise(n=sum(as.numeric(weight)))


MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(CGRP_Oral_Status = ifelse(grepl(string_CGRPOral, Treat),"Yes", "No"))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(CGRP_Injectable_Status = ifelse(grepl(string_CGRPInjectable, Treat),"Yes", "No"))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(Triptan_Status = ifelse(grepl(string_Triptan, Treat),"Yes", "No"))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(NSAID_Status = ifelse(grepl(string_NSAID, Treat),"Yes", "No"))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(Steroid_Status = ifelse(grepl(string_Steroid, Treat),"Yes", "No"))



MIG_Drug_Histories %>% group_by(Year, CGRP_Injectable_Status) %>% summarise(n=sum(as.numeric(weight)))



Pats_Year2 <- MIG_Drug_Histories %>% filter(Year==2) %>% filter(CGRP_Injectable_Status=="Yes") %>% select(patient) %>% distinct()
Pats_Year3 <- MIG_Drug_Histories %>% filter(Year==3) %>% filter(CGRP_Injectable_Status=="Yes") %>% select(patient) %>% distinct()
Pats_Year4 <- MIG_Drug_Histories %>% filter(Year==4) %>% filter(CGRP_Injectable_Status=="Yes") %>% select(patient) %>% distinct()
Pats_Year5 <- MIG_Drug_Histories %>% filter(Year==5) %>% filter(CGRP_Injectable_Status=="Yes") %>% select(patient) %>% distinct()


Pats_Year2 %>% left_join(MIG_Drug_Histories) %>% filter(Year==2) %>% filter(CGRP_Injectable_Status=="Yes") %>% summarise(n=sum(as.numeric(weight)))

Pats_Year3 %>% left_join(MIG_Drug_Histories) %>% filter(Year==3)  %>% filter(CGRP_Injectable_Status=="Yes") %>% summarise(n=sum(as.numeric(weight)))

Pats_Year4 %>% left_join(MIG_Drug_Histories) %>% filter(Year==4)  %>% filter(CGRP_Injectable_Status=="Yes") %>%  summarise(n=sum(as.numeric(weight)))


Pats_Year5 %>% left_join(MIG_Drug_Histories) %>% filter(Year==5)  %>%  filter(CGRP_Injectable_Status=="Yes") %>%   summarise(n=sum(as.numeric(weight)))


# ----
# Each year, of all the patient-months, how many patient-months do we have for the Triptan class? ------------

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0 
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

Ever_treated <- MIG_Drug_Histories_LONG %>% filter(SUM != 0)%>% select(patient)



RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- Ever_treated %>% inner_join(MIG_Drug_Histories)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(Year = ifelse(Month >=1 & Month <=12, 1, 
                                                                  ifelse(Month>=13 & Month <= 24, 2,
                                                                         ifelse(Month>=25 & Month <= 36,3,
                                                                                ifelse(Month >=37 & Month <= 48,4,5)))))

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")


string_NSAID <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "NSAID"], collapse = "|"),")\\b")
string_Analgesic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Analgesic"], collapse = "|"),")\\b")
string_WeakOpioid <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Weak Opioid"], collapse = "|"),")\\b")
string_StrongOpioid <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Strong Opioid"], collapse = "|"),")\\b")
string_Antiemetic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Antiemetic"], collapse = "|"),")\\b")
string_Steroid <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Steroid"], collapse = "|"),")\\b")
string_Sedative <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Sedative"], collapse = "|"),")\\b")
string_Antipsychotic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Antipsychotic"], collapse = "|"),")\\b")
string_Hospitalization <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Hospitalization"], collapse = "|"),")\\b")
string_Ergot <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Ergot"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Triptan"], collapse = "|"),")\\b")
string_Ditan <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Ditan"], collapse = "|"),")\\b")
string_MuscleRelaxant <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Muscle Relaxant"], collapse = "|"),")\\b")
string_Antiepileptic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Antiepileptic"], collapse = "|"),")\\b")
string_Cardiovascular <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Cardiovascular"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Beta Blocker"], collapse = "|"),")\\b")
string_CalciumBlocker <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Calcium Blocker"], collapse = "|"),")\\b")
string_Tricyclic <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Tricyclic"], collapse = "|"),")\\b")
string_SSRI <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "SSRI"], collapse = "|"),")\\b")
string_SNRI <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "SNRI"], collapse = "|"),")\\b")
string_Neural <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "Neural"], collapse = "|"),")\\b")
string_CGRPOral <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRPInjectable <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")


MIG_Drug_Histories %>% group_by(Year) %>% summarise(n=sum(as.numeric(weight)))


MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(Triptan_Status = ifelse(grepl(string_Triptan, Treat),"Yes", "No"))

MIG_Drug_Histories %>% group_by(Year, Triptan_Status) %>% summarise(n=sum(as.numeric(weight)))


Pats_Year1 <- MIG_Drug_Histories %>% filter(Year==1) %>% filter(Triptan_Status=="Yes") %>% select(patient) %>% distinct()
Pats_Year2 <- MIG_Drug_Histories %>% filter(Year==2) %>% filter(Triptan_Status=="Yes") %>% select(patient) %>% distinct()
Pats_Year3 <- MIG_Drug_Histories %>% filter(Year==3) %>% filter(Triptan_Status=="Yes") %>% select(patient) %>% distinct()
Pats_Year4 <- MIG_Drug_Histories %>% filter(Year==4) %>% filter(Triptan_Status=="Yes") %>% select(patient) %>% distinct()
Pats_Year5 <- MIG_Drug_Histories %>% filter(Year==5) %>% filter(Triptan_Status=="Yes") %>% select(patient) %>% distinct()


Pats_Year1 %>% left_join(MIG_Drug_Histories) %>% filter(Year==1) %>% filter(Triptan_Status=="Yes") %>% summarise(n=sum(as.numeric(weight)))


Pats_Year2 %>% left_join(MIG_Drug_Histories) %>% filter(Year==2) %>% filter(Triptan_Status=="Yes") %>% summarise(n=sum(as.numeric(weight)))


Pats_Year3 %>% left_join(MIG_Drug_Histories) %>% filter(Year==3)  %>% filter(Triptan_Status=="Yes") %>% summarise(n=sum(as.numeric(weight)))


Pats_Year4 %>% left_join(MIG_Drug_Histories) %>% filter(Year==4)  %>% filter(Triptan_Status=="Yes") %>%  summarise(n=sum(as.numeric(weight)))

Pats_Year5 %>% left_join(MIG_Drug_Histories) %>% filter(Year==5)  %>%  filter(Triptan_Status=="Yes") %>%   summarise(n=sum(as.numeric(weight)))

#
# Number of drugs and therapy lines per MAX Stock  ---------------------------------------------------
#Import MAX stock at month60

Max_stock <- fread("Max_stock_last_12m.txt", colClasses = "character")

# average nr of drugs on m60

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease)) %>% select(1,2,62)

Max_stock <- Max_stock %>% left_join(MIG_Drug_Histories)

Max_stock <- Max_stock %>% filter(month60 != "-")
Max_stock <- separate_rows(Max_stock, month60, sep = ",", convert=T)
Max_stock <- Max_stock %>% group_by(patient) %>% mutate(drugs_n = n())
Max_stock <- Max_stock %>% select(-c(month60))
Max_stock <- Max_stock %>% distinct()

Max_stock %>% ungroup() %>% group_by(Box) %>% summarise(n= weighted.mean(drugs_n, as.numeric(weight)))



Max_stock %>% ungroup() %>% mutate(Box=ifelse(Box=="2"|Box=="3","Mod", ifelse(Box=="4"|Box=="5","SevPrev",ifelse(Box=="6"|Box=="7","SevCGRP","Mild")))) %>% group_by(Box) %>% summarise(n= weighted.mean(drugs_n, as.numeric(weight)))


# avergae number of lines over the 5 years
Max_stock <- fread("Max_stock_last_12m.txt", colClasses = "character")


MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% select(2,3,63)


Max_stock <- Max_stock %>% left_join(MIG_nrLines_Histories, by = c("patient"="patient"))

Max_stock %>% group_by(Box) %>% summarise(n=weighted.mean(as.numeric(month60), as.numeric(weight.x)))


Max_stock %>% mutate(Box=ifelse(Box=="2"|Box=="3","Mod", ifelse(Box=="4"|Box=="5","SevPrev",ifelse(Box=="6"|Box=="7","SevCGRP","Mild")))) %>% group_by(Box) %>% summarise(n=weighted.mean(as.numeric(month60), as.numeric(weight.x)))

# ----
# Number of flows after starting CGRP vs no Start---------------------------------
# Pick patient on lines 9th to 10th, if shitf
occurs before m48
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_nrLines_Histories$Month <- as.character(MIG_nrLines_Histories$Month)
MIG_nrLines_Histories$Month <- parse_number(MIG_nrLines_Histories$Month)
MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)

temp <- MIG_nrLines_Histories %>% group_by(patient) %>% filter((Treat==5 & lag(Treat)==4)|(Treat==4 & lead(Treat)==5)) %>% arrange(patient)

Pats_9_10_BEFORE48 <- temp %>% group_by(patient) %>% filter(max(Month)<=48)

# pick when pats first start CGRP Injectable, check whether it was from line 9 to 10
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
string_CGRPInjectable <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")
string_CGRP_Oral <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "CGRP Oral"], collapse = "|"),")\\b")

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(Treat = ifelse(grepl(string_CGRP_Oral, Treat),1,0))

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, Month, Treat) %>% group_by(patient) %>% mutate(CGRFP_Inj_Exp=cumsum(Treat))
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(CGRFP_Inj_Exp = ifelse(CGRFP_Inj_Exp==0,0,1))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, Month, CGRFP_Inj_Exp)

Pats_9_10_BEFORE48 <-Pats_9_10_BEFORE48 %>% left_join(MIG_Drug_Histories, by=c("patient"="patient", "Month"="Month"))

Pats_9_10_BEFORE48_CGRP_Status <- Pats_9_10_BEFORE48 %>% group_by(patient) %>% 
  mutate(CGRP_Start = ifelse(first(CGRFP_Inj_Exp)==0 & last(CGRFP_Inj_Exp)==1, "New_Start", "No_CGRP"))

Pats_9_10_BEFORE48_CGRP_Status <- Pats_9_10_BEFORE48_CGRP_Status %>% group_by(patient) %>% filter(Month==max(Month)) %>% select(-c(disease))
Pats_9_10_BEFORE48_CGRP_Status <- Pats_9_10_BEFORE48_CGRP_Status %>% select(-c("CGRFP_Inj_Exp"))
Pats_9_10_BEFORE48_CGRP_Status <- Pats_9_10_BEFORE48_CGRP_Status %>% select(-c("Treat"))

# check how many flows they had in the 12 months after the Month of 9th to 10 line ~CGRP Status
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, p2, flow) %>% mutate(p2 = as.numeric(p2))

temp <- data.frame(Pats_9_10_BEFORE48_CGRP_Status %>% left_join(MIG_Flows_Aux._Long, by=c("patient"="patient")))
temp <- temp %>% filter(p2>=Month & p2 <= (Month+12))

temp %>% group_by(patient, weight, CGRP_Start) %>% summarise(total_flows=sum(as.numeric(flow))) %>%
  ungroup() %>% group_by(CGRP_Start) %>% summarise(mean_flow=weighted.median(total_flows, as.numeric(weight)))

# -------
# Flows table summary by Severity and Line of therapy ------------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, s1, s2, flow)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(flow=="1")
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(weight=as.numeric(weight)) %>% mutate(p1=as.numeric(p1)) %>% mutate(p2=as.numeric(p2))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p1>=48)

Max_stock <- fread("Max_stock_last_12m.txt", colClasses = "character")
Max_stock <- Max_stock %>% select(-c(weight))

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% left_join(Max_stock)

MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_nrLines_Histories$Month <- as.character(MIG_nrLines_Histories$Month)
MIG_nrLines_Histories$Month <- parse_number(MIG_nrLines_Histories$Month)
MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)
MIG_nrLines_Histories <- MIG_nrLines_Histories %>% select(patient, Month, Treat) %>% filter(Month>=48)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% left_join(MIG_nrLines_Histories, by=c("patient"="patient", "p2"="Month"))

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(Box= ifelse(Box=="1", "Mild", 
                                                                  ifelse(Box=="2"|Box=="3", "Moderate", "Severe")))




n_pats <- MIG_Flows_Aux._Long %>% group_by(s1, s2, Box, Treat) %>% count() %>% ungroup() %>% select(n)

Summary_pivot <- MIG_Flows_Aux._Long %>% group_by(s1, s2, Box, Treat) %>% summarise(population=sum(weight)) %>% bind_cols(n_pats)



Summary_pivot %>% ungroup() %>% group_by(s1,s2) %>% summarise(total=sum(population)) 

fwrite(Summary_pivot, "US_MIG_Flows_12m_Severity_LOT.txt")


US_MIG_Flows_12m_Severity_LOT <- fread("US_MIG_Flows_12m_Severity_LOT.txt")

US_MIG_Flows_12m_Severity_LOT <- US_MIG_Flows_12m_Severity_LOT %>% mutate(s1 = ifelse(s1=="x",0,
                                                                                      ifelse(s1=="a",1,
                                                                                             ifelse(s1=="A",2,
                                                                                                    ifelse(s1=="p",3,
                                                                                                           ifelse(s1=="d",4,
                                                                                                                  ifelse(s1=="D",5,
                                                                                                                         ifelse(s1=="O",6,
                                                                                                                                ifelse(s1=="I",7,s1))))))))) %>%
  mutate(s2 = ifelse(s2=="x",0,
                     ifelse(s2=="a",1,
                            ifelse(s2=="A",2,
                                   ifelse(s2=="p",3,
                                          ifelse(s2=="d",4,
                                                 ifelse(s2=="D",5,
                                                        ifelse(s2=="O",6,
                                                               ifelse(s2=="I",7,s2)))))))))

fwrite(US_MIG_Flows_12m_Severity_LOT, "US_MIG_Flows_12m_Severity_LOT.txt", sep="\t")


US_MIG_Flows_12m_Severity_LOT_individual <- fread("US_MIG_Flows_12m_Severity_LOT_individual.txt")


US_MIG_Flows_12m_Severity_LOT_individual <- US_MIG_Flows_12m_Severity_LOT_individual %>% mutate(s1 = ifelse(s1=="x",0,
                                                                                      ifelse(s1=="a",1,
                                                                                             ifelse(s1=="A",2,
                                                                                                    ifelse(s1=="p",3,
                                                                                                           ifelse(s1=="d",4,
                                                                                                                  ifelse(s1=="D",5,
                                                                                                                         ifelse(s1=="O",6,
                                                                                                                                ifelse(s1=="I",7,s1))))))))) %>%
  mutate(s2 = ifelse(s2=="x",0,
                     ifelse(s2=="a",1,
                            ifelse(s2=="A",2,
                                   ifelse(s2=="p",3,
                                          ifelse(s2=="d",4,
                                                 ifelse(s2=="D",5,
                                                        ifelse(s2=="O",6,
                                                               ifelse(s2=="I",7,s2)))))))))


fwrite(US_MIG_Flows_12m_Severity_LOT_individual, "US_MIG_Flows_12m_Severity_LOT_individual.txt", sep="\t")

# Patient Segments Joaquim (flows last 12m, Neurologist) ----------------------
Patient_segments <- fread("Patient segments.txt", colClasses = "character")
names(Patient_segments)[1] <- "patient"

patients_to_track <- Patient_segments %>% select(patient)

MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_Flows_Aux._Long <- patients_to_track %>% inner_join(MIG_Flows_Aux._Long)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>%  mutate(p1=as.numeric(p1))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(p1>=48)  

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, stops, flow)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% filter(stops=="0") %>% group_by(patient) %>% summarise(flows_total=sum(as.numeric(flow)))

Patient_segments <- Patient_segments %>% left_join(MIG_Flows_Aux._Long)

fwrite(Patient_segments, "Patient_segments.txt", sep="\t")

Patient_segments <- fread("Patient_segments.txt", colClasses = "character")
patients_to_track <- Patient_segments %>% select(patient)


Physicians_Vanguard_Lookup <- read.csv("Physicians_Vanguard_Lookup.csv", colClasses = "character", stringsAsFactors = FALSE)
removeQuotes <- function(x) gsub("\'", "", x)
Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% mutate_if(is.character, removeQuotes)
names(Physicians_Vanguard_Lookup)[1] <- "specialty"

MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 
names(MIG_Doses_BIG)[4] <- "patient"

MIG_Doses_BIG <- patients_to_track %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct

MIG_Doses_BIG <- MIG_Doses_BIG  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
MIG_Doses_BIG <- MIG_Doses_BIG %>% distinct()

Patient_segments <- Patient_segments %>% left_join(MIG_Doses_BIG)

Patient_segments <- Patient_segments %>% select(-c("Total flows", "Neurologist?"))

fwrite(Patient_segments, "Patient_segments.txt", sep="\t")

Patient_segments <- fread("Patient_segments.txt")

Patient_segments <- Patient_segments %>% mutate(Physician = ifelse(Physician=="NEUROLOGIST","NEUROLOGIST", "other"))

fwrite(Patient_segments, "Patient_segments.txt", sep="\t")

# -----

# ----------
# NEW   - Put patients on the highest Box tried over the past 12 months as a proxy of disease severity -------

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0 
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

Ever_treated <- MIG_Drug_Histories_LONG %>% filter(SUM != 0)%>% select(patient)

MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,52:63)
MIG_Box_Histories <- gather(MIG_Box_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Box_Histories <- MIG_Box_Histories %>% mutate(Treat = str_sub(Treat, 2L, 2L))
names(MIG_Box_Histories)[4] <- "Box"

MIG_Box_Histories <- MIG_Box_Histories %>% mutate(Box = ifelse(Box=="x", 0,
                                                               ifelse(Box=="a",1,
                                                                      ifelse(Box=="A",2,
                                                                             ifelse(Box=="p",3,
                                                                                    ifelse(Box=="d",4,
                                                                                           ifelse(Box=="D",5,
                                                                                                  ifelse(Box=="O",6,7))))))))

MIG_Box_Histories <- MIG_Box_Histories %>% select(-c(Month)) %>% group_by(patient, weight) %>% summarise(across(everything(),max))

Ever_treated %>% inner_join(MIG_Box_Histories) %>% group_by(Box) %>% summarise(pats=sum(as.numeric(weight)))



MAX_stock_12m <- Ever_treated %>% inner_join(MIG_Box_Histories) 



# Change patients that were on Preventive Only
Preventive_only_Pats <- MAX_stock_12m %>% filter(Box==3) # 3112866.

# Get all drugs they had
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, month49:month60)

Preventive_only_Pats <- Preventive_only_Pats %>% left_join(MIG_Drug_Histories)
Preventive_only_Pats <- gather(Preventive_only_Pats, Month, Treat, month49:month60, factor_key=TRUE)
Preventive_only_Pats <- separate_rows(Preventive_only_Pats, Treat, sep = ",", convert=T)
Preventive_only_Pats <- Preventive_only_Pats %>% select(-c(Month)) %>% group_by(patient) %>% filter(Treat!="-")%>% distinct()

# dictionary with drugs
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

Preventive_only_Pats <- Preventive_only_Pats %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_group), by=c("Treat"="molecule"))

# Comorbidity category for each drug 
RIME_Ingredients_disease_lookup <- fread("RIME Ingredients disease lookup.txt")

Preventive_only_Pats <- Preventive_only_Pats %>% left_join(RIME_Ingredients_disease_lookup %>% select(-c(drug_class)))

# Comorbidities
MIG_Comorbiditites <- fread("MIG Comorbidities.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
MIG_Comorbiditites <- MIG_Comorbiditites %>% select(patient, cardiovascular_comorbidity, epileptic_comorbidity, psychiatric_comorbidity, pain_comorbidity)

Preventive_only_Pats <- Preventive_only_Pats %>% left_join(MIG_Comorbiditites)
Preventive_only_Pats <- Preventive_only_Pats %>% ungroup()

Preventive_only_Pats <- Preventive_only_Pats %>% filter( !(cardiovascular_comorbidity=="1" & Comorbidities=="Cardiovascular") )
Preventive_only_Pats <- Preventive_only_Pats %>% filter( !(epileptic_comorbidity=="1" & Comorbidities=="Epilepsy") )
Preventive_only_Pats <- Preventive_only_Pats %>% filter( !(psychiatric_comorbidity=="1" & Comorbidities=="Psychiatric") )

Preventive_only_Pats <- Preventive_only_Pats %>% mutate(drug_group=ifelse(drug_group=="Symptomatic",1,
                                                                          ifelse(drug_group=="Acute",2,3)))

Preventive_only_Pats <- Preventive_only_Pats %>% select(patient, weight, Box, drug_group) %>% 
  group_by(patient) %>% filter(drug_group==max(drug_group)) %>% distinct()

Preventive_only_Pats <- Preventive_only_Pats %>% mutate(New_Max_box=drug_group)

Preventive_only_Pats <- Preventive_only_Pats %>% select(patient, Box, New_Max_box)

MAX_stock_12m %>% left_join(Preventive_only_Pats) %>% 
  mutate(New_Box=ifelse(Box!=3,Box,
                        ifelse(is.na(New_Max_box),0,New_Max_box))) %>% 
  group_by(New_Box) %>% summarise(n=sum(as.numeric(weight)))


MAX_stock_12m_2 <- MAX_stock_12m %>% left_join(Preventive_only_Pats) %>%
  mutate(New_Box=ifelse(Box!=3,Box,
                        ifelse(is.na(New_Max_box),0,New_Max_box)))

MAX_stock_12m_2 <- MAX_stock_12m_2 %>% select(patient, weight, Box, New_Box)


# Change patients that were on Preventive + Symptomatic 
Preventive_Sympt_Pats <- MAX_stock_12m_2 %>% filter(New_Box==4) #4954857

# Get all drugs they had
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, month49:month60)

Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% left_join(MIG_Drug_Histories)
Preventive_Sympt_Pats <- gather(Preventive_Sympt_Pats, Month, Treat, month49:month60, factor_key=TRUE)
Preventive_Sympt_Pats <- separate_rows(Preventive_Sympt_Pats, Treat, sep = ",", convert=T)
Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% group_by(patient) %>% filter(Treat!="-")%>% distinct()

# dictionary with drugs
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_group), by=c("Treat"="molecule"))

# Comorbidity category for each drug 
RIME_Ingredients_disease_lookup <- fread("RIME Ingredients disease lookup.txt")

Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% left_join(RIME_Ingredients_disease_lookup %>% select(-c(drug_class)))

# Comorbidities
MIG_Comorbiditites <- fread("MIG Comorbidities.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
MIG_Comorbiditites <- MIG_Comorbiditites %>% select(patient, cardiovascular_comorbidity, epileptic_comorbidity, psychiatric_comorbidity, pain_comorbidity)

Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% left_join(MIG_Comorbiditites)
Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% ungroup()

Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% filter( !(cardiovascular_comorbidity=="1" & Comorbidities=="Cardiovascular") )
Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% filter( !(epileptic_comorbidity=="1" & Comorbidities=="Epilepsy") )
Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% filter( !(psychiatric_comorbidity=="1" & Comorbidities=="Psychiatric") )

Preventive_Sympt_Pats <- Preventive_Sympt_Pats %>% select(patient, weight, Box, New_Box, Month, drug_group)

temp <- Preventive_Sympt_Pats %>% distinct() %>% group_by(patient, weight, Box, New_Box, Month) %>% mutate(drug_group = paste(drug_group, collapse=","))

temp <- temp %>% group_by(patient, weight, Box, New_Box, Month) %>% mutate(New_Stock = ifelse(drug_group=="Preventative,Symptomatic",4,
                                                                                              ifelse(drug_group=="Preventative",3,
                                                                                                     ifelse(drug_group=="Acute"|drug_group=="Acute,Symptomatic",2,
                                                                                                            ifelse(drug_group=="Symptomatic",1,NA)))))

temp <-temp %>% ungroup() %>% select(patient, weight, Box, New_Stock) %>% group_by(patient) %>% filter(New_Stock==max(New_Stock))
temp <- temp %>% distinct()

MAX_stock_12m_2 %>% left_join(temp) %>% 
  mutate(New_Stock=ifelse(New_Box!=4,New_Box,
                          ifelse(is.na(New_Stock),0,New_Stock))) %>% 
  group_by(New_Stock) %>% summarise(n=sum(as.numeric(weight)))



MAX_stock_12m_3 <- MAX_stock_12m_2 %>% left_join(temp) %>% 
  mutate(New_Stock=ifelse(New_Box!=4,New_Box,
                          ifelse(is.na(New_Stock),0,New_Stock)))

# -----
# How many had an Migraine Dx in the last year? -----------
MAX_stock_12m_3

MIG_Demographics <- fread("MIG Demographics.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
MIG_Demographics <- MIG_Demographics %>% select(patid, migraine_latest)

MAX_stock_12m_3 <- MAX_stock_12m_3 %>% left_join(MIG_Demographics, by=c("patient"="patid"))

MAX_stock_12m_3 <- MAX_stock_12m_3 %>% mutate(migraine_latest = as.Date(migraine_latest)) %>% mutate(Dx_last12m = ifelse(migraine_latest>="2020-05-01", "Yes", "No"))


MAX_stock_12m_3 %>% filter(Box==3 & New_Box!=3) %>% group_by(Dx_last12m, New_Box) %>% summarise(n=sum(as.numeric(weight)))

MAX_stock_12m_3 %>% filter(New_Box==4 & New_Stock!=4) %>% group_by(Dx_last12m, New_Stock) %>% summarise(n=sum(as.numeric(weight)))


MAX_stock_12m_3 <- MAX_stock_12m_3 %>% mutate(New_Stock_2 = ifelse( (New_Stock!=New_Box) & Dx_last12m=="Yes", New_Box, New_Stock))
MAX_stock_12m_3 <- MAX_stock_12m_3 %>% mutate(New_Stock_2 = ifelse( (New_Box!=Box) & Dx_last12m=="Yes", Box, New_Stock_2))


MAX_stock_12m_3 %>% group_by(New_Stock_2) %>% summarise(n=sum(as.numeric(weight)))



fwrite(MAX_stock_12m_3, "Max_Stock_trials.txt")

# -----
# Create new cateroization stock for July 2021 ---------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0 
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)
MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

Ever_treated <- MIG_Drug_Histories_LONG %>% filter(SUM != 0)%>% select(patient)

MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(2,3,63)
MIG_Box_Histories <- MIG_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))
names(MIG_Box_Histories)[3] <- "original_last_Box"

MIG_Box_Histories <- MIG_Box_Histories %>% inner_join(Ever_treated)
MIG_Box_Histories <- MIG_Box_Histories %>% select(-c(weight))

MIG_Box_Histories <- MAX_stock_12m_3 %>% left_join(MIG_Box_Histories)

MIG_Box_Histories <- MIG_Box_Histories %>% mutate(original_last_Box = ifelse(original_last_Box=="x",0,
                                                                             ifelse(original_last_Box=="a",1,
                                                                                    ifelse(original_last_Box=="A",2,
                                                                                           ifelse(original_last_Box=="p",3,
                                                                                                  ifelse(original_last_Box=="d",4,
                                                                                                         ifelse(original_last_Box=="D",5,
                                                                                                                ifelse(original_last_Box=="O",6,
                                                                                                                       ifelse(original_last_Box=="I",7,NA)))))))))                                                        ))                                 

names(MIG_Box_Histories)[3] <- "Max_Box_12m"                   
names(MIG_Box_Histories)[4] <- "Max_Box_12m_3comorb"                   
names(MIG_Box_Histories)[5] <- "Max_Box_12m_4comorb"                   
names(MIG_Box_Histories)[8] <- "Max_Box_12m_plusDx"  


MIG_Box_Histories <- MIG_Box_Histories %>% mutate(New_BoxJul21 = ifelse(Max_Box_12m_plusDx==0|Max_Box_12m_plusDx==1, "Mild", original_last_Box ))

MIG_Box_Histories %>% group_by(New_BoxJul21) %>% summarise(pats=sum(as.numeric(weight)))



New_BoxJul21 <- MIG_Box_Histories %>% select(patient, weight, New_BoxJul21)













# -----
# Concomitant classes at m60 -------------------------------------------------------------------

#drugs m60
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease)) %>% select(1,2,62)
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)

MIG_Drug_Histories %>% group_by(New_BoxJul21) %>% summarise(n=sum(as.numeric(weight))) %>% mutate(percent = n/18330405)



MIG_Drug_Histories %>% mutate(combo = ifelse(grepl(",",month60), "Combo", "Mono")) %>%
  group_by(New_BoxJul21, combo) %>% summarise(n=sum(as.numeric(weight)))



MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T)
names(MIG_Drug_Histories)[4] <- "molecule"

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

data.frame(MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, drug_group)) %>%
             select(patient, weight, New_BoxJul21, drug_group) %>% distinct() %>%
             group_by(New_BoxJul21, drug_group) %>% summarise(n=sum(as.numeric(weight))))


# -----

# Number of drugs per stock m60 ---------------------------------------------------

New_BoxJul21
#drugs m60
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease)) %>% select(1,2,62)

MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(month60 != "-")
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(drugs_n = n())
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(month60))
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()
MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(drugs_n = ifelse(drugs_n>=6,6, drugs_n))

MIG_Drug_Histories %>% ungroup() %>% group_by(New_BoxJul21) %>% summarise(n= weighted.mean(drugs_n, as.numeric(weight)))

# -----
# Number of lines per stock -----------

nrLines <- fread("MIG_nrLines_Histories.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
nrLines <- nrLines %>% select(2, 63)

New_BoxJul21 %>% left_join(nrLines) %>% group_by(New_BoxJul21) %>% summarise(n= weighted.mean(as.numeric(month60), as.numeric(weight)))
New_BoxJul21 %>% left_join(nrLines) %>% summarise(n= weighted.mean(as.numeric(month60), as.numeric(weight)))

fwrite(New_BoxJul21, "New_BoxJul21.txt")

# -----
# NEW switch Matrix with Mild vs Mod_Severe ----------

New_BoxJul21 <- fread("New_BoxJul21.txt")

MIG_Flows_Aux._Long_v2 <- fread("MIG_Flows_Aux._Long_v2.txt")

MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% mutate(p1 = as.numeric(p1)) %>% filter(p1 >=48)
MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% filter(flow == "1")

MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% left_join(New_BoxJul21)

MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% mutate(s1=ifelse(New_BoxJul21=="Mild","Mild",s1)) %>%
  mutate(s2=(ifelse(New_BoxJul21=="Mild","Mild",s2)))

temp_matrix <- data.frame(MIG_Flows_Aux._Long_v2 %>% group_by(s1, s2) %>% 
                            mutate(s1 = factor(s1, levels = c("Mild", "x", "a", "A", "p", "d", "D", "O", "I"))) %>%
                            mutate(s2 = factor(s2, levels = c("Mild", "x", "a", "A", "p", "d", "D", "O", "I"))) %>%
                            summarise(n=sum(as.numeric(weight)))) %>%
  spread(key = s2, value = n)

fwrite(temp_matrix, "New_Flow_Matrix_MildVSModSev.txt", sep="\t")

# -------
# Flows In / Out Rimegepant ---------------

MIG_Flows_Aux._Long_v2 %>% filter(p1 >=48) %>% filter(!grepl("135",d1)) %>% 
  filter(grepl("135",d2)) %>% group_by(s1) %>% summarise(pats=sum(as.numeric(weight)))



MIG_Flows_Aux._Long_v2 %>% filter(p1 >=48) %>% filter(grepl("135",d1)) %>% 
  filter(!grepl("135",d2)) %>%  group_by(s2) %>% summarise(pats=sum(as.numeric(weight)))


# ------
# Marimekko Stock vs Nr of lines ----------

nrLines <- fread("MIG_nrLines_Histories.txt")
nrLines <- nrLines %>% select(2, 63)

nrLines <- New_BoxJul21 %>% left_join(nrLines)

nrLines %>% group_by(month60) %>% summarise(n=sum(as.numeric(weight)))

weighted.mean(nrLines$month60, nrLines$weight) # 9.021561

nrLines %>% group_by(New_BoxJul21) %>% summarise(n=weighted.mean(month60, weight))



nrLines <- nrLines %>% mutate(month60 = ifelse(month60>=6, 6, month60))

nrLines %>% group_by(New_BoxJul21, month60) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key = month60, value = n)

# ----
# Number of drugs per stock on m60 --------

New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease)) %>% select(1,2,62)
MIG_Drug_Histories$patient <- as.character(MIG_Drug_Histories$patient)
MIG_Drug_Histories$weight <- as.numeric(MIG_Drug_Histories$weight)

MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(month60 != "-")
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(drugs_n = n())
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(month60))
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()
MIG_Drug_Histories %>% ungroup() %>% group_by(New_BoxJul21) %>% summarise(n= weighted.mean(drugs_n, as.numeric(weight)))

MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(drugs_n = ifelse(drugs_n>=6,6, drugs_n))

data.frame(MIG_Drug_Histories %>% ungroup() %>% group_by(New_BoxJul21, drugs_n) %>% summarise(n=sum(as.numeric(weight)))) %>%
  spread(key=New_BoxJul21, value=n)

# -----
# Number of switches per stock las 12 months MOD SEV -----------


New_BoxJul21 <- fread("New_BoxJul21.txt")

MIG_Flows_Aux._Long_v2 <- fread("MIG_Flows_Aux._Long_v2.txt")

MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow, stops)
MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% mutate(p1 = as.numeric(p1)) %>% filter(p1 >=48)
#MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% filter(flow == "1" & stops == 0)

MIG_Flows_Aux._Long_v2 <- New_BoxJul21  %>% left_join(MIG_Flows_Aux._Long_v2)

MIG_Flows_Aux._Long_v2 <- MIG_Flows_Aux._Long_v2 %>% mutate(s1=ifelse(New_BoxJul21=="Mild","Mild",s1)) %>%
  mutate(s2=(ifelse(New_BoxJul21=="Mild","Mild",s2)))

MIG_Flows_Aux._Long_v2 %>% filter(New_BoxJul21!="Mild") %>% group_by(patient, weight) %>% summarise(switches=sum(flow)) %>% ungroup() %>%
  group_by(switches) %>% summarise(total = sum(weight))




MIG_Flows_Aux._Long_v2 %>% filter(New_BoxJul21!="Mild") %>% group_by(patient, weight) %>% 
  summarise(switches=sum(flow)) %>% ungroup() %>% summarise(n=weighted.mean(switches, weight))

MIG_Flows_Aux._Long_v2 %>% filter(New_BoxJul21!="Mild") %>% group_by(patient, weight) %>% 
  summarise(switches=sum(flow)) %>% ungroup() %>% summarise(n=weighted.median(switches, weight))

MIG_Flows_Aux._Long_v2 %>% group_by(patient, weight, New_BoxJul21) %>% 
  summarise(switches=sum(flow)) %>% ungroup() %>% mutate(switches=ifelse(switches>=6,6,switches)) %>% 
  group_by(New_BoxJul21, switches) %>% summarise(total = sum(weight)) %>% 
  spread(key=New_BoxJul21, value=total)


MIG_Flows_Aux._Long_v2 %>% group_by(patient, weight, New_BoxJul21) %>% 
  summarise(switches=sum(flow)) %>% ungroup() %>% 
  group_by(New_BoxJul21) %>% summarise(mean = weighted.mean(switches, weight))


MIG_Flows_Aux._Long_v2 %>% group_by(patient, weight, New_BoxJul21) %>% 
  summarise(switches=sum(flow)) %>% ungroup() %>% 
  group_by(New_BoxJul21) %>% summarise(mean = weighted.median(switches, weight))

# -----
# Restarts (same stock, same drug, etc) --------------
MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character")

# Pats with restarts in the last year
Pats_keep <- MIG_Flows_Aux._Long %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% filter(re_starts=="1") %>% select(patient) %>% distinct()

MIG_Flows_Aux._Long <- Pats_keep %>% left_join(MIG_Flows_Aux._Long)

# keep everything until last re_start
MIG_Flows_Aux._Long_2<-MIG_Flows_Aux._Long %>% 
  group_by(patient)%>% slice(1:max(which(re_starts == "1")))

#9945082
# MIG_Flows_Aux._Long_2 %>% ungroup() %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% 
#   filter(re_starts=="1") %>% summarise(pats=sum(as.numeric(weight)))

#pick months of stops and restarts
MIG_Flows_Aux._Long_2 <- MIG_Flows_Aux._Long_2 %>% group_by(patient) %>% filter(stops=="1" | re_starts=="1")

# remove unnecessary cols
MIG_Flows_Aux._Long_2 <- MIG_Flows_Aux._Long_2  %>% select(-c(disease, flow, p1_RxExp, starts))

MIG_Flows_Aux._Long_2 <- MIG_Flows_Aux._Long_2 %>% group_by(patient) %>% 
  mutate(Stock_comp = ifelse(re_starts=="1" & s2==lag(s1), "Same", "diff"))

MIG_Flows_Aux._Long_2 <- MIG_Flows_Aux._Long_2 %>% group_by(patient) %>% 
  mutate(Drugs_comp = ifelse(re_starts=="1" & d2==lag(d1), "Same", "diff"))

MIG_Flows_Aux._Long_2$weight <- as.numeric(MIG_Flows_Aux._Long_2$weight)

MIG_Flows_Aux._Long_2 <- New_BoxJul21 %>% left_join(MIG_Flows_Aux._Long_2)

MIG_Flows_Aux._Long_2 %>% ungroup() %>% filter(New_BoxJul21!="Mild") %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% filter(re_starts=="1") %>% 
  group_by(Stock_comp, Drugs_comp) %>% summarise(pats=sum(as.numeric(weight)))


data.frame(MIG_Flows_Aux._Long_2 %>% ungroup() %>%  filter(New_BoxJul21!="Mild") %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% filter(re_starts=="1") %>% 
             group_by(s2, Stock_comp, Drugs_comp) %>% summarise(pats=sum(as.numeric(weight)))) %>%
  spread(key=s2, value=pats)
# ------
# Neurologist seen last 12 m -----------  
MIG_patients_by_unique_flow <-  read.csv("MIG_patients_by_unique_flow.csv")
MIG_patients_by_unique_flow$weight <- as.numeric(MIG_patients_by_unique_flow$weight)
MIG_patients_by_unique_flow$PatId <- as.character(MIG_patients_by_unique_flow$PatId)
names(MIG_patients_by_unique_flow)[1] <- "patient"

for (i in 3:length(MIG_patients_by_unique_flow)) { print(sum(MIG_patients_by_unique_flow$weight[MIG_patients_by_unique_flow[,i]==1]))}  


MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character")

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% filter(flow=="1") %>% filter(stops=="0")

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% summarise(N_Flows=sum(as.numeric(flow)))


MIG_patients_by_unique_flow <- MIG_patients_by_unique_flow %>% left_join(MIG_Flows_Aux._Long)




Physicians_Vanguard_Lookup <- read.csv("Physicians_Vanguard_Lookup.csv", colClasses = "character", stringsAsFactors = FALSE)
removeQuotes <- function(x) gsub("\'", "", x)
Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% mutate_if(is.character, removeQuotes)
names(Physicians_Vanguard_Lookup)[1] <- "specialty"

MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 
names(MIG_Doses_BIG)[4] <- "patient"


MIG_Doses_BIG <- MIG_patients_by_unique_flow %>% select(patient) %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()

MIG_Doses_BIG <- MIG_Doses_BIG  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
MIG_Doses_BIG <- MIG_Doses_BIG %>% distinct()

MIG_patients_by_unique_flow <- MIG_patients_by_unique_flow %>% left_join(MIG_Doses_BIG)


for (i in 3:length(MIG_patients_by_unique_flow)-2) { print(sum(MIG_patients_by_unique_flow$weight[MIG_patients_by_unique_flow[,i]==1 & MIG_patients_by_unique_flow[,19]=="NEUROLOGIST"], na.rm =T ))}  


fwrite(MIG_patients_by_unique_flow, "MIG_patients_by_unique_flow_Neurologist.txt", sep="\t") 

MIG_patients_by_unique_flow %>% filter(Inflows.to.Oral.CGRP==1) %>% summarise(n=sum(weight*N_Flows))

# -------------
# First Rimegepant, concomitant classes and month before ---------------------
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

Rimegepant_pats <- MIG_Drug_Histories %>% filter(grepl("135", Treat)) %>% select(patient, weight) %>% distinct()

MIG_Drug_Histories <- Rimegepant_pats %>% left_join(MIG_Drug_Histories)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl("135",Treat))) (which.max(grepl("135",Treat))-1):which.max(grepl("135",Treat)) else row_number())


RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
string_Preventative <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Preventative"], collapse = "|"),")\\b")
string_CGRP_Inj <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")

string_Sympt <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Acute <- paste0("\\b(",paste0(RIME_Ingredients$molecule[RIME_Ingredients$drug_group == "Acute"], collapse = "|"),")\\b")

# sum(as.numeric(MIG_Drug_Histories$weight))/2 = 50155.6


Month_Start_Drug_Pen <- MIG_Drug_Histories %>% group_by(patient, weight) %>% slice(n()) %>%
  mutate(CGRP_Inj=ifelse(str_detect(Treat,string_CGRP_Inj), "CGRP_Inj", "no")) %>%
  mutate(Preventive=ifelse(str_detect(Treat,string_Preventative), "Preventive", "no")) %>%
  mutate(Lapsed=ifelse(Treat=="-", "Lapsed", "no")) %>%
  mutate(AcuteSYmpt=ifelse(str_detect(Treat,string_Sympt)|str_detect(Treat,string_Acute), "AcuteSympt", "no")) 


Month_Start_Drug_Pen %>% ungroup() %>% filter(CGRP_Inj=="CGRP_Inj") %>% summarise(n=sum(as.numeric(weight))) # 15821
Month_Start_Drug_Pen %>% ungroup() %>% filter(Preventive=="Preventive"&CGRP_Inj!="CGRP_Inj") %>% summarise(n=sum(as.numeric(weight))) # 26647
Month_Start_Drug_Pen %>% ungroup() %>% filter(AcuteSYmpt=="AcuteSympt"&CGRP_Inj!="CGRP_Inj"&Preventive!="Preventive") %>% summarise(n=sum(as.numeric(weight))) # 2857
Month_Start_Drug_Pen %>% ungroup() %>% filter(Lapsed=="Lapsed") %>% summarise(n=sum(as.numeric(weight))) # 0


Month_Before_Drug_Pen <- MIG_Drug_Histories %>% group_by(patient, weight) %>% slice(1) %>%
  mutate(CGRP_Inj_before=ifelse(str_detect(Treat,string_CGRP_Inj), "CGRP_Inj", "no"))

# Of those who are , how many were already ON Inj?
Month_Start_Drug_Pen %>% left_join(Month_Before_Drug_Pen, by=c("patient"="patient")) %>% arrange(patient) %>%
  filter(CGRP_Inj=="CGRP_Inj") %>% ungroup() %>% group_by(CGRP_Inj_before) %>% summarise(n=sum(as.numeric(weight.x)))


# Of those who are , how many were already ON Inj?
Month_Start_Drug_Pen %>% left_join(Month_Before_Drug_Pen, by=c("patient"="patient")) %>% arrange(patient) %>%
  ungroup() %>% group_by(CGRP_Inj_before) %>% summarise(n=sum(as.numeric(weight.x)))


Prev_Drugs_Start_Rimegepant <- Month_Start_Drug_Pen %>% filter(Preventive=="Preventive"&CGRP_Inj!="CGRP_Inj") %>%
  select(patient, weight, Treat)

Prev_Drugs_Start_Rimegepant <- separate_rows(Prev_Drugs_Start_Rimegepant, Treat, sep = ",", convert=T)

RIME_Ingredients$molecule <- as.numeric(RIME_Ingredients$molecule)

Prev_Drugs_Start_Rimegepant %>% left_join(RIME_Ingredients %>% select(molecule, drug_group, drug_class), by=c("Treat"="molecule")) %>%
  filter(drug_group=="Preventative") %>% ungroup() %>% select(patient, weight, drug_class) %>%
  distinct() %>% group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))


# ------
# How many scripts of Rimegepant -----------------
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(generic_name=="Rimegepant")

#data.frame(MIG_Doses_BIG %>% mutate(dayssup=as.numeric(dayssup)) %>% group_by(dayssup) %>% count())

MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(Month = as.yearmon(from_dt))

MIG_Doses_BIG %>% group_by(Month) %>% summarise(n=sum(as.numeric(weight)))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(Total_supps = as.numeric(weight)*as.numeric(dayssup))

MIG_Doses_BIG %>% group_by(Month) %>% summarise(n=sum(as.numeric(Total_supps)))



# ----
# Neurologist seen last 12 m by type of s1 + number of flows -----------  
New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21$weight <- as.character(New_BoxJul21$weight)

MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", colClasses = "character")
MIG_Flows_Aux._Long <- New_BoxJul21 %>% left_join(MIG_Flows_Aux._Long) %>% filter(New_BoxJul21 != "Mild")
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1=as.numeric(p1)) %>% filter(p1>=48) %>% filter(flow=="1") %>% filter(stops=="0")

Flows_per_pat <- MIG_Flows_Aux._Long %>% group_by(patient) %>% summarise(N_Flows=sum(as.numeric(flow)))

MIG_Flows_Aux._Long %>% filter(re_starts == "1") %>% summarise(n=sum(as.numeric(weight))) # 5956055
MIG_Flows_Aux._Long %>% filter(s1 == "a") %>% summarise(n=sum(as.numeric(weight))) # 2026381
MIG_Flows_Aux._Long %>% filter(s1 == "A") %>% summarise(n=sum(as.numeric(weight))) # 1151155
MIG_Flows_Aux._Long %>% filter(s1 == "p") %>% summarise(n=sum(as.numeric(weight))) # 8548066
MIG_Flows_Aux._Long %>% filter(s1 == "d") %>% summarise(n=sum(as.numeric(weight))) # 13599900
MIG_Flows_Aux._Long %>% filter(s1 == "D") %>% summarise(n=sum(as.numeric(weight))) # 4703128
MIG_Flows_Aux._Long %>% filter(s1 == "O") %>% summarise(n=sum(as.numeric(weight))) # 248354.9
MIG_Flows_Aux._Long %>% filter(s1 == "I") %>% summarise(n=sum(as.numeric(weight))) # 2111488

Re_starts_pats <- MIG_Flows_Aux._Long %>% filter(re_starts == "1") %>% select(patient, weight) %>% distinct()
Sympt_pats <- MIG_Flows_Aux._Long %>% filter(s1 == "a") %>% select(patient, weight) %>% distinct()
Acute_pats <- MIG_Flows_Aux._Long %>% filter(s1 == "A") %>% select(patient, weight) %>% distinct()
Prev_pats <- MIG_Flows_Aux._Long %>% filter(s1 == "p") %>% select(patient, weight) %>% distinct()
PrevSympt_pats <- MIG_Flows_Aux._Long %>% filter(s1 == "d") %>% select(patient, weight) %>% distinct()
PrevAcute_pats <- MIG_Flows_Aux._Long %>% filter(s1 == "D") %>% select(patient, weight) %>% distinct()
CGRPOral_pats <- MIG_Flows_Aux._Long %>% filter(s1 == "O") %>% select(patient, weight) %>% distinct()
CGRPInj_pats <- MIG_Flows_Aux._Long %>% filter(s1 == "I") %>% select(patient, weight) %>% distinct()

Physicians_Vanguard_Lookup <- read.csv("Physicians_Vanguard_Lookup.csv", colClasses = "character", stringsAsFactors = FALSE)
removeQuotes <- function(x) gsub("\'", "", x)
Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% mutate_if(is.character, removeQuotes)
names(Physicians_Vanguard_Lookup)[1] <- "specialty"

MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 
names(MIG_Doses_BIG)[4] <- "patient"

Re_starts_pats_Physician <- Re_starts_pats %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()
Re_starts_pats_Physician <- Re_starts_pats_Physician  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
Re_starts_pats_Physician <- Re_starts_pats_Physician %>% distinct()
Re_starts_pats_Physician <- Re_starts_pats %>% left_join(Re_starts_pats_Physician)
Re_starts_pats_Physician %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Sympt_pats_Physician <- Sympt_pats %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()
Sympt_pats_Physician <- Sympt_pats_Physician  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
Sympt_pats_Physician <- Sympt_pats_Physician %>% distinct()
Sympt_pats_Physician <- Sympt_pats %>% left_join(Sympt_pats_Physician)
Sympt_pats_Physician %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Acute_pats_Physician <- Acute_pats %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()
Acute_pats_Physician <- Acute_pats_Physician  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
Acute_pats_Physician <- Acute_pats_Physician %>% distinct()
Acute_pats_Physician <- Acute_pats %>% left_join(Acute_pats_Physician)
Acute_pats_Physician %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

Prev_pats_Physician <- Prev_pats %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()
Prev_pats_Physician <- Prev_pats_Physician  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
Prev_pats_Physician <- Prev_pats_Physician %>% distinct()
Prev_pats_Physician <- Prev_pats %>% left_join(Prev_pats_Physician)
Prev_pats_Physician %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))


PrevSympt_pats_Physician <- PrevSympt_pats %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()
PrevSympt_pats_Physician <- PrevSympt_pats_Physician  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
PrevSympt_pats_Physician <- PrevSympt_pats_Physician %>% distinct()
PrevSympt_pats_Physician <- PrevSympt_pats %>% left_join(PrevSympt_pats_Physician)
PrevSympt_pats_Physician %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))

PrevAcute_pats_Physician <- PrevAcute_pats %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()
PrevAcute_pats_Physician <- PrevAcute_pats_Physician  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
PrevAcute_pats_Physician <- PrevAcute_pats_Physician %>% distinct()
PrevAcute_pats_Physician <- PrevAcute_pats %>% left_join(PrevAcute_pats_Physician)
PrevAcute_pats_Physician %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))


CGRPOral_pats_Physician <- CGRPOral_pats %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()
CGRPOral_pats_Physician <- CGRPOral_pats_Physician  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
CGRPOral_pats_Physician <- CGRPOral_pats_Physician %>% distinct()
CGRPOral_pats_Physician <- CGRPOral_pats %>% left_join(CGRPOral_pats_Physician)
CGRPOral_pats_Physician %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))


CGRPInj_pats_Physician <- CGRPInj_pats %>% inner_join(MIG_Doses_BIG) %>% select(patient, specialty) %>% distinct()
CGRPInj_pats_Physician <- CGRPInj_pats_Physician  %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician =="NEUROLOGIST") %>% select(patient, Physician)
CGRPInj_pats_Physician <- CGRPInj_pats_Physician %>% distinct()
CGRPInj_pats_Physician <- CGRPInj_pats %>% left_join(CGRPInj_pats_Physician)
CGRPInj_pats_Physician %>% group_by(Physician) %>% summarise(n=sum(as.numeric(weight)))


Re_starts_pats  %>% left_join(Flows_per_pat) %>% summarise(n=weighted.mean(N_Flows, as.numeric(weight))) #3.181051
Sympt_pats %>% left_join(Flows_per_pat) %>% summarise(n=weighted.mean(N_Flows, as.numeric(weight))) #5.411034
Acute_pats %>% left_join(Flows_per_pat) %>% summarise(n=weighted.mean(N_Flows, as.numeric(weight))) #4.629516
Prev_pats %>% left_join(Flows_per_pat) %>% summarise(n=weighted.mean(N_Flows, as.numeric(weight))) #4.831927
PrevSympt_pats %>% left_join(Flows_per_pat) %>% summarise(n=weighted.mean(N_Flows, as.numeric(weight))) #5.553755
PrevAcute_pats %>% left_join(Flows_per_pat) %>% summarise(n=weighted.mean(N_Flows, as.numeric(weight))) #5.496621
CGRPOral_pats %>% left_join(Flows_per_pat) %>% summarise(n=weighted.mean(N_Flows, as.numeric(weight))) #6.852552
CGRPInj_pats %>% left_join(Flows_per_pat) %>% summarise(n=weighted.mean(N_Flows, as.numeric(weight))) #6.267151

# -----
# How long do patients stay ON Riemgepant, based on s1 ? --------------

# Pick first month of Rimegepant, and stock on month before 
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))

Rimegepant_pats <- MIG_Drug_Histories %>% filter(grepl("135", Treat)) %>% select(patient, weight) %>% distinct()

MIG_Drug_Histories <- Rimegepant_pats %>% left_join(MIG_Drug_Histories)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient, weight) %>% 
  slice(if(any(grepl("135",Treat))) (which.max(grepl("135",Treat))-1):which.max(grepl("135",Treat)) else row_number())

Rimegepant_pats <- MIG_Drug_Histories %>% group_by(patient) %>% slice(1)

#Import stock 
MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- gather(MIG_Box_Histories, Month, Stock, month1:month60, factor_key=TRUE)

MIG_Box_Histories$Month <- as.character(MIG_Box_Histories$Month)
MIG_Box_Histories$Month <- parse_number(MIG_Box_Histories$Month)

MIG_Box_Histories <- MIG_Box_Histories %>% mutate(Stock = str_sub(Stock, 2L, 2L))

Rimegepant_pats <- Rimegepant_pats %>% left_join(MIG_Box_Histories) %>% select(patient, weight, Stock)


# Duration ON Rimegepant
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- Rimegepant_pats %>% select(patient) %>% left_join(MIG_Drug_Histories)

# select only columns with the months / drugs
MIG_Drug_Histories <-  MIG_Drug_Histories %>% ungroup() %>% select(4:63)

# convert no Rimegepant too zero, and Rimegepant to one   # convert to numeric everything
MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "Rimegepant"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Rimegepant",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- Rimegepant_pats %>% select(patient) %>% left_join(MIG_Drug_Histories_LONG) %>% bind_cols(MIG_Drug_Histories)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line # of course, only 2 lines possible, treatment or no treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these  periods
Rimegepant_Periods_MIG <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Rimegepant_Periods_MIG)[3] <- "Duration"

Rimegepant_Periods_MIG <- Rimegepant_Periods_MIG %>% left_join(MIG_Drug_Histories %>%  select(patient, weight), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, Total_duration) %>% distinct()

Rimegepant_pats$weight <- as.numeric(Rimegepant_pats$weight)

Rimegepant_pats <- Rimegepant_pats %>% left_join(Rimegepant_Periods_MIG) 

Rimegepant_pats %>% group_by(Stock) %>% summarise(n=weighted.mean(Total_duration, weight))



Rimegepant_pats %>% group_by(Stock) %>% summarise(n=weighted.median(Total_duration, weight))

# ------

# Class Pentrance Month 60 only MODERATE-to-SEVERE -------------------

New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21$weight <- as.character(New_BoxJul21$weight)
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21!="Mild")

sum(as.numeric(New_BoxJul21$weight)) # 9638606

RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, month60)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, month60, sep = ",", convert=T )
names(MIG_Drug_Histories)[3] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(molecule != "-") %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 9638606)*100)) %>% arrange(desc(sum_weights_percent))


# -----
# Classs Penetrance across entire 60 month period --------------------------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")

names(MIG_Drug_Histories)[6] <- "molecule"
MIG_Drug_Histories$molecule <- as.character(MIG_Drug_Histories$molecule)

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 9638606)*100)) %>% arrange(-sum_weights_percent)


# --------

# Classify patients as Acute / Episodeic / Chronic based on pills per month of max class ----------------
MIG_Doses <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Doses <- MIG_Doses %>% filter(status != "G")
MIG_Doses <- MIG_Doses %>% select(-c(prov_unique, prov_type, specialty, taxonomy1, taxonomy2))
MIG_Doses <- MIG_Doses %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses <- MIG_Doses %>% select(pat_id, generic_name, drug_group, drug_class, weight, from_dt, dayssup)

MIG_Doses_m12 <- MIG_Doses %>% filter(from_dt >= "2020-09-30") %>% filter(dayssup != "")
MIG_Doses_m12 <- MIG_Doses_m12 %>% group_by(pat_id) %>% mutate(total_n_pills = sum(as.numeric(dayssup)))
MIG_Doses_m12 <- MIG_Doses_m12 %>% group_by(pat_id, drug_class) %>%  mutate(total_n_pills_class = sum(as.numeric(dayssup)))
MIG_Doses_m12 <- MIG_Doses_m12 %>% ungroup() %>% group_by(pat_id) %>% mutate(pills_per_month = total_n_pills/12)
MIG_Doses_m12 <- MIG_Doses_m12 %>% ungroup() %>% group_by(pat_id, drug_class) %>% mutate(pills_per_month_class = total_n_pills_class/12)

MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month) %>% 
  distinct() %>% mutate(frequency = ifelse(pills_per_month>=15, "Chronic",
                                           ifelse(pills_per_month>=4, "Intermediate", "Acute"))) %>%
  group_by(frequency) %>% summarise(pats = sum(as.numeric(weight)))


MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month) %>% 
  distinct() %>% summarise(mean = weighted.mean(pills_per_month, as.numeric(weight))) 

MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month) %>% 
  distinct() %>%
  select(pills_per_month)%>%
  ggplot(aes(pills_per_month))+
  geom_density(size=2, fill="deepskyblue4", colour="deepskyblue4", alpha=0.7)+
  xlim(0,100)+
  theme(panel.background = element_blank())+
  ylab("Patient Sample Proportion\n")+
  xlab("\nTotal Number of Supply Days per Month")

MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month_class) %>% 
  distinct() %>% group_by(pat_id) %>% filter(pills_per_month_class == max(pills_per_month_class)) %>% 
  ungroup() %>% mutate(frequency = ifelse(pills_per_month_class>=15, "Chronic",
                                          ifelse(pills_per_month_class>=4, "Episodic", "Acute"))) %>%
  group_by(frequency) %>% summarise(pats = sum(as.numeric(weight)))


MIG_Doses_dayssup_m12_perMonth_perclass <- MIG_Doses_m12 %>% ungroup() %>% 
  select(pat_id, weight, drug_class, pills_per_month, pills_per_month_class) %>% distinct() %>%
  group_by(pat_id) %>% mutate(min= max(pills_per_month_class))

temp <- MIG_Doses_m12 %>% ungroup() %>% select(pat_id, weight, pills_per_month_class) %>% 
  distinct() %>% group_by(pat_id) %>% filter(pills_per_month_class == max(pills_per_month_class)) %>% 
  ungroup() %>% mutate(frequency = ifelse(pills_per_month_class>=15, "Chronic",
                                          ifelse(pills_per_month_class>=4, "Episodic", "Acute")))

fwrite(temp, "Number_pills_month_frequency.txt" , sep="\t")



# # # #
Number_pills_month_frequency <- fread("Number_pills_month_frequency.txt")
names(Number_pills_month_frequency)[1] <- "patient"
Number_pills_month_frequency <- Number_pills_month_frequency %>% select(patient, frequency)
Number_pills_month_frequency$patient <- as.character(Number_pills_month_frequency$patient)


MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21 != "Mild")


MIG_Flows_Aux._Long <- New_BoxJul21 %>% select(patient) %>% left_join(MIG_Flows_Aux._Long)

m60_stocks <- MIG_Flows_Aux._Long %>% filter(p2=="60") %>% select(patient, weight, s2)

m60_stocks <- m60_stocks %>% left_join(Number_pills_month_frequency)

m60_stocks %>% group_by(s2, frequency) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=s2, value=n)



# ------
# Classify type of flow for Mod to Sever base don Chronic Migraine status -----------
New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21 != "Mild")

#Flows table
MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- New_BoxJul21 %>% select(patient) %>% left_join(MIG_Flows_Aux._Long)

data2 <- MIG_Flows_Aux._Long[flow == 1 & stops==0,.(patient,weight,p1,p2,d1,d2,s1,s2,flow)]
data2 <- data2 %>% mutate(p1=as.numeric(p1)) %>% mutate(p2=as.numeric(p2)) %>% filter(p1>=48)


RIME_Ingredients <- fread("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients$drug_id <- unlist(lapply(RIME_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))

string_Acute <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_group == "Acute" | RIME_Ingredients$drug_group == "Symptomatic"], collapse = "|"),")\\b")
string_Prev <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_group == "Preventative"], collapse = "|"),")\\b")
string_CGRP <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_group == "CGRP Oral" | RIME_Ingredients$drug_group == "CGRP Injectable"], collapse = "|"),")\\b")


# Acute Therapy class - flags
data2 <- data2[, Acute_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Acute), str_c(unlist(str_extract_all(x, string_Acute)), collapse = ","),"")))]
data2 <- data2[, Acute_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Acute), str_c(unlist(str_extract_all(x, string_Acute)), collapse = ","),"")))]
data2 <- data2[, nr_Acute_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Acute)*1), str_split(x,","))))]
data2 <- data2[, nr_Acute_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Acute)*1), str_split(x,","))))]
data2 <- data2[, nr_AcuteUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Acute_d1","Acute_d2")] 
data2 <- data2[, Acute_flow_type := ifelse(nr_Acute_d2 < nr_Acute_d1 & nr_AcuteUnq_d1d2 > nr_Acute_d1, "D+S", 
                                           ifelse(nr_Acute_d2 > nr_Acute_d1 & nr_AcuteUnq_d1d2 > nr_Acute_d2, "A+S",
                                                  ifelse(nr_Acute_d2 < nr_Acute_d1, "D", 
                                                         ifelse(nr_Acute_d2 > nr_Acute_d1, "A", 
                                                                ifelse(nr_Acute_d2 == nr_Acute_d1 & Acute_d2 != Acute_d1, "S","-")))))] 


# Prev Therapy class - flags
data2 <- data2[, Prev_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Prev), str_c(unlist(str_extract_all(x, string_Prev)), collapse = ","),"")))]
data2 <- data2[, Prev_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Prev), str_c(unlist(str_extract_all(x, string_Prev)), collapse = ","),"")))]
data2 <- data2[, nr_Prev_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Prev)*1), str_split(x,","))))]
data2 <- data2[, nr_Prev_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Prev)*1), str_split(x,","))))]
data2 <- data2[, nr_PrevUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Prev_d1","Prev_d2")] 
data2 <- data2[, Prev_flow_type := ifelse(nr_Prev_d2 < nr_Prev_d1 & nr_PrevUnq_d1d2 > nr_Prev_d1, "D+S", 
                                          ifelse(nr_Prev_d2 > nr_Prev_d1 & nr_PrevUnq_d1d2 > nr_Prev_d2, "A+S",
                                                 ifelse(nr_Prev_d2 < nr_Prev_d1, "D", 
                                                        ifelse(nr_Prev_d2 > nr_Prev_d1, "A", 
                                                               ifelse(nr_Prev_d2 == nr_Prev_d1 & Prev_d2 != Prev_d1, "S","-")))))] 


# CGRP Therapy class - flags
data2 <- data2[, CGRP_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_CGRP), str_c(unlist(str_extract_all(x, string_CGRP)), collapse = ","),"")))]
data2 <- data2[, CGRP_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_CGRP), str_c(unlist(str_extract_all(x, string_CGRP)), collapse = ","),"")))]
data2 <- data2[, nr_CGRP_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_CGRP)*1), str_split(x,","))))]
data2 <- data2[, nr_CGRP_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_CGRP)*1), str_split(x,","))))]
data2 <- data2[, nr_CGRPUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("CGRP_d1","CGRP_d2")] 
data2 <- data2[, CGRP_flow_type := ifelse(nr_CGRP_d2 < nr_CGRP_d1 & nr_CGRPUnq_d1d2 > nr_CGRP_d1, "D+S", 
                                          ifelse(nr_CGRP_d2 > nr_CGRP_d1 & nr_CGRPUnq_d1d2 > nr_CGRP_d2, "A+S",
                                                 ifelse(nr_CGRP_d2 < nr_CGRP_d1, "D", 
                                                        ifelse(nr_CGRP_d2 > nr_CGRP_d1, "A", 
                                                               ifelse(nr_CGRP_d2 == nr_CGRP_d1 & CGRP_d2 != CGRP_d1, "S","-")))))] 


fwrite(data2, "Flows_long_HighGranularity_DrugGroups_MOD_SEV_All_Flows.csv")
data2 <- fread("Flows_long_HighGranularity_DrugGroups_MOD_SEV_All_Flows.csv")
data2$patient <- as.character(data2$patient)

sum(as.numeric(data2$weight)) # 38515708

New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21 != "Mild")
New_BoxJul21 %>% group_by(New_BoxJul21) %>% summarise(n=sum(weight))

New_BoxJul21 %>% select(patient, New_BoxJul21) %>%  left_join(data2) %>% filter(flow==1) %>%
  summarise(n=sum(as.numeric(weight))) # 38515708

data3 <- New_BoxJul21 %>% select(patient, New_BoxJul21) %>%  left_join(data2) 

# Total flows mod to sev last 12 m
data3 %>% filter(flow==1) %>% summarise(n=sum(as.numeric(weight))) # 38515708

# Total flows mod to sev last 12 m per stock on m60
data3 %>% group_by(s1) %>% filter(flow==1) %>% summarise(n=sum(as.numeric(weight)))
 


# Total flows mod to sev OF ACUTE DRUGS last 12 m per stock on m60
data3 %>% group_by(New_BoxJul21) %>% filter(flow==1) %>% filter(Acute_flow_type!="-" & Prev_flow_type=="-" & CGRP_flow_type=="-") %>% summarise(n=sum(as.numeric(weight)))



# Any acute counts, regadless of the rest
# data3 %>% group_by(New_BoxJul21) %>% filter(flow==1) %>% filter(Acute_flow_type!="-") %>% summarise(n=sum(as.numeric(weight)))



# Any Add and or switch of acute vs everything else
# data3 %>% group_by(s1) %>% filter(flow==1) %>% filter(Acute_flow_type=="A" | Acute_flow_type=="S" | Acute_flow_type=="A+S" | Acute_flow_type=="D+S" | Acute_flow_type=="D") %>% summarise(n=sum(as.numeric(weight)))




# Total flows mod to sev OF Prev or CGRP DRUGS last 12 m per stock on m60
data3 %>% group_by(New_BoxJul21) %>% filter(flow==1) %>% filter(Prev_flow_type!="-" | CGRP_flow_type !="-") %>% summarise(n=sum(as.numeric(weight)))



# Prev or CGRP flows without ACUTE
# data3 %>% group_by(New_BoxJul21) %>% filter(flow==1) %>% filter(Acute_flow_type=="-" & (Prev_flow_type!="-" | CGRP_flow_type !="-")) %>% summarise(n=sum(as.numeric(weight)))
# 



# data3 %>% filter(flow==1) %>% filter((Acute_flow_type=="A" | Acute_flow_type=="S" | Acute_flow_type=="A+S" | Acute_flow_type=="D+S") &
#                                        ((Prev_flow_type=="-") | (Prev_flow_type=="D")) & ((CGRP_flow_type=="-") | (CGRP_flow_type=="D"))) %>%
#   summarise(n=sum(as.numeric(weight)))
# 
# 
# data3 %>% filter(flow==1) %>% 
#   filter(((Prev_flow_type=="A" | Prev_flow_type=="S" | Prev_flow_type=="A+S" | Prev_flow_type=="D+S") | 
#            (CGRP_flow_type=="A" | CGRP_flow_type=="S" | CGRP_flow_type=="A+S" | CGRP_flow_type=="D+S")) & 
#            (Acute_flow_type=="A" | Acute_flow_type=="S" | Acute_flow_type=="A+S" | Acute_flow_type=="D+S")) %>% 
#   summarise(n=sum(as.numeric(weight)))
# 
# 
# data3 %>% filter(flow==1) %>% filter((Acute_flow_type=="D") & ((Prev_flow_type=="D") | (CGRP_flow_type=="D"))) %>%
#   summarise(n=sum(as.numeric(weight)))
#   
# 
# data3 %>% filter(flow==1) %>% filter((Acute_flow_type=="D") & ((Prev_flow_type=="-") | (CGRP_flow_type=="-"))) %>%
#   summarise(n=sum(as.numeric(weight)))
# 
# 
# 
# data3 %>% filter(flow==1) %>% filter((Acute_flow_type=="D") & ((Prev_flow_type=="D") | (CGRP_flow_type=="D"))) %>%
#   summarise(n=sum(as.numeric(weight)))
# 
# 
# data3 %>% filter(flow==1) %>% filter( 
#   ((Acute_flow_type=="-")|(Acute_flow_type=="D")) & 
#     ((CGRP_flow_type=="A" | CGRP_flow_type=="S" | CGRP_flow_type=="A+S" | CGRP_flow_type=="D+S") | 
#     (Prev_flow_type=="A" | Prev_flow_type=="S" | Prev_flow_type=="A+S" | Prev_flow_type=="D+S"))) %>%
#      summarise(n=sum(as.numeric(weight)))
# 
# 
# data3 %>% filter(flow==1) %>% filter(Acute_flow_type=="A" | Acute_flow_type=="S" | Acute_flow_type=="A+S" | Acute_flow_type=="D+S")
# 
# 




# Flow Acute (exc drops)

data3$Index = seq.int(nrow(data3)) 

data3 %>% group_by(s1) %>% filter(flow==1) %>% 
  filter(Acute_flow_type=="A" | Acute_flow_type=="S" | Acute_flow_type=="A+S" | Acute_flow_type=="D+S") %>% 
  summarise(n=sum(as.numeric(weight)))

to_remove <- data3 %>% group_by(s1) %>% filter(flow==1) %>% 
  filter(Acute_flow_type=="A" | Acute_flow_type=="S" | Acute_flow_type=="A+S" | Acute_flow_type=="D+S") %>%
  select(Index)

data3 <- data3 %>% anti_join(to_remove)


# Prev/CGRP Flows (exc drops)
data3 %>% group_by(s1) %>% filter(flow==1) %>% 
  filter(Acute_flow_type=="-" & 
           ((Prev_flow_type=="A" | Prev_flow_type=="S" | Prev_flow_type=="A+S" | Prev_flow_type=="D+S") | 
              (CGRP_flow_type=="A" | CGRP_flow_type=="S" | CGRP_flow_type=="A+S" | CGRP_flow_type=="D+S"))) %>% 
  summarise(n=sum(as.numeric(weight)))

to_remove_2 <- data3 %>% group_by(s1) %>% filter(flow==1) %>% 
  filter(Acute_flow_type=="-" & 
           ((Prev_flow_type=="A" | Prev_flow_type=="S" | Prev_flow_type=="A+S" | Prev_flow_type=="D+S") | 
              (CGRP_flow_type=="A" | CGRP_flow_type=="S" | CGRP_flow_type=="A+S" | CGRP_flow_type=="D+S"))) %>%
  select(Index)


                                            
data3 <- data3 %>% anti_join(to_remove_2)

# Drops
data3 %>% group_by(s1) %>% filter(flow==1) %>% 
  filter(Acute_flow_type=="D" | Prev_flow_type=="D" | CGRP_flow_type=="D") %>% 
  summarise(n=sum(as.numeric(weight)))

# Split into Chronic vs not chronic based on Dx hisotry  -------------
RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
RIME_Demographics <- RIME_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight, diagnosis)
RIME_Demographics <- RIME_Demographics %>% filter(grepl("Chronic", diagnosis))
names(RIME_Demographics)[1] <- "patient"
RIME_Demographics$diagnosis <- "Chronic"

data3 %>% left_join(RIME_Demographics) %>% group_by(New_BoxJul21, diagnosis) %>% filter(flow==1) %>% filter(Prev_flow_type!="-" | CGRP_flow_type !="-") %>% summarise(n=sum(as.numeric(weight)))



# Prev or CGRP flows without ACUTE
# data3 %>% left_join(RIME_Demographics) %>% group_by(New_BoxJul21, diagnosis) %>% filter(flow==1) %>% filter(Acute_flow_type=="-" & (Prev_flow_type!="-" | CGRP_flow_type !="-")) %>% summarise(n=sum(as.numeric(weight)))

                                            
# -------
# Check for each stock (inc. Mild) the % of chronic -----

New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)

RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
RIME_Demographics <- RIME_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight, diagnosis)
RIME_Demographics <- RIME_Demographics %>% filter(grepl("Chronic", diagnosis))
names(RIME_Demographics)[1] <- "patient"
RIME_Demographics$diagnosis <- "Chronic"
RIME_Demographics$weight <- as.numeric(RIME_Demographics$weight)

New_BoxJul21 %>% left_join(RIME_Demographics) %>%
  group_by(New_BoxJul21, diagnosis) %>% summarise(n=sum(weight))

New_BoxJul21 %>% left_join(RIME_Demographics) %>%
  group_by(diagnosis) %>% summarise(n=sum(weight))



# ----
# Classify each flow on chronic/acute/episodic
RIME_Demographics <- read.table("RIME Demographics.txt", header = T, sep="\t",colClasses = "character", stringsAsFactors = FALSE)
RIME_Demographics <- RIME_Demographics %>% filter(diagnosis != "-") %>% select(patid, weight, diagnosis)
RIME_Demographics <- RIME_Demographics %>% filter(grepl("Chronic", diagnosis))
names(RIME_Demographics)[1] <- "patient"
RIME_Demographics$diagnosis <- "Chronic"

data3 <- data3 %>% left_join(RIME_Demographics)

# Acute flows
data.frame(data3 %>% filter(flow==1) %>% filter(Acute_flow_type!="-" & Prev_flow_type=="-" & CGRP_flow_type=="-") %>% group_by(s1,s2) %>% summarise(n=sum(as.numeric(weight))) %>%
             spread(key=s2, value=n))

# Chronic flows
data.frame(data3 %>% filter(flow==1) %>% filter(Prev_flow_type!="-" | CGRP_flow_type !="-") %>% filter(diagnosis=="Chronic") %>% group_by(s1,s2) %>% summarise(n=sum(as.numeric(weight))) %>%
             spread(key=s2, value=n))



# Episodic flows
data.frame(data3 %>% filter(flow==1) %>% filter(Prev_flow_type!="-" | CGRP_flow_type !="-") %>% filter(is.na(diagnosis)) %>% group_by(s1,s2) %>% summarise(n=sum(as.numeric(weight))) %>%
             spread(key=s2, value=n))


# Ternary Plots -------------

New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21 != "Mild")
New_BoxJul21$weight <- as.character(New_BoxJul21$weight)

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease)) %>% select(1,2,51:62)

MIG_Drug_Histories <- New_BoxJul21 %>% select(patient, weight) %>% left_join(MIG_Drug_Histories)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))

MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient, weight) %>% distinct()

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient) %>% mutate(Total_Mols = n())



# dictionary with drugs
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))
RIME_Ingredients$molecule <- as.numeric(RIME_Ingredients$molecule)

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>% select(molecule, drug_group), by=c("Treat"="molecule"))

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient, drug_group) %>% arrange(patient, drug_group) %>% mutate(Mols_category = n())

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(weight, Treat))

MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

MIG_Drug_Histories <- MIG_Drug_Histories %>% ungroup() %>% group_by(patient) %>% spread(key=drug_group, value=Mols_category)

MIG_Drug_Histories[3:7][is.na(MIG_Drug_Histories[3:7])] <- 0

MIG_Drug_Histories[8] <- MIG_Drug_Histories[3]/MIG_Drug_Histories[2]
MIG_Drug_Histories[9] <- MIG_Drug_Histories[4]/MIG_Drug_Histories[2]
MIG_Drug_Histories[10] <- MIG_Drug_Histories[5]/MIG_Drug_Histories[2]
MIG_Drug_Histories[11] <- MIG_Drug_Histories[6]/MIG_Drug_Histories[2]
MIG_Drug_Histories[12] <- MIG_Drug_Histories[7]/MIG_Drug_Histories[2]

names(MIG_Drug_Histories)[8] <- "Acute_prop"
names(MIG_Drug_Histories)[9] <- "CGRPInj_prop"
names(MIG_Drug_Histories)[10] <-  "CGRPOral_prop"
names(MIG_Drug_Histories)[11] <- "Preventive_prop"
names(MIG_Drug_Histories)[12] <- "Sympt_prop"


MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate(CGRP_Use = ifelse(CGRPInj_prop>0|CGRPOral_prop>0,"Yes", "No"))

temp <- MIG_Drug_Histories %>% select(1,8,11,12,13)


fwrite(temp, "Prop_Mols_12m_TernaryPlot_uniqueDrugs.txt", sep="\t")




library(plotly)

temp <- fread("Prop_Mols_12m_TernaryPlot.txt", sep="\t")


axis <- function(title) {
  list(
    title = title,
    titlefont = list(
      size = 20
    ),
    tickfont = list(
      size = 15
    ),
    tickcolor = 'rgba(0,0,0,0)',
    ticklen = 5
  )
}


fig <- temp %>% plot_ly()

fig <- fig %>% add_trace(
  type = 'scatterternary',
  mode = 'markers',
  a = ~Acute_prop,
  b = ~Preventive_prop,
  c = ~Sympt_prop,
  text = ~V1,
  marker = list( 
    symbol = 100,
    color = '#17A6A8',
    size = 2,
    line = list('width' = 0.2)
  )
)
fig <- fig %>% layout(
  title = "Simple Ternary Plot with Markers",
  ternary = list(
    sum = 100,
    aaxis = axis('Acute (%)'),
    baxis = axis('Preventive (%)'),
    caxis = axis('Symptomatic (%)')
  )
)

fig



# ---------
# MOD-SEV Classs Penetrance vs Duration across last 12 month period ---------------------
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21$weight <- as.character(New_BoxJul21$weight)
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21 != "Mild")

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))

length(unique(MIG_Drug_Histories$patient)) # 106269
sum(as.numeric(MIG_Drug_Histories$weight)) # 9638606

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(1,2,51:62)
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 9638606)*100)) %>% arrange(-sum_weights_percent)




# Durations 12 months
#CGRP Injectable
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('137',.), ~replace(., grepl('137', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('138',.), ~replace(., grepl('138', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('139',.), ~replace(., grepl('139', .), "CGRP Injectable"))%>% 
  mutate_if(grepl('140',.), ~replace(., grepl('140', .), "CGRP Injectable"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Injectable",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

CGRP_Injectable_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(CGRP_Injectable_Periods)[3] <- "Duration"
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% left_join(MIG_Drug_Histories) 
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% mutate(weight = as.numeric(weight))
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
CGRP_Injectable_Periods <- CGRP_Injectable_Periods %>% distinct()

library(spatstat)
weighted.mean(CGRP_Injectable_Periods$Total_Duration, CGRP_Injectable_Periods$weight)  #7.778877
weighted.median(CGRP_Injectable_Periods$Total_Duration, CGRP_Injectable_Periods$weight)  #8.5




#CGRP Oral
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)


MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('135',.), ~replace(., grepl('135', .), "CGRP Oral"))%>% 
  mutate_if(grepl('136',.), ~replace(., grepl('136', .), "CGRP Oral"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="CGRP Oral",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

CGRP_Oral_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(CGRP_Oral_Periods)[3] <- "Duration"
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% left_join(MIG_Drug_Histories) 
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% mutate(weight = as.numeric(weight))
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
CGRP_Oral_Periods <- CGRP_Oral_Periods %>% distinct()

library(spatstat)
weighted.mean(CGRP_Oral_Periods$Total_Duration, CGRP_Oral_Periods$weight)  #3.592865
weighted.median(CGRP_Oral_Periods$Total_Duration, CGRP_Oral_Periods$weight)  #1.5




#Ditan
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)


MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('77',.), ~replace(., grepl('77', .), "Ditan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ditan",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Ditan_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Ditan_Periods)[3] <- "Duration"
Ditan_Periods <- Ditan_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Ditan_Periods <- Ditan_Periods %>% left_join(MIG_Drug_Histories) 
Ditan_Periods <- Ditan_Periods %>% mutate(weight = as.numeric(weight))
Ditan_Periods <- Ditan_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Ditan_Periods <- Ditan_Periods %>% distinct()

library(spatstat)
weighted.mean(Ditan_Periods$Total_Duration, Ditan_Periods$weight)  #2.946151
weighted.median(Ditan_Periods$Total_Duration, Ditan_Periods$weight)  #1.5



#Triptans
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('70',.), ~replace(., grepl('70', .), "Triptan")) %>%
  mutate_if(grepl('71',.), ~replace(., grepl('71', .), "Triptan")) %>%
  mutate_if(grepl('72',.), ~replace(., grepl('72', .), "Triptan")) %>%
  mutate_if(grepl('73',.), ~replace(., grepl('73', .), "Triptan")) %>%
  mutate_if(grepl('74',.), ~replace(., grepl('74', .), "Triptan")) %>%
  mutate_if(grepl('75',.), ~replace(., grepl('75', .), "Triptan")) %>%
  mutate_if(grepl('76',.), ~replace(., grepl('76', .), "Triptan"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Triptan",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Triptan_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Triptan_Periods)[3] <- "Duration"
Triptan_Periods <- Triptan_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Triptan_Periods <- Triptan_Periods %>% left_join(MIG_Drug_Histories) 
Triptan_Periods <- Triptan_Periods %>% mutate(weight = as.numeric(weight))
Triptan_Periods <- Triptan_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Triptan_Periods <- Triptan_Periods %>% distinct()

library(spatstat)
weighted.mean(Triptan_Periods$Total_Duration, Triptan_Periods$weight) #4.45563
weighted.median(Triptan_Periods$Total_Duration, Triptan_Periods$weight)   #2.5



#Ergot
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('68',.), ~replace(., grepl('68', .), "Ergot")) %>%
  mutate_if(grepl('69',.), ~replace(., grepl('69', .), "Ergot"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ergot",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)


MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Ergot_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Ergot_Periods)[3] <- "Duration"
Ergot_Periods <- Ergot_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Ergot_Periods <- Ergot_Periods %>% left_join(MIG_Drug_Histories) 
Ergot_Periods <- Ergot_Periods %>% mutate(weight = as.numeric(weight))
Ergot_Periods <- Ergot_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Ergot_Periods <- Ergot_Periods %>% distinct()

library(spatstat)
weighted.mean(Ergot_Periods$Total_Duration, Ergot_Periods$weight) #2.603112
weighted.median(Ergot_Periods$Total_Duration, Ergot_Periods$weight) #1




#Weak Opioid
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "Weak Opioid")) %>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "Weak Opioid")) %>%
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "Weak Opioid")) %>%
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "Weak Opioid"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Weak Opioid",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Weak_Opioid_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Weak_Opioid_Periods)[3] <- "Duration"
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% left_join(MIG_Drug_Histories) 
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% mutate(weight = as.numeric(weight))
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Weak_Opioid_Periods <- Weak_Opioid_Periods %>% distinct()

library(spatstat)
weighted.mean(Weak_Opioid_Periods$Total_Duration, Weak_Opioid_Periods$weight) #5.355803
weighted.median(Weak_Opioid_Periods$Total_Duration, Weak_Opioid_Periods$weight) #1.5



#Strong Opioid
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "Strong Opioid")) %>%
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "Strong Opioid")) %>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "Strong Opioid")) %>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "Strong Opioid")) %>%
  mutate_if(grepl('38',.), ~replace(., grepl('38', .), "Strong Opioid")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('39', .), "Strong Opioid")) %>%
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "Strong Opioid")) %>%
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "Strong Opioid")) %>%
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "Strong Opioid")) %>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "Strong Opioid")) %>%
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Strong Opioid")) %>%
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Strong Opioid")) %>%
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Strong Opioid")) %>%
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Strong Opioid")) %>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Strong Opioid")) %>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Strong Opioid"))


MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Strong Opioid",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Strong_Opioid_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Strong_Opioid_Periods)[3] <- "Duration"
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% left_join(MIG_Drug_Histories) 
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% mutate(weight = as.numeric(weight))
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Strong_Opioid_Periods <- Strong_Opioid_Periods %>% distinct()

library(spatstat)
weighted.mean(Strong_Opioid_Periods$Total_Duration, Strong_Opioid_Periods$weight)  #4.80293
weighted.median(Strong_Opioid_Periods$Total_Duration, Strong_Opioid_Periods$weight)  #1.5




#NSAID
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(3{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(3{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(4{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(4{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(5{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(5{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(6{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(6{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(7{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(7{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(17{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "NSAID")) %>%
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "NSAID"))


MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="NSAID",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

NSAID_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(NSAID_Periods)[3] <- "Duration"
NSAID_Periods <- NSAID_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
NSAID_Periods <- NSAID_Periods %>% left_join(MIG_Drug_Histories) 
NSAID_Periods <- NSAID_Periods %>% mutate(weight = as.numeric(weight))
NSAID_Periods <- NSAID_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
NSAID_Periods <- NSAID_Periods %>% distinct()

library(spatstat)
weighted.mean(NSAID_Periods$Total_Duration, NSAID_Periods$weight) #3.480901
weighted.median(NSAID_Periods$Total_Duration, NSAID_Periods$weight) #2.5  




#Steroid
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('58',.), ~replace(., grepl('58', .), "Steroid")) %>%
  mutate_if(grepl('59',.), ~replace(., grepl('59', .), "Steroid")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Steroid",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Steroid_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Steroid_Periods)[3] <- "Duration"
Steroid_Periods <- Steroid_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Steroid_Periods <- Steroid_Periods %>% left_join(MIG_Drug_Histories) 
Steroid_Periods <- Steroid_Periods %>% mutate(weight = as.numeric(weight))
Steroid_Periods <- Steroid_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Steroid_Periods <- Steroid_Periods %>% distinct()

library(spatstat)
weighted.mean(Steroid_Periods$Total_Duration, Steroid_Periods$weight)  #2.082192
weighted.median(Steroid_Periods$Total_Duration, Steroid_Periods$weight)  #1




#Neural
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('132',.), ~replace(., grepl('132', .), "Neural")) %>%
  mutate_if(grepl('133',.), ~replace(., grepl('133', .), "Neural"))  %>%
  mutate_if(grepl('134',.), ~replace(., grepl('134', .), "Neural")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Neural",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Neural_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Neural_Periods)[3] <- "Duration"
Neural_Periods <- Neural_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Neural_Periods <- Neural_Periods %>% left_join(MIG_Drug_Histories) 
Neural_Periods <- Neural_Periods %>% mutate(weight = as.numeric(weight))
Neural_Periods <- Neural_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Neural_Periods <- Neural_Periods %>% distinct()

library(spatstat)
weighted.mean(Neural_Periods$Total_Duration, Neural_Periods$weight)  #7.266879
weighted.median(Neural_Periods$Total_Duration, Neural_Periods$weight)  #7.5






#Hospitalization
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('67',.), ~replace(., grepl('67', .), "Hospitalization"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Hospitalization",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Hospitalization_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Hospitalization_Periods)[3] <- "Duration"
Hospitalization_Periods <- Hospitalization_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Hospitalization_Periods <- Hospitalization_Periods %>% left_join(MIG_Drug_Histories) 
Hospitalization_Periods <- Hospitalization_Periods %>% mutate(weight = as.numeric(weight))
Hospitalization_Periods <- Hospitalization_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Hospitalization_Periods <- Hospitalization_Periods %>% distinct()

library(spatstat)
weighted.mean(Hospitalization_Periods$Total_Duration, Hospitalization_Periods$weight)  #1.112253
weighted.median(Hospitalization_Periods$Total_Duration, Hospitalization_Periods$weight)  #1







#Antiemetic
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Antiemetic")) %>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Antiemetic")) %>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Antiemetic")) %>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Antiemetic")) %>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Antiemetic")) %>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Antiemetic")) %>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Antiemetic")) %>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Antiemetic")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antiemetic",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Antiemetic_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Antiemetic_Periods)[3] <- "Duration"
Antiemetic_Periods <- Antiemetic_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Antiemetic_Periods <- Antiemetic_Periods %>% left_join(MIG_Drug_Histories) 
Antiemetic_Periods <- Antiemetic_Periods %>% mutate(weight = as.numeric(weight))
Antiemetic_Periods <- Antiemetic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Antiemetic_Periods <- Antiemetic_Periods %>% distinct()

library(spatstat)
weighted.mean(Antiemetic_Periods$Total_Duration, Antiemetic_Periods$weight)  #2.91612
weighted.median(Antiemetic_Periods$Total_Duration, Antiemetic_Periods$weight)  #1





#SSRI
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('122',.), ~replace(., grepl('122', .), "SSRI")) %>%
  mutate_if(grepl('123',.), ~replace(., grepl('123', .), "SSRI")) %>%
  mutate_if(grepl('124',.), ~replace(., grepl('124', .), "SSRI")) %>%
  mutate_if(grepl('125',.), ~replace(., grepl('125', .), "SSRI")) %>%
  mutate_if(grepl('126',.), ~replace(., grepl('126', .), "SSRI"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SSRI",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

SSRI_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(SSRI_Periods)[3] <- "Duration"
SSRI_Periods <- SSRI_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
SSRI_Periods <- SSRI_Periods %>% left_join(MIG_Drug_Histories) 
SSRI_Periods <- SSRI_Periods %>% mutate(weight = as.numeric(weight))
SSRI_Periods <- SSRI_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
SSRI_Periods <- SSRI_Periods %>% distinct()

library(spatstat)
weighted.mean(SSRI_Periods$Total_Duration, SSRI_Periods$weight)  #8.377645
weighted.median(SSRI_Periods$Total_Duration, SSRI_Periods$weight)  #9.5






#SNRI
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('127',.), ~replace(., grepl('127', .), "SNRI")) %>%
  mutate_if(grepl('128',.), ~replace(., grepl('128', .), "SNRI")) %>%
  mutate_if(grepl('129',.), ~replace(., grepl('129', .), "SNRI")) %>%
  mutate_if(grepl('130',.), ~replace(., grepl('130', .), "SNRI")) %>%
  mutate_if(grepl('131',.), ~replace(., grepl('131', .), "SNRI"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SNRI",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

SNRI_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(SNRI_Periods)[3] <- "Duration"
SNRI_Periods <- SNRI_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
SNRI_Periods <- SNRI_Periods %>% left_join(MIG_Drug_Histories) 
SNRI_Periods <- SNRI_Periods %>% mutate(weight = as.numeric(weight))
SNRI_Periods <- SNRI_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
SNRI_Periods <- SNRI_Periods %>% distinct()

library(spatstat)
weighted.mean(SNRI_Periods$Total_Duration, SNRI_Periods$weight)  #8.35498
weighted.median(SNRI_Periods$Total_Duration, SNRI_Periods$weight)  #9.5






#Muscle Relax
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('78',.), ~replace(., grepl('78', .), "Muscle")) %>%
  mutate_if(grepl('79',.), ~replace(., grepl('79', .), "Muscle")) %>%
  mutate_if(grepl('80',.), ~replace(., grepl('80', .), "Muscle")) %>%
  mutate_if(grepl('81',.), ~replace(., grepl('81', .), "Muscle")) %>%
  mutate_if(grepl('82',.), ~replace(., grepl('82', .), "Muscle"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Muscle",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Muscle_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Muscle_Periods)[3] <- "Duration"
Muscle_Periods <- Muscle_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Muscle_Periods <- Muscle_Periods %>% left_join(MIG_Drug_Histories) 
Muscle_Periods <- Muscle_Periods %>% mutate(weight = as.numeric(weight))
Muscle_Periods <- Muscle_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Muscle_Periods <- Muscle_Periods %>% distinct()

library(spatstat)
weighted.mean(Muscle_Periods$Total_Duration, Muscle_Periods$weight)  #4.557787
weighted.median(Muscle_Periods$Total_Duration, Muscle_Periods$weight)  #1.5









#BetaBlocker 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('100',.), ~replace(., grepl('100', .), "BetaBlocker")) %>%
  mutate_if(grepl('101',.), ~replace(., grepl('101', .), "BetaBlocker")) %>%
  mutate_if(grepl('102',.), ~replace(., grepl('102', .), "BetaBlocker")) %>%
  mutate_if(grepl('103',.), ~replace(., grepl('103', .), "BetaBlocker")) %>%
  mutate_if(grepl('104',.), ~replace(., grepl('104', .), "BetaBlocker")) %>%
  mutate_if(grepl('105',.), ~replace(., grepl('105', .), "BetaBlocker")) %>%
  mutate_if(grepl('106',.), ~replace(., grepl('106', .), "BetaBlocker"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="BetaBlocker",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

BetaBlocker_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(BetaBlocker_Periods)[3] <- "Duration"
BetaBlocker_Periods <- BetaBlocker_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
BetaBlocker_Periods <- BetaBlocker_Periods %>% left_join(MIG_Drug_Histories) 
BetaBlocker_Periods <- BetaBlocker_Periods %>% mutate(weight = as.numeric(weight))
BetaBlocker_Periods <- BetaBlocker_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
BetaBlocker_Periods <- BetaBlocker_Periods %>% distinct()

library(spatstat)
weighted.mean(BetaBlocker_Periods$Total_Duration, BetaBlocker_Periods$weight)  #8.249293
weighted.median(BetaBlocker_Periods$Total_Duration, BetaBlocker_Periods$weight)  #9.5








#Cardiovascular 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('95',.), ~replace(., grepl('95', .), "Cardiovascular")) %>%
  mutate_if(grepl('96',.), ~replace(., grepl('96', .), "Cardiovascular")) %>%
  mutate_if(grepl('97',.), ~replace(., grepl('97', .), "Cardiovascular")) %>%
  mutate_if(grepl('98',.), ~replace(., grepl('98', .), "Cardiovascular")) %>%
  mutate_if(grepl('99',.), ~replace(., grepl('99', .), "Cardiovascular"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Cardiovascular",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Cardiovascular_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Cardiovascular_Periods)[3] <- "Duration"
Cardiovascular_Periods <- Cardiovascular_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Cardiovascular_Periods <- Cardiovascular_Periods %>% left_join(MIG_Drug_Histories) 
Cardiovascular_Periods <- Cardiovascular_Periods %>% mutate(weight = as.numeric(weight))
Cardiovascular_Periods <- Cardiovascular_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Cardiovascular_Periods <- Cardiovascular_Periods %>% distinct()

library(spatstat)
weighted.mean(Cardiovascular_Periods$Total_Duration, Cardiovascular_Periods$weight)  #8.917927
weighted.median(Cardiovascular_Periods$Total_Duration, Cardiovascular_Periods$weight)  #11.5






#Sedative 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('60',.), ~replace(., grepl('60', .), "Sedative")) %>%
  mutate_if(grepl('61',.), ~replace(., grepl('61', .), "Sedative")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Sedative",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Sedative_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Sedative_Periods)[3] <- "Duration"
Sedative_Periods <- Sedative_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Sedative_Periods <- Sedative_Periods %>% left_join(MIG_Drug_Histories) 
Sedative_Periods <- Sedative_Periods %>% mutate(weight = as.numeric(weight))
Sedative_Periods <- Sedative_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Sedative_Periods <- Sedative_Periods %>% distinct()

library(spatstat)
weighted.mean(Sedative_Periods$Total_Duration, Sedative_Periods$weight)  #3.705711
weighted.median(Sedative_Periods$Total_Duration, Sedative_Periods$weight)  #1







#Tricyclic 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('112',.), ~replace(., grepl('112', .), "Tricyclic")) %>%
  mutate_if(grepl('113',.), ~replace(., grepl('113', .), "Tricyclic"))%>%
  mutate_if(grepl('113',.), ~replace(., grepl('114', .), "Tricyclic")) %>%
  mutate_if(grepl('115',.), ~replace(., grepl('115', .), "Tricyclic"))%>%
  mutate_if(grepl('116',.), ~replace(., grepl('116', .), "Tricyclic")) %>%
  mutate_if(grepl('117',.), ~replace(., grepl('117', .), "Tricyclic"))%>%
  mutate_if(grepl('118',.), ~replace(., grepl('118', .), "Tricyclic")) %>%
  mutate_if(grepl('119',.), ~replace(., grepl('119', .), "Tricyclic"))%>%
  mutate_if(grepl('120',.), ~replace(., grepl('120', .), "Tricyclic")) %>%
  mutate_if(grepl('121',.), ~replace(., grepl('121', .), "Tricyclic"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Tricyclic",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Tricyclic_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Tricyclic_Periods)[3] <- "Duration"
Tricyclic_Periods <- Tricyclic_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Tricyclic_Periods <- Tricyclic_Periods %>% left_join(MIG_Drug_Histories) 
Tricyclic_Periods <- Tricyclic_Periods %>% mutate(weight = as.numeric(weight))
Tricyclic_Periods <- Tricyclic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Tricyclic_Periods <- Tricyclic_Periods %>% distinct()

library(spatstat)
weighted.mean(Tricyclic_Periods$Total_Duration, Tricyclic_Periods$weight)  #6.913634
weighted.median(Tricyclic_Periods$Total_Duration, Tricyclic_Periods$weight)  # 6.5






# Analgesic 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "Analgesic")) %>%
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "Analgesic")) %>%
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "Analgesic")) %>%
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "Analgesic")) %>%
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "Analgesic")) %>%
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "Analgesic")) %>%
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "Analgesic")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Analgesic",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Analgesic_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Analgesic_Periods)[3] <- "Duration"
Analgesic_Periods <- Analgesic_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Analgesic_Periods <- Analgesic_Periods %>% left_join(MIG_Drug_Histories) 
Analgesic_Periods <- Analgesic_Periods %>% mutate(weight = as.numeric(weight))
Analgesic_Periods <- Analgesic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Analgesic_Periods <- Analgesic_Periods %>% distinct()

library(spatstat)
weighted.mean(Analgesic_Periods$Total_Duration, Analgesic_Periods$weight)  # 8.03497
weighted.median(Analgesic_Periods$Total_Duration, Analgesic_Periods$weight)  # 8.5









# Calcium 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('107',.), ~replace(., grepl('107', .), "Calcium")) %>%
  mutate_if(grepl('108',.), ~replace(., grepl('108', .), "Calcium")) %>%
  mutate_if(grepl('109',.), ~replace(., grepl('109', .), "Calcium")) %>%
  mutate_if(grepl('110',.), ~replace(., grepl('110', .), "Calcium")) %>%
  mutate_if(grepl('111',.), ~replace(., grepl('111', .), "Calcium")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Calcium",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Calcium_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Calcium_Periods)[3] <- "Duration"
Calcium_Periods <- Calcium_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Calcium_Periods <- Calcium_Periods %>% left_join(MIG_Drug_Histories) 
Calcium_Periods <- Calcium_Periods %>% mutate(weight = as.numeric(weight))
Calcium_Periods <- Calcium_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Calcium_Periods <- Calcium_Periods %>% distinct()

library(spatstat)
weighted.mean(Calcium_Periods$Total_Duration, Calcium_Periods$weight)  # 7.748456
weighted.median(Calcium_Periods$Total_Duration, Calcium_Periods$weight)  # 8.5






# Antipsychotic 
RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))
MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(51:62)

MIG_Drug_Histories <- MIG_Drug_Histories %>% 
  mutate_if(grepl('62',.), ~replace(., grepl('62', .), "Antipsychotic")) %>%
  mutate_if(grepl('63',.), ~replace(., grepl('63', .), "Antipsychotic")) %>%
  mutate_if(grepl('64',.), ~replace(., grepl('64', .), "Antipsychotic")) %>%
  mutate_if(grepl('65',.), ~replace(., grepl('65', .), "Antipsychotic")) %>%
  mutate_if(grepl('66',.), ~replace(., grepl('66', .), "Antipsychotic")) 

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antipsychotic",1,0))
MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories_LONG <- New_BoxJul21 %>% left_join(MIG_Drug_Histories_LONG)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(-c(New_BoxJul21))

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)


MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

Antipsychotic_Periods <- MIG_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())
names(Antipsychotic_Periods)[3] <- "Duration"
Antipsychotic_Periods <- Antipsychotic_Periods %>% select(patient, Duration) 

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight) %>% distinct()
Antipsychotic_Periods <- Antipsychotic_Periods %>% left_join(MIG_Drug_Histories) 
Antipsychotic_Periods <- Antipsychotic_Periods %>% mutate(weight = as.numeric(weight))
Antipsychotic_Periods <- Antipsychotic_Periods %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)
Antipsychotic_Periods <- Antipsychotic_Periods %>% distinct()

library(spatstat)
weighted.mean(Antipsychotic_Periods$Total_Duration, Antipsychotic_Periods$weight)  # 2.709868
weighted.median(Antipsychotic_Periods$Total_Duration, Antipsychotic_Periods$weight)  # 1



# Penetration
Penetrance_vs_Duration <- read.csv("Penetrance_vs_Duration.csv")
names(Penetrance_vs_Duration) <- c("drug_class", "penetrance", "duration")

library(ggrepel)
library(hrbrthemes)
library(viridis)

ggplot(Penetrance_vs_Duration, aes(x=penetrance, y=duration, size = penetrance, fill=penetrance, colour=penetrance)) +
  geom_point(alpha=0.7)+
  xlim(0,40)+
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
  ylab("Weighted Average Duration (months)\n")

# --------
# Inflows to RImegepant: Nr Mols, Nr Lines, % Neurologist -------------------------
MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))

MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("135",d1)) %>% 
  filter(grepl("135",d2)) %>% group_by(s1) %>% summarise(pats=sum(as.numeric(weight)))
# #Inflows

# Nr of Lines on each Month
MIG_nrLines_Histories <- read.table("MIG_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

MIG_nrLines_Histories <- gather(MIG_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_nrLines_Histories$Month <- as.character(MIG_nrLines_Histories$Month)
MIG_nrLines_Histories$Month <- parse_number(MIG_nrLines_Histories$Month)

MIG_nrLines_Histories$Treat <- as.numeric(MIG_nrLines_Histories$Treat)

MIG_nrLines_Histories <- MIG_nrLines_Histories %>% select(patient, Month, Treat)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% left_join(MIG_nrLines_Histories, by=c("patient"="patient", "p1"="Month"))

MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("135",d1)) %>% 
  filter(grepl("135",d2)) %>% group_by(s1) %>% summarise(n=weighted.mean(Treat, as.numeric(weight)))




# Nr of Molecules on the months before
temp <- MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("135",d1)) %>%  filter(grepl("135",d2)) %>%
  select(patient, weight, p1, d1, s1)

temp <- separate_rows(temp, d1, sep = ",", convert=T)
temp <- temp %>% group_by(patient, weight, p1, s1) %>% count()
temp %>% ungroup() %>% group_by(s1)  %>% summarise(Mols=weighted.mean(n, as.numeric(weight)))


# Seen by neurologist last 12months
patients_to_track <- MIG_Flows_Aux._Long %>% filter(p1 >=48) %>% filter(!grepl("135",d1)) %>% 
  filter(grepl("135",d2)) %>% select(patient, p1, s1)

weightsTojoin <- MIG_Flows_Aux._Long %>% select(patient, weight) %>% distinct()

# Physicians
Physicians_Vanguard_Lookup <- read.csv("Physicians_Vanguard_Lookup.csv", colClasses = "character", stringsAsFactors = FALSE)
removeQuotes <- function(x) gsub("\'", "", x)
Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% mutate_if(is.character, removeQuotes)
names(Physicians_Vanguard_Lookup)[1] <- "specialty"

# Months Lookup
Months_lookup <- fread("Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
Months_lookup$Month <- paste0(Months_lookup$Month,"-1")
Months_lookup$Month <- format(as.Date(Months_lookup$Month), "%Y-%m")

# All scripts
MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(pat_id, from_dt, specialty)
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2019-04-01" & from_dt <= "2021-07-31") 
names(MIG_Doses_BIG)[1] <- "patient"

MIG_Doses_BIG <- MIG_Doses_BIG %>% left_join(Physicians_Vanguard_Lookup) %>% filter(Physician=="NEUROLOGIST") %>%
  select(-c(specialty)) %>% distinct()

MIG_Doses_BIG$from_dt <- format(as.Date(MIG_Doses_BIG$from_dt), "%Y-%m")

MIG_Doses_BIG <- MIG_Doses_BIG %>% left_join(Months_lookup, by=c("from_dt"="Month"))

temp <- patients_to_track %>% inner_join(MIG_Doses_BIG, by=c("patient"="patient")) 

temp <- temp %>% group_by(patient) %>% filter((Exact_Month <= p1) & (Exact_Month >= p1-12))

temp <- temp %>% left_join(weightsTojoin) 

temp <- temp %>% select(patient, p1, s1, weight) %>% distinct()

temp %>% ungroup() %>% group_by(s1) %>% summarise(n=sum(as.numeric(weight)))

# --------

# Rimegepant pats to vizualize -------
MIG_Drug_Histories <- fread("MIG Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

RimegepantPats <- MIG_Drug_Histories %>% filter(grepl("135",month60)&!grepl("137",month60)&!grepl("138",month60)&!grepl("139",month60)&!grepl("140",month60)) %>% select(patient)
RimegepantPats <- RimegepantPats %>% mutate(patient = paste(patient, collapse= ","))
RimegepantPats <- RimegepantPats %>% distinct()
fwrite(RimegepantPats, "RimegepantPats.txt", sep="\t")




MIG_Drug_Histories <- fread("MIG Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

OralPats <- MIG_Drug_Histories %>% filter(grepl("135",month60)|grepl("136",month60)) %>% select(patient)
OralPats <- OralPats %>% mutate(patient = paste(patient, collapse= ","))
OralPats <- OralPats %>% distinct()
fwrite(OralPats, "OralPats.txt", sep="\t")



New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21 != "Mild") %>% select(patient)
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)

MIG_Flows_Aux._Long <- read.table("MIG_Flows_Aux._Long_v2.txt", header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p2=as.numeric(p2))

MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(patient, weight, p2, s2, starts)

New_BoxJul21 %>% left_join(MIG_Flows_Aux._Long) %>% filter(p2>=49) %>% filter(starts == "1") %>% group_by(s2) %>%
  summarise(n=sum(as.numeric(weight)))

# ----------------

# Develop a classification method for CGRP Experience ---------------

RIME_Ingredients <- fread("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients$drug_id <- unlist(lapply(RIME_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))

# Class lookups
string_NSAID <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "NSAID"], collapse = "|"),")\\b")
string_Analgesic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Analgesic"], collapse = "|"),")\\b")
string_WeakOpioid <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Weak Opioid"], collapse = "|"),")\\b")
string_StrongOpioid <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Strong Opioid"], collapse = "|"),")\\b")
string_Antiemetic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Antiemetic"], collapse = "|"),")\\b")
string_Steroid <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Steroid"], collapse = "|"),")\\b")
string_Sedative <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Sedative"], collapse = "|"),")\\b")
string_Antipsychotic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Antipsychotic"], collapse = "|"),")\\b")
string_Hospitalization <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Hospitalization"], collapse = "|"),")\\b")
string_Ergot <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Ergot"], collapse = "|"),")\\b")
string_Triptan <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Triptan"], collapse = "|"),")\\b")
string_Ditan <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Ditan"], collapse = "|"),")\\b")
string_MuscleRelaxant <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Muscle Relaxant"], collapse = "|"),")\\b")
string_Antiepileptic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Antiepileptic"], collapse = "|"),")\\b")
string_Cardiovascular <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Cardiovascular"], collapse = "|"),")\\b")
string_BetaBlocker <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Beta Blocker"], collapse = "|"),")\\b")
string_CalciumBlocker <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Calcium Blocker"], collapse = "|"),")\\b")
string_Tricyclic <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Tricyclic"], collapse = "|"),")\\b")
string_SSRI <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "SSRI"], collapse = "|"),")\\b")
string_SNRI <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "SNRI"], collapse = "|"),")\\b")
string_Neural <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "Neural"], collapse = "|"),")\\b")
string_CGRPOral <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "CGRP Oral"], collapse = "|"),")\\b")
string_CGRPInjectable <- paste0("\\b(",paste0(RIME_Ingredients$drug_id[RIME_Ingredients$drug_class == "CGRP Injectable"], collapse = "|"),")\\b")




MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(4:63)
# sum across rows, to see hoe many remain zero "0" 
MIG_Drug_Histories[MIG_Drug_Histories != "-"] <- 1  # on drug 
MIG_Drug_Histories[MIG_Drug_Histories == "-"] <- 0  # no drug
MIG_Drug_Histories[] <- lapply(MIG_Drug_Histories, as.numeric)
MIG_Drug_Histories$SUM <- rowSums(MIG_Drug_Histories)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)
MIG_Drug_Histories_LONG<- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% filter(SUM != 0)

fwrite(MIG_Drug_Histories_LONG, "Treatment_exp_Vector.txt", sep="\t")

Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt",  colClasses = "character")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)



# Cumulative drug class experience every month 
MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", integer64 = "character", stringsAsFactors = F)
MIG_Flows_Aux._Long <- Treatment_exp_Vector %>% left_join(MIG_Flows_Aux._Long)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(-c(disease, starts, stops, re_starts))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(-c(s1, s2, p1_RxExp, flow, weight))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_OralExp = ifelse(grepl("135",d1)|grepl("135",d2)|grepl("136",d1)|grepl("136",d2),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = cumsum(p1_OralExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = ifelse(p1_OralExp==0,0,1))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_InjExp = ifelse(grepl("137",d1)|grepl("138",d1)|grepl("139",d1)|grepl("140",d1)|
                                                                           grepl("137",d2)|grepl("138",d2)|grepl("139",d2)|grepl("140",d2),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = cumsum(p1_InjExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = ifelse(p1_InjExp==0,0,1))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_NeuralExp = ifelse(grepl("132",d1)|grepl("133",d1)|grepl("134",d1)|
                                                                              grepl("132",d2)|grepl("133",d2)|grepl("134",d2),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_NeuralExp = cumsum(p1_NeuralExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_NeuralExp = ifelse(p1_NeuralExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_SNRIExp = ifelse(grepl("127",d1)|grepl("128",d1)|grepl("129",d1)|grepl("130",d1)|grepl("131",d1)|
                                                                            grepl("127",d2)|grepl("128",d2)|grepl("129",d2)|grepl("130",d2)|grepl("131",d2),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SNRIExp = cumsum(p1_SNRIExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SNRIExp = ifelse(p1_SNRIExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_SSRIExp = ifelse(grepl("122",d1)|grepl("123",d1)|grepl("124",d1)|grepl("125",d1)|grepl("126",d1)|
                                                                            grepl("122",d2)|grepl("123",d2)|grepl("124",d2)|grepl("125",d2)|grepl("126",d2),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SSRIExp = cumsum(p1_SSRIExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SSRIExp = ifelse(p1_SSRIExp==0,0,1))




MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_TricyclicExp = ifelse(grepl("112",d1)|grepl("113",d1)|grepl("114",d1)|grepl("115",d1)|
                                                                                 grepl("116",d1)|grepl("117",d1)|grepl("118",d1)|grepl("119",d1)|
                                                                                 grepl("120",d1)|grepl("121",d1)|
                                                                                 grepl("112",d2)|grepl("113",d2)|grepl("114",d2)|grepl("115",d2)|
                                                                                 grepl("116",d2)|grepl("117",d2)|grepl("118",d2)|grepl("119",d2)|
                                                                                 grepl("120",d2)|grepl("121",d2),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_TricyclicExp = cumsum(p1_TricyclicExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_TricyclicExp = ifelse(p1_TricyclicExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_CalciumExp = ifelse(grepl("107",d1)|grepl("108",d1)|grepl("109",d1)|grepl("110",d1)|grepl("111",d1)|
                                                                               grepl("107",d2)|grepl("108",d2)|grepl("109",d2)|grepl("110",d2)|grepl("111",d2),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_CalciumExp = cumsum(p1_CalciumExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_CalciumExp = ifelse(p1_CalciumExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_BetaBlockerExp = ifelse(str_detect(d1, string_BetaBlocker)|str_detect(d2, string_BetaBlocker),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_BetaBlockerExp = cumsum(p1_BetaBlockerExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_BetaBlockerExp = ifelse(p1_BetaBlockerExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_CardiovascularExp = ifelse(str_detect(d1, string_Cardiovascular)|str_detect(d2, string_Cardiovascular),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_CardiovascularExp = cumsum(p1_CardiovascularExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_CardiovascularExp = ifelse(p1_CardiovascularExp==0,0,1))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_AntiepilepticExp = ifelse(str_detect(d1, string_Antiepileptic)|str_detect(d2, string_Antiepileptic),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntiepilepticExp = cumsum(p1_AntiepilepticExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntiepilepticExp = ifelse(p1_AntiepilepticExp==0,0,1))




MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_MuscleRelaxantExp = ifelse(str_detect(d1, string_MuscleRelaxant)|str_detect(d2, string_MuscleRelaxant),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_MuscleRelaxantExp = cumsum(p1_MuscleRelaxantExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_MuscleRelaxantExp = ifelse(p1_MuscleRelaxantExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_DitanExp = ifelse(str_detect(d1, string_Ditan)|str_detect(d2, string_Ditan),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_DitanExp = cumsum(p1_DitanExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_DitanExp = ifelse(p1_DitanExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_TriptanExp = ifelse(str_detect(d1, string_Triptan)|str_detect(d2, string_Triptan),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_TriptanExp = cumsum(p1_TriptanExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_TriptanExp = ifelse(p1_TriptanExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_ErgotExp = ifelse(str_detect(d1, string_Ergot)|str_detect(d2, string_Ergot),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ErgotExp = cumsum(p1_ErgotExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ErgotExp = ifelse(p1_ErgotExp==0,0,1))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_ErgotExp = ifelse(str_detect(d1, string_Ergot)|str_detect(d2, string_Ergot),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ErgotExp = cumsum(p1_ErgotExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_ErgotExp = ifelse(p1_ErgotExp==0,0,1))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_HospitalizationExp = ifelse(str_detect(d1, string_Hospitalization)|str_detect(d2, string_Hospitalization),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_HospitalizationExp = cumsum(p1_HospitalizationExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_HospitalizationExp = ifelse(p1_HospitalizationExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_AntipsychoticExp = ifelse(str_detect(d1, string_Antipsychotic)|str_detect(d2, string_Antipsychotic),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntipsychoticExp = cumsum(p1_AntipsychoticExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntipsychoticExp = ifelse(p1_AntipsychoticExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_SedativeExp = ifelse(str_detect(d1, string_Sedative)|str_detect(d2, string_Sedative),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SedativeExp = cumsum(p1_SedativeExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SedativeExp = ifelse(p1_SedativeExp==0,0,1))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_SteroidExp = ifelse(str_detect(d1, string_Steroid)|str_detect(d2, string_Steroid),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SteroidExp = cumsum(p1_SteroidExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SteroidExp = ifelse(p1_SteroidExp==0,0,1))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_AntiemeticExp = ifelse(str_detect(d1, string_Antiemetic)|str_detect(d2, string_Antiemetic),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntiemeticExp = cumsum(p1_AntiemeticExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntiemeticExp = ifelse(p1_AntiemeticExp==0,0,1))




MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_StrongOpioidExp = ifelse(str_detect(d1, string_StrongOpioid)|str_detect(d2, string_StrongOpioid),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_StrongOpioidExp = cumsum(p1_StrongOpioidExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_StrongOpioidExp = ifelse(p1_StrongOpioidExp==0,0,1))



MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_WeakOpioidExp = ifelse(str_detect(d1, string_WeakOpioid)|str_detect(d2, string_WeakOpioid),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_WeakOpioidExp = cumsum(p1_WeakOpioidExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_WeakOpioidExp = ifelse(p1_WeakOpioidExp==0,0,1))





MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_AnalgesicExp = ifelse(str_detect(d1, string_Analgesic)|str_detect(d2, string_Analgesic),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AnalgesicExp = cumsum(p1_AnalgesicExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AnalgesicExp = ifelse(p1_AnalgesicExp==0,0,1))




MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% mutate(p1_NSAIDExp = ifelse(str_detect(d1, string_NSAID)|str_detect(d2, string_NSAID),1,0))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_NSAIDExp = cumsum(p1_NSAIDExp))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_NSAIDExp = ifelse(p1_NSAIDExp==0,0,1))


MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(-c(d1, d2))

fwrite(MIG_Flows_Aux._Long, "Cum_Class_Experience_EveryMonth.txt", sep="\t")






# Get GENDER & MAX AGE & Comorbidity status per patient
# Demographics

MIG_Demographics <- fread("MIG Demographics.txt")

names(MIG_Demographics)[1] <- "patient"

MIG_Demographics <- MIG_Demographics %>% select(patient, age, gender, intractable_earliest, severe_earliest,
                                                chronic_earliest, aura_earliest)


MIG_Demographics <- MIG_Demographics %>% mutate(intractable_earliest = ifelse(is.na(intractable_earliest), 0,1)) %>%
  mutate(severe_earliest  = ifelse(is.na(severe_earliest ), 0,1)) %>%
  mutate(chronic_earliest = ifelse(is.na(chronic_earliest), 0,1)) %>%
  mutate(aura_earliest = ifelse(is.na(aura_earliest), 0,1))


fwrite(MIG_Demographics, "MIG_Demographics_Short_Predictors.txt", sep="\t")






# Number of lines and number of drugs and number of flows 

# Flows
Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t",  colClasses = "character")

MIG_Flows_Aux._Long <- fread("MIG_Flows_Aux._Long_v2.txt", integer64 = "character", colClasses = "character", stringsAsFactors = F)
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(-c(disease, starts, stops, re_starts))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% select(-c(s1, s2, p1_RxExp, weight))
MIG_Flows_Aux._Long <- MIG_Flows_Aux._Long %>% group_by(patient) %>% mutate(cumflow = cumsum(flow))

Cummulative_Flows <- MIG_Flows_Aux._Long  %>% select(patient, p1, p2, cumflow)

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% left_join(Cummulative_Flows)

fwrite(Cum_Class_Experience_EveryMonth, "Cum_Class_Experience_EveryMonth.txt", sep="\t")

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", colClasses = "character")




# Lines
drgMIG2 <- fread("MIG Drug Histories.txt", integer64 = "character", colClasses = "character", stringsAsFactors = F)

data <- data.frame(drgMIG2, stringsAsFactors = F)

nrLines <- data[,c(1:3)] 
nrLines$month1 <- (data$month1 != "-")*1

for(i in 2:60){
  cat(i)
  nrLines[,i+3] <- apply(data[,(4:(i+3))], 1, function(x) length(unique(x[x!="-"])))
  names(nrLines)[i+3] <- paste0("month",i)
}

fwrite(nrLines,"MIG_nrLines_Histories.txt")

nrLines <- gather(nrLines, Month, Lines, month1:month60, factor_key=TRUE)

nrLines <- nrLines %>% select(patient, Month, Lines)

nrLines$Month <- as.character(nrLines$Month)
nrLines$Month <- parse_number(nrLines$Month)
nrLines$Month <- as.character(nrLines$Month)

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% left_join(nrLines, by=c("patient"="patient", "p2"="Month"))

fwrite(Cum_Class_Experience_EveryMonth, "Cum_Class_Experience_EveryMonth.txt", sep="\t")


# Nr drugs cumm

MIG_Drug_Histories <- fread("MIG Drug Histories.txt", integer64 = "character",  colClasses = "character", stringsAsFactors = F)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-disease)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-weight)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Drugs != "-")

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Drugs, sep = ",", convert=T )

MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% count()

names(MIG_Drug_Histories)[2] <- "Diff_Drugs_exp"


fwrite(MIG_Drug_Histories, "Number_Drugs_EverExperienced.txt", sep="\t")



Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% left_join(MIG_Drug_Histories, by=c("patient"="patient"))

fwrite(Cum_Class_Experience_EveryMonth, "Cum_Class_Experience_EveryMonth.txt", sep="\t")




# Physicians Experience

Physicians_Vanguard_Lookup <- read.csv("Physicians_Vanguard_Lookup.csv", colClasses = "character", stringsAsFactors = FALSE)
removeQuotes <- function(x) gsub("\'", "", x)
Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% mutate_if(is.character, removeQuotes)
names(Physicians_Vanguard_Lookup)[1] <- "specialty"

MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(status != "G")
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(-c(drug_id, weight, dayssup, taxonomy1, taxonomy2, status))
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 
names(MIG_Doses_BIG)[4] <- "patient"


MIG_Doses_BIG <- MIG_Doses_BIG %>% select(patient, specialty) %>% distinct()
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(specialty != "")
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(!is.na(specialty))

MIG_Doses_BIG <- MIG_Doses_BIG  %>% left_join(Physicians_Vanguard_Lookup) 
MIG_Doses_BIG <- MIG_Doses_BIG %>% select(patient, Physician) %>% distinct()
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(Physician_Exp = 1) %>% drop_na() %>%
  spread(key=Physician, value=Physician_Exp)


Treatment_exp_Vector <- fread("Treatment_exp_Vector.txt",  colClasses = "character")
Treatment_exp_Vector <- Treatment_exp_Vector %>% select(patient)

MIG_Doses_BIG <- Treatment_exp_Vector %>% left_join(MIG_Doses_BIG)

MIG_Doses_BIG[is.na(MIG_Doses_BIG)] <- 0

fwrite(MIG_Doses_BIG, "Physicians_Experience.txt", sep="\t")







# Charlson
MIG_Disorder_Histories <- fread("MIG Disorder Histories.txt" ,  colClasses = "character")
MIG_Disorder_Histories <- MIG_Disorder_Histories %>% select(patient, month60)

MIG_Disorder_Histories <- MIG_Disorder_Histories %>% mutate(comorbs = str_extract(month60, "[a-z]+"))

MIG_Disorder_Histories <- MIG_Disorder_Histories %>% filter(!is.na(comorbs))

MIG_Disorder_Histories

MIG_Disorder_Histories_sep <- separate_rows(MIG_Disorder_Histories, comorbs, sep = "", convert=F )

MIG_Disorder_Histories_sep <- MIG_Disorder_Histories_sep %>% filter(comorbs != "")

MIG_Disorder_Histories_sep <- MIG_Disorder_Histories_sep %>% select(-c(month60))

MIG_Disorder_Histories_sep <- MIG_Disorder_Histories_sep %>% spread(key=comorbs, value=comorbs)

MIG_Disorder_Histories_sep <- MIG_Disorder_Histories_sep %>% 
  mutate(a=ifelse(is.na(a),0,1)) %>%
  mutate(c=ifelse(is.na(c),0,1)) %>%
  mutate(d=ifelse(is.na(d),0,1)) %>%
  mutate(f=ifelse(is.na(f),0,1)) %>%
  mutate(k=ifelse(is.na(k),0,1)) %>%
  mutate(l=ifelse(is.na(l),0,1)) %>%
  mutate(p=ifelse(is.na(p),0,1)) %>%
  mutate(r=ifelse(is.na(r),0,1)) %>%
  mutate(s=ifelse(is.na(s),0,1)) %>%
  mutate(u=ifelse(is.na(u),0,1)) %>%
  mutate(v=ifelse(is.na(v),0,1)) %>%
  mutate(z=ifelse(is.na(z),0,1))


fwrite(MIG_Disorder_Histories_sep, "MIG_Comorbidities_wide.txt", sep="\t")





# plan, income

MIG_Demographics <- fread("MIG Demographics.txt",  colClasses = "character")

names(MIG_Demographics)[1] <- "patient"

MIG_Demographics <- MIG_Demographics %>% select(patient, plan, income)

MIG_Demographics$planID <- 1

MIG_Demographics <- MIG_Demographics %>% spread(key=plan, value=planID)


MIG_Demographics[is.na(MIG_Demographics)] <- 0


Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t",  colClasses = "character")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% left_join(MIG_Demographics)

fwrite(Cum_Class_Experience_EveryMonth, "Cum_Class_Experience_EveryMonth.txt", sep="\t")




# Time spent lapsed
MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories <-  MIG_Drug_Histories %>%  select(4:63)

MIG_Drug_Histories <- MIG_Drug_Histories %>% mutate_if(grepl('-',.), ~replace(., grepl('-', .), "Lapsed"))

MIG_Drug_Histories <-  MIG_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Lapsed",1,0))

MIG_Drug_Histories[] <-  lapply(MIG_Drug_Histories,as.numeric)

MIG_Drug_Histories_LONG <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Drug_Histories_LONG <- MIG_Drug_Histories_LONG %>% select(patient, weight)

MIG_Drug_Histories <- MIG_Drug_Histories_LONG %>% bind_cols(MIG_Drug_Histories)
rm(MIG_Drug_Histories_LONG)

MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

MIG_Drug_Histories$Month <- as.character(MIG_Drug_Histories$Month)
MIG_Drug_Histories$Month <- parse_number(MIG_Drug_Histories$Month)

MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat == 1)

names(MIG_Drug_Histories)[2] <- "LapsedTime"

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t",  colClasses = "character")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% left_join(MIG_Drug_Histories) %>% mutate(LapsedTime=ifelse(is.na(LapsedTime),0, LapsedTime))

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% mutate(TreatedTime = 60-LapsedTime)

fwrite(Cum_Class_Experience_EveryMonth, "Cum_Class_Experience_EveryMonth.txt", sep="\t")









# Logistic regression CGRP  Exp vs No CGRP 

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t",  colClasses = "character")
MIG_Comorbidities_wide <- fread("MIG_Comorbidities_wide.txt", sep="\t",  colClasses = "character")
Physicians_Experience <- fread("Physicians_Experience.txt", sep="\t",  colClasses = "character")
MIG_Demographics_Short_Predictors <- fread("MIG_Demographics_Short_Predictors.txt", sep="\t" ,  colClasses = "character")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% filter(p2==60) %>% select(-c(p1, p2))

Dems_Labs_TreatExp <- MIG_Demographics_Short_Predictors %>% inner_join(Cum_Class_Experience_EveryMonth)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% inner_join(MIG_Comorbidities_wide) %>% inner_join(Physicians_Experience)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(gender = ifelse(gender=="M", 1, 0))

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-patient)

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate_if(is_all_numeric,as.numeric)


Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(Group = ifelse(p1_InjExp==1|p1_OralExp ==1, "CGRP_Exp", "No_CGRP"))

Dems_Labs_TreatExp$Group <- as.factor(Dems_Labs_TreatExp$Group)

Dems_Labs_TreatExp$Group <- relevel(Dems_Labs_TreatExp$Group,"No_CGRP")

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-c(p1_InjExp, p1_OralExp))



Dems_Labs_TreatExp %>% group_by(Group) %>% count()

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% group_by(Group) %>% sample_n(5522)
Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% ungroup()




create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}



Dems_Labs_TreatExp <- Dems_Labs_TreatExp[sample(1:nrow(Dems_Labs_TreatExp)), ]

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(c(Group, age, intractable_earliest, severe_earliest, chronic_earliest,
                                                      p1_SSRIExp, p1_CardiovascularExp, p1_TriptanExp,
                                                      p1_WeakOpioidExp, Lines, C, LapsedTime, NEUROLOGIST))

data_train <- create_train_test(Dems_Labs_TreatExp, 0.8, train = TRUE)
data_test <- create_train_test(Dems_Labs_TreatExp, 0.8, train = FALSE)

Risk_pred_model <- glm( Group ~ ., data = data_train, family = binomial)

summary(Risk_pred_model)

predict <- predict(Risk_pred_model, data_test, type = 'response')

table_mat <- table(data_test$Group, predict > 0.50)
table_mat

plot(table_mat)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test # 0.8601177


precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}



recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}


prec <- precision(table_mat)
prec # 0.859633

rec <- recall(table_mat)
rec # 0.8572736

f1 <- 2 * ((prec * rec) / (prec + rec))
f1 # 0.8584517


Odd_ratios_CGRP_noCGRP_Treat <- fread("Odd_ratios_CGRP_noCGRP_Treat.txt", sep="\t")

Odd_ratios_CGRP_noCGRP_Treat$Predictor <- as.factor(Odd_ratios_CGRP_noCGRP_Treat$Predictor)

Odd_ratios_CGRP_noCGRP_Treat <- Odd_ratios_CGRP_noCGRP_Treat %>% arrange(`Odd ratio`)


Odd_ratios_CGRP_noCGRP_Treat %>%
  ggplot() +
  geom_segment( aes(x=reorder(Predictor, `Odd ratio`) , xend=reorder(Predictor, `Odd ratio`), y=Low, yend=High), color="brown3", size=1) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=Low), color="deepskyblue4", size=3 ) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=High), color="deeppink4", size=3 ) +
  coord_flip()+
  theme_ridges() +
  theme(legend.position = "none") +
  scale_y_continuous(trans='log10')+
  xlab("Predictor \n") +
  ylab("\n Log10 Odd ratio (Lower-Upper ends)")


























# Logistic regression CGRP Oral Exp vs No CGRP Oral (any)

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t",  colClasses = "character")
MIG_Comorbidities_wide <- fread("MIG_Comorbidities_wide.txt", sep="\t",  colClasses = "character")
Physicians_Experience <- fread("Physicians_Experience.txt", sep="\t",  colClasses = "character")
MIG_Demographics_Short_Predictors <- fread("MIG_Demographics_Short_Predictors.txt", sep="\t" ,  colClasses = "character")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% filter(p2==60) %>% select(-c(p1, p2))

Dems_Labs_TreatExp <- MIG_Demographics_Short_Predictors %>% inner_join(Cum_Class_Experience_EveryMonth)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% inner_join(MIG_Comorbidities_wide) %>% inner_join(Physicians_Experience)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(gender = ifelse(gender=="M", 1, 0))

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-patient)

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate_if(is_all_numeric,as.numeric)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(Group = ifelse(p1_OralExp ==1, "CGRP_Exp", "No_CGRP"))

Dems_Labs_TreatExp$Group <- as.factor(Dems_Labs_TreatExp$Group)

Dems_Labs_TreatExp$Group <- relevel(Dems_Labs_TreatExp$Group,"No_CGRP")

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-c(p1_OralExp))



Dems_Labs_TreatExp %>% group_by(Group) %>% count()

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% group_by(Group) %>% sample_n(1526)
Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% ungroup()




create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}



Dems_Labs_TreatExp <- Dems_Labs_TreatExp[sample(1:nrow(Dems_Labs_TreatExp)), ]

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(c(Group, chronic_earliest, p1_InjExp, p1_TriptanExp, `ANEST/PAIN`,NEUROLOGIST))

data_train <- create_train_test(Dems_Labs_TreatExp, 0.8, train = TRUE)
data_test <- create_train_test(Dems_Labs_TreatExp, 0.8, train = FALSE)

Risk_pred_model <- glm( Group ~ ., data = data_train, family = binomial)

summary(Risk_pred_model)


predict <- predict(Risk_pred_model, data_test, type = 'response')

table_mat <- table(data_test$Group, predict > 0.50)
table_mat

plot(table_mat)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test # 0.8461538


precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}



recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}


prec <- precision(table_mat)
prec # 0.8440678

rec <- recall(table_mat)
rec # 0.8383838

f1 <- 2 * ((prec * rec) / (prec + rec))
f1 # 0.8412162


Odd_ratios_CGRP_noCGRP_Treat <- fread("Odd_ratios_Oral_CGRP_All_Treat.txt", sep="\t")

Odd_ratios_CGRP_noCGRP_Treat$Predictor <- as.factor(Odd_ratios_CGRP_noCGRP_Treat$Predictor)

Odd_ratios_CGRP_noCGRP_Treat <- Odd_ratios_CGRP_noCGRP_Treat %>% arrange(`Odd ratio`)


Odd_ratios_CGRP_noCGRP_Treat %>%
  ggplot() +
  geom_segment( aes(x=reorder(Predictor, `Odd ratio`) , xend=reorder(Predictor, `Odd ratio`), y=Low, yend=High), color="brown3", size=1) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=Low), color="deepskyblue4", size=3 ) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=High), color="deeppink4", size=3 ) +
  coord_flip()+
  theme_ridges() +
  theme(legend.position = "none") +
  ylim(0,10)+
  #scale_y_continuous(trans='log10')+
  xlab("Predictor \n") +
  ylab("\n Odd ratio (Lower-Upper ends)")

















# Logistic regression CGRP Oral Exp vs No CGRP (excluding Injs)

Cum_Class_Experience_EveryMonth <- fread("Cum_Class_Experience_EveryMonth.txt", sep="\t",  colClasses = "character")
MIG_Comorbidities_wide <- fread("MIG_Comorbidities_wide.txt", sep="\t",  colClasses = "character")
Physicians_Experience <- fread("Physicians_Experience.txt", sep="\t",  colClasses = "character")
MIG_Demographics_Short_Predictors <- fread("MIG_Demographics_Short_Predictors.txt", sep="\t" ,  colClasses = "character")

Cum_Class_Experience_EveryMonth <- Cum_Class_Experience_EveryMonth %>% filter(p2==60) %>% select(-c(p1, p2))

Dems_Labs_TreatExp <- MIG_Demographics_Short_Predictors %>% inner_join(Cum_Class_Experience_EveryMonth)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% inner_join(MIG_Comorbidities_wide) %>% inner_join(Physicians_Experience)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(gender = ifelse(gender=="M", 1, 0))

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-patient)

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate_if(is_all_numeric,as.numeric)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% filter(p1_InjExp == 0)

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% mutate(Group = ifelse(p1_OralExp ==1, "CGRP_Exp", "No_CGRP"))

Dems_Labs_TreatExp$Group <- as.factor(Dems_Labs_TreatExp$Group)

Dems_Labs_TreatExp$Group <- relevel(Dems_Labs_TreatExp$Group,"No_CGRP")

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(-c(p1_OralExp))



Dems_Labs_TreatExp %>% group_by(Group) %>% count()

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% group_by(Group) %>% sample_n(829)
Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% ungroup()




create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}



Dems_Labs_TreatExp <- Dems_Labs_TreatExp[sample(1:nrow(Dems_Labs_TreatExp)), ]

Dems_Labs_TreatExp <- Dems_Labs_TreatExp %>% select(c(Group, chronic_earliest, p1_TriptanExp, C,NEUROLOGIST))

data_train <- create_train_test(Dems_Labs_TreatExp, 0.8, train = TRUE)
data_test <- create_train_test(Dems_Labs_TreatExp, 0.8, train = FALSE)

Risk_pred_model <- glm( Group ~ ., data = data_train, family = binomial)

summary(Risk_pred_model)


predict <- predict(Risk_pred_model, data_test, type = 'response')

table_mat <- table(data_test$Group, predict > 0.50)
table_mat

plot(table_mat)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test # 0.810241


precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}



recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}


prec <- precision(table_mat)
prec # 0.8636364

rec <- recall(table_mat)
rec # 0.76

f1 <- 2 * ((prec * rec) / (prec + rec))
f1 # 0.8085106


Odd_ratios_CGRP_noCGRP_Treat <- fread("Odd_ratios_Oral_CGRP_All_Treat_(exc_All_Inj).txt", sep="\t")

Odd_ratios_CGRP_noCGRP_Treat$Predictor <- as.factor(Odd_ratios_CGRP_noCGRP_Treat$Predictor)

Odd_ratios_CGRP_noCGRP_Treat <- Odd_ratios_CGRP_noCGRP_Treat %>% arrange(`Odd ratio`)


Odd_ratios_CGRP_noCGRP_Treat %>%
  ggplot() +
  geom_segment( aes(x=reorder(Predictor, `Odd ratio`) , xend=reorder(Predictor, `Odd ratio`), y=Low, yend=High), color="brown3", size=1) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=Low), color="deepskyblue4", size=3 ) +
  geom_point( aes(x=reorder(Predictor, `Odd ratio`), y=High), color="deeppink4", size=3 ) +
  coord_flip()+
  theme_ridges() +
  theme(legend.position = "none") +
  ylim(0,10)+
  #scale_y_continuous(trans='log10')+
  xlab("Predictor \n") +
  ylab("\n Odd ratio (Lower-Upper ends)")










# ---------------------
# X-Y plot time vs penetrance US and japan ----------------------------


# US - - - - - -  -- - - - - - - - 


RIME_Ingredients <- read.table("RIME Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
RIME_Ingredients <- RIME_Ingredients %>% separate(drug_id, c('class', 'molecule'))

New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21$weight <- as.character(New_BoxJul21$weight)
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21 != "Mild")

MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))

length(unique(MIG_Drug_Histories$patient)) # 106269
sum(as.numeric(MIG_Drug_Histories$weight)) # 9638606

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(1,2,51:62)
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(Month))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()

data.frame(MIG_Drug_Histories %>% group_by(drug_class) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 9638606)*100)) 




MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))

length(unique(MIG_Drug_Histories$patient)) # 106269
sum(as.numeric(MIG_Drug_Histories$weight)) # 9638606

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(1,2,51:62)
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% left_join(RIME_Ingredients %>%  select(molecule, generic_name, drug_class))
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, Month, drug_class)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()


data.frame(MIG_Drug_Histories %>% group_by(patient, weight, drug_class) %>% count()  %>% ungroup() %>%
             group_by(drug_class) %>% summarise(mean=weighted.mean(n, as.numeric(weight)))) 




New_BoxJul21


MIG_Drug_Histories <- read.table("MIG Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(disease))
MIG_Drug_Histories <- New_BoxJul21 %>% left_join(MIG_Drug_Histories)
MIG_Drug_Histories <- MIG_Drug_Histories %>% select(-c(New_BoxJul21))

length(unique(MIG_Drug_Histories$patient)) # 106269
sum(as.numeric(MIG_Drug_Histories$weight)) # 9638606

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(1,2,51:62)
MIG_Drug_Histories <- gather(MIG_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Drug_Histories <- MIG_Drug_Histories %>% group_by(patient) %>% arrange(patient)
MIG_Drug_Histories <- separate_rows(MIG_Drug_Histories, Treat, sep = ",", convert=T )
MIG_Drug_Histories <- MIG_Drug_Histories %>% filter(Treat != "-")
names(MIG_Drug_Histories)[4] <- "molecule"

MIG_Drug_Histories <- MIG_Drug_Histories %>% select(patient, weight, Month)
MIG_Drug_Histories <- MIG_Drug_Histories %>% distinct()



MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(patient, month60) %>% mutate(month60 = str_sub(month60, 2L, 2L))
MIG_Box_Histories <- MIG_Box_Histories %>% 
  mutate(month60=ifelse(grepl("I", month60)|grepl("O", month60)|grepl("D", month60)|grepl("d", month60)|grepl("p", month60), "P", "A"))


data.frame(MIG_Drug_Histories %>%
             left_join(MIG_Box_Histories) %>%
             group_by(patient, weight, month60) %>% count()  %>% ungroup() %>%
             group_by(month60) %>%
             summarise(mean=weighted.mean(n, as.numeric(weight)))) 


US_time <- MIG_Drug_Histories %>%
             left_join(MIG_Box_Histories) %>%
             group_by(patient, weight, month60) %>% count()  %>% ungroup() %>%
             group_by(month60, n) %>%
  summarise(pats=sum(as.numeric(weight)))


# JAPAN - - - - - -  -- - - - - - - - 

Pts_segment <- fread("Pts_segment.txt",  colClasses = "character", stringsAsFactors = FALSE)
unique(Pts_segment$pts_segment)
Pts_segment <- Pts_segment %>%  filter(pts_segment == "Mod-high") %>% select(patient, weight)
sum(as.numeric(Pts_segment$weight))



MIG_Box_Histories <- read.table("MIG Box Histories2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories$weight <- as.numeric(MIG_Box_Histories$weight)
sum(MIG_Box_Histories$weight)
MIG_Box_Histories <- MIG_Box_Histories %>% select(-c(disease))
MIG_Box_Histories <- Pts_segment %>% inner_join(MIG_Box_Histories %>% select(-weight))

length(unique(MIG_Box_Histories$patient)) # 38773
sum(as.numeric(MIG_Box_Histories$weight)) # 1537917

MIG_Box_Histories <- MIG_Box_Histories %>% select(1,2,51:62)
MIG_Box_Histories <- gather(MIG_Box_Histories, Month, Treat, month49:month60, factor_key=TRUE)
MIG_Box_Histories <- MIG_Box_Histories %>% group_by(patient) %>% arrange(patient)

unique(MIG_Box_Histories$Treat)

groups <- MIG_Box_Histories %>% mutate(Treat=ifelse(Treat=="I"|Treat=="K"|Treat=="Y"|Treat=="P", "P", "A")) %>%  filter(Month=="month60")
groups <- groups %>% select(patient, weight, Treat)


MIG_Box_Histories <- MIG_Box_Histories %>% filter(Treat != "N" & Treat != "X")
length(unique(MIG_Box_Histories$patient))
MIG_Box_Histories <- MIG_Box_Histories %>% select(patient, weight, Month)
MIG_Box_Histories <- MIG_Box_Histories %>% distinct()
MIG_Box_Histories$Month <- parse_number(as.character(MIG_Box_Histories$Month)) 
MIG_Box_Histories <- MIG_Box_Histories %>% filter(Month>=49)



JP_time <- MIG_Box_Histories %>%
             left_join(groups) %>%
             group_by(patient, weight, Treat) %>% count()  %>% ungroup() %>%
             group_by(Treat, n) %>%
  summarise(pats=sum(as.numeric(weight)))



names(JP_time)[1] <- "group"
JP_time$Country <- "JP"

names(US_time)[1] <- "group"
US_time$Country <- "US"

times_df <- JP_time %>% bind_rows(US_time)

times_df %>% group_by(Country, group) %>% summarise(mean=weighted.mean(n, pats))


times_df %>% filter(group=="P") %>%
  ggplot(aes(n, pats, colour=Country, fill=Country)) +
  geom_col(alpha=0.8) +
  facet_wrap(~Country, scales="free_y") +
  theme_minimal() +
  xlim(0,13) +
  xlab("\n No Months ON Treatment") + ylab("Population \n")  +
  scale_fill_manual(values=c("midnightblue", "firebrick")) +
  scale_colour_manual(values=c("midnightblue", "firebrick")) 



# --------------------

# ------------------
# Physicians US vs Japan ------------

# US 


New_BoxJul21 <- fread("New_BoxJul21.txt")
New_BoxJul21$patient <- as.character(New_BoxJul21$patient)
New_BoxJul21$weight <- as.character(New_BoxJul21$weight)
New_BoxJul21 <- New_BoxJul21 %>% filter(New_BoxJul21 != "Mild")

MIG_Box_Histories <- read.table("MIG Box Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories <- MIG_Box_Histories %>% select(patient, month60) %>% mutate(month60 = str_sub(month60, 2L, 2L))
MIG_Box_Histories <- MIG_Box_Histories %>% 
  mutate(month60=ifelse(grepl("x", month60)|grepl("A", month60)|grepl("a", month60), "A", 
                        ifelse(grepl("p", month60)|grepl("O", month60)|grepl("I", month60), "P", "P+A")))


Physicians_Vanguard_Lookup <- read.csv("Physicians_Vanguard_Lookup.csv", colClasses = "character", stringsAsFactors = FALSE)
removeQuotes <- function(x) gsub("\'", "", x)
Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% mutate_if(is.character, removeQuotes)
names(Physicians_Vanguard_Lookup)[1] <- "specialty"

unique(Physicians_Vanguard_Lookup$Physician)

Physicians_Vanguard_Lookup <- Physicians_Vanguard_Lookup %>% 
  filter(Physician=="PCP"|Physician=="INTERNAL MEDICINE"|Physician=="NEUROLOGIST") %>%
  mutate(Physician=ifelse(Physician=="PCP"|Physician=="INTERNAL MEDICINE", "GP", "NEURO"))


MIG_Doses_BIG <- read.table("MIG Doses.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

MIG_Doses_BIG <- MIG_Doses_BIG %>% select(drug_group, drug_class, pat_id, weight, from_dt, dayssup , specialty)
MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(from_dt = as.Date(from_dt))
MIG_Doses_BIG <- MIG_Doses_BIG %>%filter(from_dt >= "2020-08-01" & from_dt <= "2021-07-31") 
names(MIG_Doses_BIG)[3] <- "patient"

MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(specialty != "")
MIG_Doses_BIG <- MIG_Doses_BIG %>% filter(!is.na(specialty))

MIG_Doses_BIG <- MIG_Doses_BIG  %>% inner_join(Physicians_Vanguard_Lookup) 
unique(MIG_Doses_BIG$drug_group)

MIG_Doses_BIG <- MIG_Doses_BIG %>% 
  mutate(drug_group=ifelse(drug_group=="CGRP Oral"|drug_group=="CGRP Injectable"|drug_group=="Preventative", "P", "A"))

MIG_Doses_BIG <- MIG_Doses_BIG %>% mutate(total=as.numeric(weight)*as.numeric(dayssup)) %>% select(-c(drug_class, weight, dayssup, specialty)) 

MIG_Doses_BIG <- MIG_Doses_BIG %>% inner_join(New_BoxJul21 %>% select(patient))

MIG_Doses_BIG <- MIG_Doses_BIG %>% inner_join(MIG_Box_Histories) 

MIG_Doses_BIG %>% group_by(month60, drug_group, Physician) %>% summarise(total=sum(total))


# JAPAN 

Pts_segment <- fread("Pts_segment.txt",  colClasses = "character", stringsAsFactors = FALSE)
unique(Pts_segment$pts_segment)
Pts_segment <- Pts_segment %>%  filter(pts_segment == "Mod-high") %>% select(patient, weight)
sum(as.numeric(Pts_segment$weight))


MIG_Box_Histories <- read.table("MIG Box Histories2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
MIG_Box_Histories$weight <- as.numeric(MIG_Box_Histories$weight)
sum(MIG_Box_Histories$weight)
MIG_Box_Histories <- MIG_Box_Histories %>% select(-c(disease))
MIG_Box_Histories <- Pts_segment %>% inner_join(MIG_Box_Histories %>% select(-weight))

length(unique(MIG_Box_Histories$patient)) # 38773
sum(as.numeric(MIG_Box_Histories$weight)) # 1537917

MIG_Box_Histories <- MIG_Box_Histories %>% select(1,2,62)
unique(MIG_Box_Histories$month60)

MIG_Box_Histories <- MIG_Box_Histories %>% mutate(month60=ifelse(month60=="P"|month60=="I", "P",
                                            ifelse(month60=="Y"|month60=="K", "A+P","A")))

Doses <- read.table("Doses with Facility Groups.txt", header=T, sep=",", dec=".", quote='"')

Doses <- Doses %>% select(large_class_dpt, drug_group, patient, weight, date_of_prescription, days_supply)

unique(Doses$large_class_dpt)
unique(Doses$drug_group)

Doses <- Doses %>% filter(large_class_dpt=="Internal Medicine"|large_class_dpt=="Neurology")

Doses <- Doses %>% inner_join(MIG_Box_Histories %>% mutate(weight=as.numeric(weight)))

unique(Doses$drug_group)

Doses <- Doses %>% mutate(CGRP=ifelse(drug_group=="CGRP injectable", 1,0)) %>% 
  mutate(drug_group=ifelse(drug_group=="Preventive"|drug_group=="CGRP injectable", "P", "A"))

Doses <- Doses %>% mutate(total=as.numeric(weight)*as.numeric(days_supply)) %>% select(-c(weight, days_supply)) 


Doses %>% mutate(month60=ifelse(grepl("P", month60), "P", month60)) %>%
 group_by(month60, drug_group, large_class_dpt, CGRP ) %>% summarise(total=sum(total))



# ------------------
