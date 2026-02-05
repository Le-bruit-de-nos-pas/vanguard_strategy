# JMPC JAPAN, DRUG PENETRANCE -------------------------------------------------------------
library(data.table)
library(tidyverse)
options(scipen = 999)
library(randomForest)
library(DALEX)

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION 
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% separate(drug_id, c('class', 'molecule'))

# ----
# DRUG PENETRANCE BY DRUG/MOLECULE---------------------------------------------------------------

# ----------
# OBESITY JAPAN DRUG HISTORIES --------------------------------------------------------

OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories_v2.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

# Sum of weights of the treated ones  # 161198  ,   matches the population expectation
OBE_Japan_Drug_Histories_Weights_treated <- OBE_Japan_Drug_Histories %>%select(patient, weight, month60)

OBE_Japan_Drug_Histories_Weights_treated %>% filter(month60 != "-") %>% summarise(sum_weights = sum(as.numeric(weight)))

# sum of weights of the non-treated ones  #    18632973
OBE_Japan_Drug_Histories_Weights_treated %>% filter(month60 == "-") %>% summarise(sum_weights = sum(as.numeric(weight)))
#Total weights  #18632973+161198   #18794171

length(unique(OBE_Japan_Drug_Histories$patient)) # 450355
sum(as.numeric(OBE_Japan_Drug_Histories$weight)) #18564058
length(unique(OBE_Japan_Drug_Histories$disease)) #1


# Select only month60 
OBE_Japan_Drug_Histories_month60 <- OBE_Japan_Drug_Histories %>% select(patient, weight, month60)

#450401-450355  #46 drugs after commas
OBE_Japan_Drug_Histories_month60 <- separate_rows(OBE_Japan_Drug_Histories_month60, month60, sep = ",", convert=T )
names(OBE_Japan_Drug_Histories_month60)[3] <- "molecule"


# Calculate N. of patients on each drug 
# Devide by number of total patients. non-MECE
OBE_Japan_Drug_Histories_month60 %>% group_by(molecule) %>% summarise(n = n()) %>% left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group))



# Calculate sum of weights of patients on each drug
OBE_Japan_Drug_Histories_month60_Drug_labels <- OBE_Japan_Drug_Histories_month60 %>% 
  group_by(molecule) %>% summarise(sum_weights = sum(as.numeric(weight))) %>%
  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group))

OBE_Japan_Drug_Histories_month60 %>% group_by(molecule) %>% 
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group)) %>%
  select(generic_name, drug_group, sum_weights) %>%
  mutate(sum_weights_percent = (sum_weights / 18564058)*100) %>%
  mutate(generic_name = fct_reorder(generic_name, drug_group)) %>%
  filter(!is.na(generic_name)) %>%
  mutate(drug_group=as.factor(drug_group)) %>%
  ggplot(aes(x=generic_name, y=sum_weights_percent, fill=drug_group, label=round(sum_weights_percent, digits = 2))) +
  geom_bar(stat="identity", alpha = 0.8, show.legend = FALSE) +
  ylim(0,3)+
  geom_text(vjust=-1)+
  xlab("\nGeneric Drug Name") + ylab("% Penetrance (non-MECE)\n") +
  ggtitle("Percentage Drug Penetrance amongst Obesity Patients in Japan")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  ggsci::scale_fill_jama()


# --------
# DIABETES JAPAN DRUG HISTORIES ----------------------------------------------------
# ----
# DRUG PENETRANCE DIABETES -----------------------------------------------------------------

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories_Weights_treated <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


# sum of weights of the treated ones
#6692942 matches the population expectation
DIA_Japan_Drug_Histories_Weights_treated %>% 
  filter(month60 != "-") %>%
  summarise(sum_weights = sum(as.numeric(weight)))

# sum of weights of the non-treated ones
#1274543
DIA_Japan_Drug_Histories_Weights_treated %>% filter(month60 == "-") %>%
  summarise(sum_weights = sum(as.numeric(weight)))

# Total  
# 1274543+6692942   
# 7967485

length(unique(DIA_Japan_Drug_Histories$patient)) # 96910

length(unique(DIA_Japan_Drug_Histories$disease)) #1

# select only month60
DIA_Japan_Drug_Histories_month60 <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories_month60 <- separate_rows(DIA_Japan_Drug_Histories_month60, month60, sep = ",", convert=T)

names(DIA_Japan_Drug_Histories_month60)[3] <- "molecule"


# Calculate N. of patients on each drug
# Sevide by number of total patients. non-MECE 
data.frame(DIA_Japan_Drug_Histories_month60 %>% 
             group_by(molecule) %>% 
             summarise(n=n()) %>% 
             mutate(percentage_n = (n/96910)*100) %>%
             left_join(DANU_Japan_Ingredients %>% 
                         select(molecule, generic_name, drug_group)))


# sum of weights instead
DIA_Japan_Drug_Histories_month60_Drug_labels <- 
  data.frame(DIA_Japan_Drug_Histories_month60 %>% 
  group_by(molecule) %>% 
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group)))

data.frame(DIA_Japan_Drug_Histories_month60 %>% 
  group_by(molecule) %>% 
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group)) %>%
  select(generic_name, drug_group, sum_weights) %>%
  mutate(sum_weights_percent = (sum_weights / 17895480)*100) %>%
  mutate(generic_name = fct_reorder(generic_name, drug_group)) %>%
  filter(!is.na(generic_name)) %>%
  filter(sum_weights_percent >0.5) %>%
  mutate(drug_group=as.factor(drug_group)) %>%
  arrange(drug_group))



# sum of weights, percentage of total weights

DIA_Japan_Drug_Histories_month60 %>% 
  group_by(molecule) %>% 
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group)) %>%
  select(generic_name, drug_group, sum_weights) %>%
  mutate(sum_weights_percent = (sum_weights / 17895480)*100) %>%
  mutate(generic_name = fct_reorder(generic_name, drug_group)) %>%
  filter(!is.na(generic_name)) %>%
  filter(sum_weights_percent >0.5) %>%
  mutate(drug_group=as.factor(drug_group)) %>%
  arrange(drug_group) %>%
  ggplot(aes(x=generic_name, y=sum_weights_percent, fill=drug_group, label=round(sum_weights_percent, digits = 2))) +
  geom_bar(stat="identity", alpha = 0.8, show.legend = FALSE) +
  ylim(0,25)+
  geom_text(vjust=-1)+
  xlab("\nGeneric Drug Name") + ylab("% Penetrance (non-MECE)\n") +
  ggtitle("Percentage Drug Penetrance amongst T2 Diabetes Patients in Japan\n(only >0.5% shown)")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  ggsci::scale_fill_jama()








# ----
# DRUG PENETRANCE BY CLASS-----------------------------------
# ----
# DRUG PENETRANCE BY CLASS OBESITY -------------------------------------------------

OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories_v2.txt", 
                                       header = T, sep=",", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only month60
OBE_Japan_Drug_Histories_month60 <-  OBE_Japan_Drug_Histories %>%  select(patient, weight, month60)

#446915-446873
#42 drugs after commas
OBE_Japan_Drug_Histories_month60 <- separate_rows(OBE_Japan_Drug_Histories_month60, month60, sep = ",", convert=T )

names(OBE_Japan_Drug_Histories_month60)[3] <- "molecule"

OBE_Japan_Drug_Histories_month60_classes <- 
  OBE_Japan_Drug_Histories_month60 %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))

OBE_Japan_Drug_Histories_month60_classes <- 
  OBE_Japan_Drug_Histories_month60_classes %>% 
  select(patient, weight, drug_group)

#Total weight 
#18564058 (from above)

#remove duplicates
OBE_Japan_Drug_Histories_month60_classes <- OBE_Japan_Drug_Histories_month60_classes %>% distinct()

 OBE_Japan_Drug_Histories_month60_classes %>% filter(grepl("GLP",drug_group)) %>%
    group_by(drug_group) %>% summarise(sum_weights = sum(as.numeric(weight)))



# Weight per class, percentage of total weights, non-MECE
OBE_Japan_Drug_Histories_month60_classes %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  mutate(sum_weights_percent = (sum_weights / 18564058)*100) %>%
  filter(!is.na(drug_group)) %>%
  ggplot(aes(x=reorder(drug_group, -sum_weights_percent), y=sum_weights_percent, label=round(sum_weights_percent, digits = 2))) +
  geom_bar(stat="identity", alpha = 0.7, fill = "firebrick", show.legend = FALSE) +
  ylim(0,3)+
  geom_text(vjust=-1)+
  xlab("\n Drug Class") + ylab("% Penetrance (non-MECE)\n") +
  ggtitle("Percentage Drug Class Penetrance\nObesity Patients in Japan")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))







# ----
# DRUG PENETRANCE BY CLASS DIABETES -----------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only month60 
DIA_Japan_Drug_Histories_month60 <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories_month60 <- 
  separate_rows(DIA_Japan_Drug_Histories_month60, month60, sep = ",", convert=T )

names(DIA_Japan_Drug_Histories_month60)[3] <- "molecule"


DIA_Japan_Drug_Histories_month60_classes <- 
  DIA_Japan_Drug_Histories_month60 %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group, drug_class))


DIA_Japan_Drug_Histories_month60_classes <- 
  DIA_Japan_Drug_Histories_month60_classes %>% 
  select(patient, weight, drug_group, drug_class)



# remove duplicates
DIA_Japan_Drug_Histories_month60_classes <- 
  DIA_Japan_Drug_Histories_month60_classes %>% distinct()





DIA_Japan_Drug_Histories_month60_classes %>%
  group_by(drug_group, drug_class) %>%
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  mutate(sum_weights_percent = (sum_weights / 17895480)*100) %>%
  filter(!is.na(drug_group)) %>%
  ggplot(aes(x=reorder(drug_group, -sum_weights_percent), y=sum_weights_percent, label=round(sum_weights_percent, digits = 2))) +
  geom_bar(stat="identity", alpha = 0.7, fill = "firebrick", show.legend = FALSE) +
  ylim(0,30)+
  geom_text(vjust=-1)+
  xlab("\n Drug Class") + ylab("% Penetrance (non-MECE)\n") +
  ggtitle("Percentage Drug Class Penetrance T2 Diabetes Patients in Japan")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))




# ----
# How many have never been treated? -------------------------------------------------

# Import files again 

OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


# select only columns with the months / drugs
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(4:63)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(4:63)

# convert lapsed (-) to zero, and non-lapsed (!= "-") to one 
# sum across rows, to see hoe many remain zero "0"
OBE_Japan_Drug_Histories[OBE_Japan_Drug_Histories != "-"] <- 1  # on drug 
OBE_Japan_Drug_Histories[OBE_Japan_Drug_Histories == "-"] <- 0  # no drug

OBE_Japan_Drug_Histories[] <-  lapply(OBE_Japan_Drug_Histories, as.numeric)

OBE_Japan_Drug_Histories$SUM <- rowSums(OBE_Japan_Drug_Histories)

OBE_Japan_Drug_Histories %>% group_by(SUM) %>%summarise(n=n())


#438253  
#98% never took anything

# convert lapsed (-) to zero, and non-lapsed (!= "-") to one 
# sum across rows, to see hoe many remain zero "0" 
DIA_Japan_Drug_Histories[DIA_Japan_Drug_Histories != "-"] <- 1  # on drug 
DIA_Japan_Drug_Histories[DIA_Japan_Drug_Histories == "-"] <- 0  # no drug

DIA_Japan_Drug_Histories[] <- 
  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories$SUM <- 
  rowSums(DIA_Japan_Drug_Histories)

DIA_Japan_Drug_Histories %>% 
  group_by(SUM) %>%
  summarise(n=n())

# ----
# Obesity --------------------------------------

OBE_Japan_Drug_Histories <- 
  OBE_Japan_Drug_Histories %>% 
  mutate(SUM = ifelse(SUM == 0, "Treat_Naive", "Treat_Experienced"))

OBE_Japan_Drug_Histories_Treat_experience <- OBE_Japan_Drug_Histories
rm(OBE_Japan_Drug_Histories)

# Import files again
OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories_v2.txt", 
                                       header = T, sep=",", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# the original one again
OBE_Treatment_Experience_Label <- OBE_Japan_Drug_Histories %>%
  select(patient, weight) %>%
  bind_cols(OBE_Japan_Drug_Histories_Treat_experience %>% select(SUM))


# Diabetes
DIA_Japan_Drug_Histories <-
  DIA_Japan_Drug_Histories %>% 
  mutate(SUM = ifelse(SUM == 0, "Treat_Naive", "Treat_Experienced"))

DIA_Japan_Drug_Histories_Treat_experience <- DIA_Japan_Drug_Histories
rm(DIA_Japan_Drug_Histories)

# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# the original one again
DIA_Treatment_Experience_Label <- DIA_Japan_Drug_Histories %>%
  select(patient, weight) %>%
  bind_cols(DIA_Japan_Drug_Histories_Treat_experience %>% select(SUM))



OBE_Treatment_Experience_Label
write.csv(OBE_Treatment_Experience_Label, "OBE_Treatment_Experience_Label.csv")

DIA_Treatment_Experience_Label 
write.csv(DIA_Treatment_Experience_Label, "DIA_Treatment_Experience_Label.csv")



OBE_Treatment_Experience_Label %>%
  group_by(SUM) %>%
  summarise(N= sum(as.numeric(weight)))







 
# ----
# How many have been treated at any point of the past 12 months? --------------------------------------------

# Repeat the same above, but for only the last year


OBE_Japan_Drug_Histories <- 
  OBE_Japan_Drug_Histories %>% 
  select(49:60)

OBE_Japan_Drug_Histories$SUM <- 
  rowSums(OBE_Japan_Drug_Histories)

OBE_Japan_Drug_Histories %>% 
  group_by(SUM) %>%
  summarise(n=n())


# LONG is the original file, imported again because we need the weights

OBE_Japan_Drug_Histories_LONG <- read.table("OBE Japan Drug Histories_v2.txt", 
                                       header = T, sep=",", 
                                       colClasses = "character", stringsAsFactors = FALSE)


OBE_Japan_Drug_Histories_LONG <- 
  OBE_Japan_Drug_Histories_LONG %>% 
  select(patient, weight) %>%
  bind_cols(OBE_Japan_Drug_Histories)


OBE_Japan_Drug_Histories_LONG %>% 
  group_by(SUM) %>%
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  filter(SUM != 0) %>%
  mutate(tot_sum = sum(sum_weights))


# repeat the same above, but for only the last year
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(49:60)

DIA_Japan_Drug_Histories$SUM <- 
  rowSums(DIA_Japan_Drug_Histories)

DIA_Japan_Drug_Histories %>% 
  group_by(SUM) %>%
  summarise(n=n())



DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- 
  DIA_Japan_Drug_Histories_LONG %>% 
  select(patient, weight) %>%
  bind_cols(DIA_Japan_Drug_Histories)


DIA_Japan_Drug_Histories_LONG %>% 
  group_by(SUM) %>%
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  filter(SUM != 0) %>%
  mutate(tot_sum = sum(sum_weights))





# ----
# MUTUALLY EXCLUSIVE  --------------------------------------------------------------------
# ----
# PENETRANCE PER CATEGORY -------------------------------------------------------------


# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION 
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS
DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))



DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)



# select only month60
DIA_Japan_Drug_Histories_month60 <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)



DIA_Japan_Drug_Histories_month60 <- 
  separate_rows(DIA_Japan_Drug_Histories_month60, month60, sep = ",", convert=T )

names(DIA_Japan_Drug_Histories_month60)[3] <- "molecule"


# add labels from the dictionary

DIA_Japan_Drug_Histories_month60_classes <- 
  DIA_Japan_Drug_Histories_month60 %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))


# convert to numeric so that we can arrange it

DIA_Japan_Drug_Histories_month60_classes$molecule<- 
  as.numeric(DIA_Japan_Drug_Histories_month60_classes$molecule)

# sort it

DIA_Japan_Drug_Histories_month60_classes <- 
  DIA_Japan_Drug_Histories_month60_classes %>%
  arrange(patient, molecule)


# check for duplicates
# none at this poiint because we have the  molecules there

DIA_Japan_Drug_Histories_month60_classes %>% distinct()

# pick only pats, weights and groups

DIA_Japan_Drug_Histories_month60_classes <- 
  DIA_Japan_Drug_Histories_month60_classes %>% 
  select(patient, weight, drug_group)



#Total weight 
#17895480 (from above)

# replace NAs
DIA_Japan_Drug_Histories_month60_classes[is.na(DIA_Japan_Drug_Histories_month60_classes)] <- "Lapsed"


# remove duplicates
# i.e. each aptient contribute sonly once per drug group now

DIA_Japan_Drug_Histories_month60_classes <- 
  DIA_Japan_Drug_Histories_month60_classes %>% 
  distinct()


# check which drug_groups we have, just in case
unique(DIA_Japan_Drug_Histories_month60_classes$drug_group)


# replace drug_group codes by numbers

RANKED_DIA_Japan_Drug_Histories_month60_classes <- 
  DIA_Japan_Drug_Histories_month60_classes %>%
  mutate(drug_group = 
           ifelse(drug_group == "Lapsed", "1", 
                  ifelse(drug_group == "Biguanide", "2", 
                         ifelse(drug_group == "Antiobesity", "3", 
                                ifelse(drug_group == "Antidiabetic", "4",
                                       ifelse(drug_group == "DPP4", "5", 
                                              ifelse(drug_group == "SGLT2", "6",
                                                     ifelse(drug_group == "GLP1 Oral", "7",
                                                                   ifelse(drug_group == "GLP1 Injectable", "8",
                                                                          ifelse(drug_group == "Insulin", "9",
                                                                                 ifelse(drug_group == "Surgery", "10",
                                                                                        drug_group)))))))))))





# convert drug_group to numeric

RANKED_DIA_Japan_Drug_Histories_month60_classes$drug_group <-
  as.numeric(RANKED_DIA_Japan_Drug_Histories_month60_classes$drug_group)


# picked the highest number per patients 
#(surgery -> insulin -> GPL inject, etc. as the rank)

RANKED_DIA_Japan_Drug_Histories_month60_classes <- 
  RANKED_DIA_Japan_Drug_Histories_month60_classes %>% 
  group_by(patient) %>%
  summarize(across(everything(), max))



# back to labels
#note how we have 242002 patients again! Perfect

RANKED_DIA_Japan_Drug_Histories_month60_classes <- 
  RANKED_DIA_Japan_Drug_Histories_month60_classes %>%
  mutate(drug_group = 
           ifelse(drug_group == "1", "Lapsed", 
                  ifelse(drug_group == "2", "Biguanide", 
                         ifelse(drug_group == "3", "Antiobesity", 
                                ifelse(drug_group == "4", "Antidiabetic",
                                       ifelse(drug_group == "5", "DPP4", 
                                              ifelse(drug_group == "6", "SGLT2",
                                                     ifelse(drug_group == "7", "GLP1 Oral",
                                                            ifelse(drug_group == "8", "GLP1 Injectable",
                                                                   ifelse(drug_group == "9", "Insulin",
                                                                          ifelse(drug_group == "10", "Surgery",
                                                                                 drug_group)))))))))))




# weights to numeric to sum them up
RANKED_DIA_Japan_Drug_Histories_month60_classes$weight <-
  as.numeric(RANKED_DIA_Japan_Drug_Histories_month60_classes$weight)



# sum of weights as % of total weights for diabetes
RANKED_DIA_Japan_Drug_Histories_month60_classes %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(weight)) %>%
  mutate(sum_weight_percentage = (sum_weights / 17895480)*100)





# ----
# TREATMENT EXPERIENCED ONLY -----------------------------------------------
# Repeat DRUG PENETRANCE, REMOVING THOSE WHO HAVE NEVER BEEN TREATED

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS
DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))




# The files stating wether the patient has ever been treated or not
OBE_Treatment_Experience_label <- read.table("OBE_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)


DIA_Treatment_Experience_label <- read.table("DIA_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)


# The original files with drugs over months 
OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)





# join them
OBE_Treatment_Experience_label <- 
  OBE_Treatment_Experience_label %>%
  left_join(OBE_Japan_Drug_Histories, by = "patient")

DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>%
  left_join(DIA_Japan_Drug_Histories, by = "patient")

# remove originals
rm(DIA_Japan_Drug_Histories, OBE_Japan_Drug_Histories)



# ----
# DRUG PENETRANCE --------------------------------------------------------
# ----
# DRUG PENETRANCE OBESITY -------------------------------------------------------

OBE_Treatment_Experience_label <- 
  OBE_Treatment_Experience_label %>% 
  select(patient, weight.x, month60, SUM)


OBE_Treatment_Experience_label <- 
  OBE_Treatment_Experience_label %>%
  filter(SUM == "Treat_Experienced") 

length(unique(OBE_Treatment_Experience_label$patient)) # 8620
sum(as.numeric(OBE_Treatment_Experience_label$weight.x)) #441120.1


OBE_Treatment_Experience_label <- 
  separate_rows(OBE_Treatment_Experience_label, month60, sep = ",", convert=T )

names(OBE_Treatment_Experience_label)[3] <- "molecule"





# Calculate N. of patients on each drug, devide by number of total patients. non-MECE

OBE_Treatment_Experience_label %>% 
  group_by(molecule) %>% 
  summarise(n = n()) %>%
  mutate(percentage = (n/8620)*100) %>%
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))


# Calculate sum of weights of patients on each drug

OBE_Treatment_Experience_label %>% 
  group_by(molecule) %>% 
  summarise(sum_weights = sum(as.numeric(weight.x))) %>%
  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group)) %>%
  select(generic_name, drug_group, sum_weights) %>%
  mutate(sum_weights_percent = (sum_weights / 441120.1)*100)











# ----
# DRUG PENETRANCE DIABETES ------------------------------------------------------------------------

DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>% 
  select(patient, weight.x, month60, SUM)


DIA_Treatment_Experience_label <- DIA_Treatment_Experience_label %>%
  filter(SUM == "Treat_Experienced") 


length(unique(DIA_Treatment_Experience_label$patient)) # 100392
sum(as.numeric(DIA_Treatment_Experience_label$weight.x)) #8197598


DIA_Treatment_Experience_label <- 
  separate_rows(DIA_Treatment_Experience_label, month60, sep = ",", convert=T )

names(DIA_Treatment_Experience_label)[3] <- "molecule"


# Calculate N. of patients on each drug, devide by number of total patients. non-MECE

data.frame(DIA_Treatment_Experience_label %>% 
             group_by(molecule) %>% 
             summarise(n = n()) %>%
             mutate(percentage = (n/100392)*100) %>%
             left_join(DANU_Japan_Ingredients %>% 
                         select(molecule, generic_name, drug_group)))


# Calculate sum of weights of patients on each drug

data.frame(DIA_Treatment_Experience_label %>% 
             group_by(molecule) %>% 
             summarise(sum_weights = sum(as.numeric(weight.x))) %>%
             left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group)) %>%
             select(generic_name, drug_group, sum_weights) %>%
             mutate(sum_weights_percent = (sum_weights / 8197598)*100))




# ----
# PER CLASS -----------------------------------------------------------------------------------
# ----
# PER CLASS OBESITY -----------------------------------------------------------------------------------

OBE_Treatment_Experience_label <- 
  OBE_Treatment_Experience_label %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))

OBE_Treatment_Experience_label <- 
  OBE_Treatment_Experience_label %>% 
  select(patient, weight.x, drug_group)

OBE_Treatment_Experience_label <- 
  OBE_Treatment_Experience_label %>% 
  distinct()

OBE_Treatment_Experience_label %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(as.numeric(weight.x))) %>%
  mutate(sum_weights_percent = (sum_weights / 441120.1)*100) %>%
  filter(!is.na(drug_group))





# ----
# PER CLASS DIABETES -----------------------------------------------------------------------------------

DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))

DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>% 
  select(patient, weight.x, drug_group)

DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>% 
  distinct()

DIA_Treatment_Experience_label %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(as.numeric(weight.x))) %>%
  mutate(sum_weights_percent = (sum_weights / 8197598)*100) %>%
  filter(!is.na(drug_group))





# ----
# MUTUALLY EXCLUSIVE -----------------------------------------------------------------------------------------
# ----
# PENETRANCE PER CATEGORY -----------------------------------------------------------------------------------------


# replace NAs
DIA_Treatment_Experience_label[is.na(DIA_Treatment_Experience_label)] <- "Lapsed"


# remove duplicates, i.e. each aptient contribute sonly once per drug group now
DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>% 
  distinct()


# check which drug_groups we have, just in case
unique(DIA_Treatment_Experience_label$drug_group)



# replace drug_group codes by numbers
DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>%
  mutate(drug_group = 
           ifelse(drug_group == "Lapsed", "1", 
                  ifelse(drug_group == "Biguanide", "2", 
                         ifelse(drug_group == "Antiobesity", "3", 
                                ifelse(drug_group == "Antidiabetic", "4",
                                       ifelse(drug_group == "DPP4", "5", 
                                              ifelse(drug_group == "SGLT2", "6",
                                                     ifelse(drug_group == "GLP1 Oral", "7",
                                                            ifelse(drug_group == "GLP1 Injectable", "8",
                                                                   ifelse(drug_group == "Insulin", "9",
                                                                          ifelse(drug_group == "Surgery", "10",
                                                                                 drug_group)))))))))))





# convert drug_group to numeric
DIA_Treatment_Experience_label$drug_group <-
  as.numeric(DIA_Treatment_Experience_label$drug_group)


DIA_Treatment_Experience_label <- DIA_Treatment_Experience_label %>%
  arrange(patient, drug_group)


# picked the highest number per patients (surgery -> insulin -> GPL inject, etc. as the rank)
DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>% 
  group_by(patient) %>%
  summarize(across(everything(), max))





# back to labels, note how we have 100392 patients again. Perfect

DIA_Treatment_Experience_label <- 
  DIA_Treatment_Experience_label %>%
  mutate(drug_group = 
           ifelse(drug_group == "1", "Lapsed", 
                  ifelse(drug_group == "2", "Biguanide", 
                         ifelse(drug_group == "3", "Antiobesity", 
                                ifelse(drug_group == "4", "Antidiabetic",
                                       ifelse(drug_group == "5", "DPP4", 
                                              ifelse(drug_group == "6", "SGLT2",
                                                     ifelse(drug_group == "7", "GLP1 Oral",
                                                            ifelse(drug_group == "8", "GLP1 Injectable",
                                                                   ifelse(drug_group == "9", "Insulin",
                                                                          ifelse(drug_group == "10", "Surgery",
                                                                                 drug_group)))))))))))




# weights to numeric to sum them up
DIA_Treatment_Experience_label$weight.x <-
  as.numeric(DIA_Treatment_Experience_label$weight.x)



# sum of weights as % of total weights for diabetes
DIA_Treatment_Experience_label %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(weight.x)) %>%
  mutate(sum_weight_percentage = (sum_weights / 8197598)*100)









# ----
# Lapsed periods duration ------------------------------------------------------------------
# ----
# Calculate the duration of lapsed periods for each patient ------------------------------
# ----
# How many have never been treated?---------------------------------------------------------


# original files with patient id, weights, and all that
# Import files again
OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)



# convert drugs to zero or one (no drug vs drug)
# select only columns with the months / drugs
OBE_Japan_Drug_Histories <- 
  OBE_Japan_Drug_Histories %>% 
  select(4:63)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(4:63)


# convert lapsed (-) to zero, and non-lapsed (!= "-") to one
# convert to numeric everything
# sum across rows, to see hoe many remain zero "0"
OBE_Japan_Drug_Histories[OBE_Japan_Drug_Histories != "-"] <- 1  # on drug 
OBE_Japan_Drug_Histories[OBE_Japan_Drug_Histories == "-"] <- 0  # no drug

OBE_Japan_Drug_Histories[] <- 
  lapply(OBE_Japan_Drug_Histories, as.numeric)


# convert lapsed (-) to zero, and non-lapsed (!= "-") to one
# convert to numeric everything
# sum across rows, to see hoe many remain zero "0"
DIA_Japan_Drug_Histories[DIA_Japan_Drug_Histories != "-"] <- 1  # on drug 
DIA_Japan_Drug_Histories[DIA_Japan_Drug_Histories == "-"] <- 0  # no drug

DIA_Japan_Drug_Histories[] <- 
  lapply(DIA_Japan_Drug_Histories,as.numeric)




# The files stating wether the patient has ever been treated or not
# so that i can filter by treatment-experienced patients


OBE_Treatment_Experience_label <- read.table("OBE_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Drug_Histories <- OBE_Treatment_Experience_label %>%
  bind_cols(OBE_Japan_Drug_Histories)


DIA_Treatment_Experience_label <- read.table("DIA_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- DIA_Treatment_Experience_label %>%
  bind_cols(DIA_Japan_Drug_Histories)





# pick only treatment experienced
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%
  filter(SUM == "Treat_Experienced")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(SUM == "Treat_Experienced")


# convert to long format, where we have the month column and wether the patient was on drug or not 1/0
# sort by patient, month combination

OBE_Japan_Drug_Histories <- gather(OBE_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment


OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of no treatment
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%
  filter(Treat == 0)


# count (how many months) in each of this lapsed periods!
Lapsed_Periods_OBE <- OBE_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Lapsed_Periods_OBE)[3] <- "Duration"

data.frame(Lapsed_Periods_OBE %>%
  group_by(Duration) %>%
  summarise(n = n()))



# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})


# filter for the periods of no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 0)


# count (how many months) in each of this lapsed periods!
Lapsed_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Lapsed_Periods_DIA)[3] <- "Duration"

data.frame(Lapsed_Periods_DIA %>%
  group_by(Duration) %>%
  summarise(n = n()))




 
### Check for patients that do not start as lapsed
# measure lapsed period durations 
# split between those that are not followed by therapy and those who are 

### Check for patients that do not start as lapsed 
### split over those who remain lapsed, and those who re-enter therapy 
# original files with patient id, weights, and all that
# Import files again

OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)


# convert drugs to zero or one (no drug vs drug)
# select only columns with the months / drugs
OBE_Japan_Drug_Histories <- 
  OBE_Japan_Drug_Histories %>% 
  select(4:63)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(4:63)


# convert lapsed (-) to zero, and non-lapsed (!= "-") to one
# convert to numeric everything
# sum across rows, to see hoe many remain zero "0"
OBE_Japan_Drug_Histories[OBE_Japan_Drug_Histories != "-"] <- 1  # on drug 
OBE_Japan_Drug_Histories[OBE_Japan_Drug_Histories == "-"] <- 0  # no drug

OBE_Japan_Drug_Histories[] <- 
  lapply(OBE_Japan_Drug_Histories, as.numeric)


# convert lapsed (-) to zero, and non-lapsed (!= "-") to one
# convert to numeric everything
# sum across rows, to see hoe many remain zero "0"
DIA_Japan_Drug_Histories[DIA_Japan_Drug_Histories != "-"] <- 1  # on drug 
DIA_Japan_Drug_Histories[DIA_Japan_Drug_Histories == "-"] <- 0  # no drug

DIA_Japan_Drug_Histories[] <- 
  lapply(DIA_Japan_Drug_Histories,as.numeric)




# The files stating wether the patient has ever been treated or not 
# so that i can filter by treatment-experienced patients

OBE_Treatment_Experience_label <- read.table("OBE_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Drug_Histories <- OBE_Treatment_Experience_label %>%
  bind_cols(OBE_Japan_Drug_Histories)


DIA_Treatment_Experience_label <- read.table("DIA_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- DIA_Treatment_Experience_label %>%
  bind_cols(DIA_Japan_Drug_Histories)


# pick only treatment experienced
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%
  filter(SUM == "Treat_Experienced")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(SUM == "Treat_Experienced")



# remove those who start off as lapsed!
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%
  filter(month1 != 0)

# remove those who start off as lapsed!
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(month1 != 0)


# convert to long format, where we have the month column and wether the patient was on drug or not 1/0
# sort by patient, month combination

OBE_Japan_Drug_Histories <- gather(OBE_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)



# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})


# filter for the periods of no treatment
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%
  filter(Treat == 0)

# we have all of those that do not start as lapsed
# so now we have to pick those who entered lapsed and stay lapsed until month60
OBE_NO_return_treat <- OBE_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(Month == "month60")

OBE_NO_return_treat <- OBE_NO_return_treat %>% select(patient, grp)
OBE_NO_return_treat <- OBE_NO_return_treat %>% mutate(return = "NO_RETURN_TREAT")

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%
  left_join(OBE_NO_return_treat, by = c("patient"="patient", "grp"="grp"))


OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(patient, Treat, grp, return)





# for each patient, count how long it remains on the same line 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 0)


# we have all of those that do not start as lapsed
# so now we have to pick those who entered lapsed and stay lapsed until month60

DIA_NO_return_treat <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(Month == "month60")

DIA_NO_return_treat <- DIA_NO_return_treat %>% select(patient, grp)
DIA_NO_return_treat <- DIA_NO_return_treat %>% mutate(return = "NO_RETURN_TREAT")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  left_join(DIA_NO_return_treat, by = c("patient"="patient", "grp"="grp"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, Treat, grp, return)


# count (how many months) in each of this lapsed periods!
#split periods that lead to no return

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  mutate(return = ifelse(is.na(return), "RETURN_TREAT", return))

DIA_Periods_of_no_return <- DIA_Japan_Drug_Histories %>%
  filter(return == "NO_RETURN_TREAT")

DIA_Periods_of_return <- DIA_Japan_Drug_Histories %>%
  filter(return == "RETURN_TREAT")


Lapsed_Periods_DIA_no_return <- DIA_Periods_of_no_return %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Lapsed_Periods_DIA_no_return)[3] <- "Duration"

data.frame(Lapsed_Periods_DIA_no_return %>%
             group_by(Duration) %>%
             summarise(n = n()))



Lapsed_Periods_DIA_return <- DIA_Periods_of_return %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Lapsed_Periods_DIA_return)[3] <- "Duration"

data.frame(Lapsed_Periods_DIA_return %>%
             group_by(Duration) %>%
             summarise(n = n()))




# ----
# Check How Many Patients were on each of those 3 antiobesity drugs  ---------------------------------------------------------------

OBE_Japan_Drug_Stocks <- read.table("OBE Japan Drug Stocks.txt", 
                                             header = T, sep="\t", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Stocks <- read.table("DIA Japan Drug Stocks.txt", 
                                    header = T, sep="\t", quote="", 
                                    colClasses = "character", stringsAsFactors = FALSE)

# 
# Total weights
# 441120.1
# 8197598

# IN OBESITY 
# 0 in Epalrestat in Obesity

# Boiogito
data.frame(OBE_Japan_Drug_Stocks %>% filter(grepl("Boiogito",generic)) %>%
             mutate(period = as.numeric(period)) %>%
             group_by(period) %>%
             summarise(sum_weights = sum(as.numeric(pats)), sum_n_pats = sum(as.numeric(n))))





# Lisdexamfetamine
data.frame(OBE_Japan_Drug_Stocks %>% filter(grepl("Lisdexamfetamine",generic)) %>%
             mutate(period = as.numeric(period)) %>%
             group_by(period) %>%
             summarise(sum_weights = sum(as.numeric(pats)), sum_n_pats = sum(as.numeric(n))))



#   IN DIABETES 

# Epalrestat
data.frame(DIA_Japan_Drug_Stocks %>% filter(grepl("Epalrestat",generic)) %>%
             mutate(period = as.numeric(period)) %>%
             group_by(period) %>%
             summarise(sum_weights = sum(as.numeric(pats)), sum_n_pats = sum(as.numeric(n))))




# Boiogito
data.frame(DIA_Japan_Drug_Stocks %>% filter(grepl("Boiogito",generic)) %>%
             mutate(period = as.numeric(period)) %>%
             group_by(period) %>%
             summarise(sum_weights = sum(as.numeric(pats)), sum_n_pats = sum(as.numeric(n))))







# ----
# DRUG PENETRANCE BY CLASS MARKs file MECE -------------------------

# With MARK-s file,  MECE
# DIABETES       OVER TIME 
DIA_Japan_Drug_Stocks <- read.table("DIA Japan Drug Stocks.txt", 
                                    header = T, sep="\t", quote="", 
                                    colClasses = "character", stringsAsFactors = FALSE)

# Total weights diabetes = 8197598

DIA_nonMECE_all_months <-
  DIA_Japan_Drug_Stocks %>% 
  select(period, group, pats) %>%
  group_by(period, group) %>%
  summarise(sumN = sum(as.numeric(pats)))


DIA_nonMECE_all_months <- 
  DIA_nonMECE_all_months %>%
  mutate(percentwise = (sumN / 8197598)*100)


DIA_nonMECE_all_months <- 
  DIA_nonMECE_all_months %>% select(-sumN)


DIA_nonMECE_all_months <- 
  DIA_nonMECE_all_months %>% mutate(period = as.numeric(period))

DIA_nonMECE_all_months <- DIA_nonMECE_all_months %>% 
  spread(key = group, value = percentwise)

write.csv(DIA_nonMECE_all_months, "DIA_nonMECE_all_months.csv")




# ----
# Non MECE -----------------------------------------------------
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)


# The files stating wether the patient has ever been treated or not
DIA_Treatment_Experience_label <- read.table("DIA_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)


DIA_Treatment_Experience_label <- DIA_Treatment_Experience_label %>%
  left_join(DIA_Japan_Drug_Histories, by = "patient")


# remove originals
rm(DIA_Japan_Drug_Histories)


DIA_Treatment_Experience_label <- DIA_Treatment_Experience_label %>%
  filter(SUM == "Treat_Experienced") 

length(unique(DIA_Treatment_Experience_label$patient)) # 100392
sum(as.numeric(DIA_Treatment_Experience_label$weight.x)) #8197598


# repeat the following for each month
# ->

DIA_Treatment_Experience_label

DIA_Treatment_Experience_label_month1 <- 
  DIA_Treatment_Experience_label %>% 
  select(patient, weight.x, month1)


DIA_Treatment_Experience_label_month1 <- 
  separate_rows(DIA_Treatment_Experience_label_month1, month1, sep = ",", convert=T )

names(DIA_Treatment_Experience_label_month1)[3] <- "molecule"

DIA_Treatment_Experience_label_month1 <- 
  DIA_Treatment_Experience_label_month1 %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))

DIA_Treatment_Experience_label_month1 <- 
  DIA_Treatment_Experience_label_month1 %>% 
  select(patient, weight.x, drug_group)

DIA_Treatment_Experience_label_month1 <- 
  DIA_Treatment_Experience_label_month1 %>% 
  distinct()

data.frame(DIA_Treatment_Experience_label_month1 %>%
             group_by(drug_group) %>%
             summarise(sum_weights = sum(as.numeric(weight.x))) %>%
             mutate(sum_weights_percent = (sum_weights / 8197598)*100) %>%
             filter(!is.na(drug_group)))








# ----
# DRUG PENETRANCE DIABETES per AGE Bucket --------------------------------------
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)

DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% separate(drug_id, c('class', 'molecule'))

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

sum(as.numeric(DIA_Japan_Drug_Histories$weight)) #7967485

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, month60)

DIA_Japan_Drug_Histories <- separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

names(DIA_Japan_Drug_Histories)[3] <- "molecule"

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, drug_group)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% distinct()

data.frame(DIA_Japan_Drug_Histories %>%
             group_by(drug_group) %>%
             summarise(sum_weights = sum(as.numeric(weight))) %>%
             mutate(sum_weights_percent = (sum_weights / 7967485)*100) %>%
             filter(!is.na(drug_group)))




DIA_Japan_Demographics <- read.table("DIA Japan Demographics.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Demographics <- DIA_Japan_Demographics %>% select(patid, weight, age)
names(DIA_Japan_Demographics)[1] <- "patient"
DIA_Japan_Drug_Histories<- DIA_Japan_Drug_Histories %>% left_join(DIA_Japan_Demographics)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(age = as.numeric(age))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(age_group = ifelse(age<45,"< 45", 
                                                                                   ifelse(age>=45 & age<55, "45 to 55", 
                                                                                          ifelse(age>=55 & age<65, "55 to 65",
                                                                                                 ifelse(age>=65 & age<75, "65 to 75", "> 75")))))



data.frame(DIA_Japan_Drug_Histories %>% group_by(drug_group, age_group) %>% 
             summarise(sum_weights = sum(as.numeric(weight))) %>% filter(!is.na(drug_group)))

# ----
# DEMOGRAPHICS TREAT-EXP  vs TREAT-NAIVE patients ----------------------------------------

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# File with age and gender for everyone
DIA_Japan_Demographics <- read.table("DIA Japan Demographics.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)


# The file stating wether the patient has ever been treated or not
DIA_Treatment_Experience_label <- read.table("DIA_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Demographics <- DIA_Japan_Demographics %>% select(patid, weight, gender, age)
names(DIA_Japan_Demographics)[1] <- "patient"

DIA_Japan_Demographics <- DIA_Treatment_Experience_label %>% left_join(DIA_Japan_Demographics)

DIA_Japan_Demographics %>% 
  group_by(SUM, gender) %>%
  summarise(n= sum(as.numeric(weight)))




DIA_Japan_Demographics <- DIA_Japan_Demographics %>% 
  mutate(weight = as.numeric(weight)) %>%
  mutate(gender = as.factor(gender))

DIA_Japan_Demographics <- setDT(expandRows(DIA_Japan_Demographics, "weight"))[]

cols <- c("deeppink4", "darkslategrey")

DIA_Japan_Demographics %>%
  group_by(SUM, age) %>%
  summarise(n = n()) %>%
  mutate(age = as.numeric(age)) %>%
  ggplot(aes(x=age, y =n, fill=SUM))+
  xlab("\nAge")+
  ylab("Projected # of patients\n")+
  xlim(0,100)+
  geom_density(stat="identity", show.legend = T, alpha=0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values = cols)


DIA_Japan_Demographics %>%
  group_by(SUM) %>%
  summarise(n = mean(as.numeric(age)))

DIA_Japan_Demographics %>%
  group_by(SUM) %>%
  summarise(n = median(as.numeric(age)))








# ----
# Check HbA1C evolution over time ------------------------------------------------------
# Treat naive vs treat exp 

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)


# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", 
                                     header = T, sep="\t", 
                                     colClasses = "character", stringsAsFactors = FALSE)

names(HbA1cHist)[1] <- "patient"
HbA1cHist <- HbA1cHist %>% select(-c(weight))

# The file stating wether the patient has ever been treated or not
DIA_Treatment_Experience_label <- read.table("DIA_Treatment_Experience_label.csv", 
                                             header = T, sep=",", quote="", 
                                             colClasses = "character", stringsAsFactors = FALSE)


DIA_Treatment_Experience_label <- DIA_Treatment_Experience_label %>% left_join(HbA1cHist)


DIA_Treatment_Experience_label <- gather(DIA_Treatment_Experience_label, Month, HbA1c, X1:X60, factor_key=TRUE)

DIA_Treatment_Experience_label
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X1", "1")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X2", "2")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X3", "3")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X4", "4")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X5", "5")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X6", "6")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X7", "7")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X8", "8")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X9", "9")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X10", "10")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X11", "11")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X12", "12")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X13", "13")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X14", "14")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X15", "15")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X16", "16")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X17", "17")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X18", "18")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X19", "19")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X20", "20")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X21", "21")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X22", "22")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X23", "23")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X24", "24")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X25", "25")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X26", "26")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X27", "27")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X28", "28")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X29", "29")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X30", "30")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X31", "31")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X32", "32")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X33", "33")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X34", "34")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X35", "35")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X36", "36")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X37", "37")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X38", "38")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X39", "39")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X40", "40")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X41", "41")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X42", "42")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X43", "43")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X44", "44")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X45", "45")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X46", "46")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X47", "47")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X48", "48")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X49", "49")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X50", "50")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X51", "51")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X52", "52")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X53", "53")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X54", "54")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X55", "55")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X56", "56")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X57", "57")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X58", "58")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X59", "59")
DIA_Treatment_Experience_label$Month <- str_replace(DIA_Treatment_Experience_label$Month, "X60", "60")

DIA_Treatment_Experience_label <- separate_rows(DIA_Treatment_Experience_label, HbA1c, sep = ",", convert=T )

DIA_Treatment_Experience_label <- DIA_Treatment_Experience_label %>%
  mutate(Month = as.numeric(Month)) %>%
  mutate(HbA1c = as.numeric(HbA1c))

cols <- c("deeppink4", "darkslategrey")

DIA_Treatment_Experience_label %>%
  ggplot(aes(x=Month, y=HbA1c, fill=SUM, colour=SUM))+
  geom_smooth(size=1, level=0.99, span=0.1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)



DIA_Treatment_Experience_label %>%filter(!is.na(HbA1c)) %>% filter(Month >48) %>% group_by(SUM) %>%
  summarise(n = mean(HbA1c, na.rm = T))


data.frame(DIA_Treatment_Experience_label %>%
             group_by(Month) %>%
             summarise(mean = mean(HbA1c, na.rm = T)))



# ----
# DRUG PENETRANCE ------------------------------------------------------------------------
# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)


# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION

DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))




DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)




# ------- DIABETES
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


length(unique(DIA_Japan_Drug_Histories$patient)) # 96910
sum(as.numeric(DIA_Japan_Drug_Histories$weight)) #7967485


DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )


names(DIA_Japan_Drug_Histories)[3] <- "molecule"



# Calculate N. of patients on each drug, devide by number of total patients. non-MECE


data.frame(DIA_Japan_Drug_Histories %>% 
             group_by(molecule) %>% 
             summarise(n = n()) %>%
             mutate(percentage = (n/96910)*100) %>%
             left_join(DANU_Japan_Ingredients %>% 
                         select(molecule, generic_name, drug_group, drug_class)))


# Calculate sum of weights of patients on each drug

data.frame(DIA_Japan_Drug_Histories %>% 
             group_by(molecule) %>% 
             summarise(sum_weights = sum(as.numeric(weight))) %>%
             left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group, drug_class)) %>%
             select(generic_name, drug_group, sum_weights, drug_class) %>%
             mutate(sum_weights_percent = (sum_weights / 7967485)*100))






# DIABETES 
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

#204619

names(DIA_Japan_Drug_Histories)[3] <- "molecule"


DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, drug_group)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  distinct()

DIA_Japan_Drug_Histories %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  mutate(sum_weights_percent = (sum_weights / 7967485)*100) %>%
  filter(!is.na(drug_group))





# ----
# MUTUALLY EXCLUSIVE --------------------------------------------------------------------------

# ----
# PENETRANCE PER CATEGORY-------------------------------------------------------------------------

# replace NAs
DIA_Japan_Drug_Histories[is.na(DIA_Japan_Drug_Histories)] <- "Lapsed"


# remove duplicates, i.e. each aptient contribute sonly once per drug group now
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  distinct()

# check which drug_groups we have, just in case
unique(DIA_Japan_Drug_Histories$drug_group)

# replace drug_group codes by numbers
DIA_Japan_Drug_Histories_rank <- 
  DIA_Japan_Drug_Histories %>%
  mutate(drug_group = 
           ifelse(drug_group == "Lapsed", "1", 
                  ifelse(drug_group == "Biguanide", "2", 
                         ifelse(drug_group == "Antiobesity", "3", 
                                ifelse(drug_group == "Antidiabetic", "4",
                                       ifelse(drug_group == "DPP4", "5", 
                                              ifelse(drug_group == "SGLT2", "6",
                                                     ifelse(drug_group == "GLP1 Oral", "7",
                                                            ifelse(drug_group == "GLP1 Injectable", "8",
                                                                   ifelse(drug_group == "Insulin", "9",
                                                                          ifelse(drug_group == "Surgery", "10",
                                                                                 drug_group)))))))))))





# convert drug_group to numeric
DIA_Japan_Drug_Histories_rank$drug_group <-
  as.numeric(DIA_Japan_Drug_Histories_rank$drug_group)


DIA_Japan_Drug_Histories_rank <- DIA_Japan_Drug_Histories_rank %>%
  arrange(patient, drug_group)


# picked the highest number per patients (surgery -> insulin -> GPL inject, etc. as the rank)
DIA_Japan_Drug_Histories_rank <- 
  DIA_Japan_Drug_Histories_rank %>% 
  group_by(patient) %>%
  summarize(across(everything(), max))



# back to labels, note how we have 96910 patients again! Perfect
DIA_Japan_Drug_Histories_rank <- 
  DIA_Japan_Drug_Histories_rank %>%
  mutate(drug_group = 
           ifelse(drug_group == "1", "Lapsed", 
                  ifelse(drug_group == "2", "Biguanide", 
                         ifelse(drug_group == "3", "Antiobesity", 
                                ifelse(drug_group == "4", "Antidiabetic",
                                       ifelse(drug_group == "5", "DPP4", 
                                              ifelse(drug_group == "6", "SGLT2",
                                                     ifelse(drug_group == "7", "GLP1 Oral",
                                                            ifelse(drug_group == "8", "GLP1 Injectable",
                                                                   ifelse(drug_group == "9", "Insulin",
                                                                          ifelse(drug_group == "10", "Surgery",
                                                                                 drug_group)))))))))))




# weights to numeric to sum them up
DIA_Japan_Drug_Histories_rank$weight <- as.numeric(DIA_Japan_Drug_Histories_rank$weight)


# sum of weights as % of total weights for diabetes
DIA_Japan_Drug_Histories_rank %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(weight)) %>%
  mutate(sum_weight_percentage = (sum_weights / 7967485)*100)


names(DIA_Japan_Drug_Histories_rank)[3] <- "drug_group_rank_max"

DIA_Japan_Drug_Histories$weight <- as.numeric(DIA_Japan_Drug_Histories$weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% left_join(DIA_Japan_Drug_Histories_rank)


data.frame(DIA_Japan_Drug_Histories %>%
  group_by(drug_group_rank_max, drug_group) %>%
  summarise(sum_weights = sum(weight)))



# ----
# CLASS PENETRANCE ACROSS THE ENTIRE 60MONTH PERIOD ------------------------------------------

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)


# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))




DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(-c(disease))


length(unique(DIA_Japan_Drug_Histories$patient)) # 96910
sum(as.numeric(DIA_Japan_Drug_Histories$weight)) #7967485


DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat != "-")

names(DIA_Japan_Drug_Histories)[4] <- "molecule"

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-c(Month))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, drug_group)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% distinct()

DIA_Japan_Drug_Histories %>% group_by(drug_group) %>%
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  mutate(sum_weights_percent = (sum_weights / 7967485)*100) %>%
  filter(!is.na(drug_group))





# ----
# Mean duration of each class -------------------------------------
# ----
# GLPs periods duration ----------------------------------------



# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs to zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('38',.), ~replace(., grepl('38', .), "GLP"))%>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "GLP"))%>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "GLP"))%>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "GLP"))%>%
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "GLP"))%>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "GLP"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
GPL_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(GPL_Periods_DIA)[3] <- "Duration"

data.frame(GPL_Periods_DIA %>%
             group_by(Duration) %>%
             summarise(n = n()))







# ----
# SGLs periods duration ---------------------------------------------------------

# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "SGLT2"))%>% 
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "SGLT2"))%>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "SGLT2"))%>%
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "SGLT2"))%>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "SGLT2"))%>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "SGLT2"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SGLT2",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

data.frame(SGLT2_Periods_DIA %>%
             group_by(Duration) %>%
             summarise(n = n()))







# ----
# DPP4s periods duration ------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "DPP4"))%>% 
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "DPP4"))%>% 
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "DPP4"))%>%
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "DPP4"))%>%
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "DPP4"))%>%
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "DPP4"))%>%
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "DPP4"))%>%
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "DPP4"))%>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "DPP4"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="DPP4",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

data.frame(DPP4_Periods_DIA %>%
             group_by(Duration) %>%
             summarise(n = n()))



# ----
# INsulins periods duration -------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"

data.frame(Insulin_Periods_DIA %>%
             group_by(Duration) %>%
             summarise(n = n()))










# ----
# Biguanides periods duration --------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2, 4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="1","Biguanide",Treat)) %>%
  mutate(Treat = ifelse(Treat=="2","Biguanide",Treat))


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="Biguanide","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Biguanide_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Biguanide_Periods_DIA)[3] <- "Duration"

data.frame(Biguanide_Periods_DIA %>%
             group_by(Duration) %>%
             summarise(n = n()))

















# ----
# Antidiabetics periods duration ------------------------------------------------------------------------

# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2, 4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="8","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="9","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="10","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="11","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="12","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="13","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="14","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="15","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="16","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="17","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="18","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="19","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="20","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="21","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="22","Antidiabetic",Treat))


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="Antidiabetic","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Antidiabetic_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Antidiabetic_Periods_DIA)[3] <- "Duration"

data.frame(Antidiabetic_Periods_DIA %>%
             group_by(Duration) %>%
             summarise(n = n()))










# ----
# SImple vs combined therapies -----------------------------------------------------------------------------
# ----
# FOr the patients ON each class, see how many are on just that class vs other classes! ----------------------------------------

# import libs

library(data.table)
library(tidyverse)
library(hacksaw)
library(splitstackshape)
options(scipen = 999)
# keep R from using scientific notation for patient IDs
options(scipen = 999)


# ----
# BIGUANIDES  ---------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_Biguanides <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "1" | month60=="2") %>%
  select(patient)

Patients_Biguanides <- Patients_Biguanides %>% distinct()

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

Patients_Biguanides <- Patients_Biguanides %>% left_join(DIA_Japan_Drug_Histories)





Patients_Biguanides %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))

sum(as.numeric(Patients_Biguanides$weight))




# ----
# ANTIDIABETICS  ---------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_Antidiabetics <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "8" | month60=="9" | month60=="10" | month60=="11"  | month60=="12" |
           month60=="13" |  month60=="14"  | month60=="15"  | month60=="16" |
           month60=="17" | month60=="18"  | month60=="19"  | month60=="20" |
           month60=="21"  | month60=="22") %>%
  select(patient)

Patients_Antidiabetics <- Patients_Antidiabetics %>% distinct()

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

Patients_Antidiabetics <- Patients_Antidiabetics %>% left_join(DIA_Japan_Drug_Histories)




Patients_Antidiabetics %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))

sum(as.numeric(Patients_Antidiabetics$weight))



# ----
# DPP4  ---------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_DPP4 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "23" | month60=="24" | month60=="25" | month60=="26"  | month60=="27" |
           month60=="28" |  month60=="29"  | month60=="30"  | month60=="31") %>%
  select(patient)

Patients_DPP4 <- Patients_DPP4 %>% distinct()

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

Patients_DPP4 <- Patients_DPP4 %>% left_join(DIA_Japan_Drug_Histories)




Patients_DPP4 %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))

sum(as.numeric(Patients_DPP4$weight))


# ----
# SGLT2  ---------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_SGLT2 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "32" | month60=="33" | month60=="34" | month60=="35"  | month60=="36" |
           month60=="37") %>%
  select(patient)

Patients_SGLT2 <- Patients_SGLT2 %>% distinct()

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

Patients_SGLT2 <- Patients_SGLT2 %>% left_join(DIA_Japan_Drug_Histories)



Patients_SGLT2 %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))

sum(as.numeric(Patients_SGLT2$weight))



# ----
# GLP1  ---------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_GLP1 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "38" | month60=="39" | month60=="40" | month60=="41"  | month60=="42" |
           month60=="43") %>%
  select(patient)

Patients_GLP1 <- Patients_GLP1 %>% distinct()

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

Patients_GLP1 <- Patients_GLP1 %>% left_join(DIA_Japan_Drug_Histories)



Patients_GLP1 %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))

sum(as.numeric(Patients_GLP1$weight))



# ----
# Insulin  ---------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_Insulin <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "44" | month60=="45" | month60=="46" | month60=="47"  | month60=="48" |
           month60=="48" | month60 == "49" | month60=="50" | month60=="51" | month60=="52"  | 
           month60=="53" | month60=="54" | month60=="55"  | month60=="56" | month60=="57") %>%
  select(patient)

Patients_Insulin <- Patients_Insulin %>% distinct()

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

Patients_Insulin <- Patients_Insulin %>% left_join(DIA_Japan_Drug_Histories)

Patients_Insulin %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))





# ----
# SIMPLES vs COMBOs, MECE order, within highest rank --------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% select(patient, weight, month60)
sum(as.numeric(DIA_Japan_Drug_Histories$weight))




DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p2, s2)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p2 = as.numeric(p2)) %>% filter(p2 == 60)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(-c(p2))


DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% left_join(DIA_Japan_Drug_Histories)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(month60 != "-")



DIA_Flows_Aux._Long %>% group_by(s2) %>% summarise(n = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% group_by(s2) %>% filter(grepl(",",month60)) %>% summarise(n = sum(as.numeric(weight)))







# ----
# Mean duration of each class (not each piece) --------------------------------------------------------------------
# POPULATION PROJECTED, LAST 12 months, SUM INDIVIDUAL DURATIONS (not each piece)

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)


# ----
# GLPs periods duration --------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)


# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(52:63)


# convert no GLPs to zero, and GLPs to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('38',.), ~replace(., grepl('38', .), "GLP"))%>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "GLP"))%>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "GLP"))%>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "GLP"))%>%
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "GLP"))%>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "GLP"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
GPL_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(GPL_Periods_DIA)[3] <- "Duration"

GPL_Periods_DIA <- GPL_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))


GPL_Periods_DIA <- GPL_Periods_DIA %>% select(patient, sumDuration) %>% distinct()



DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

GPL_Periods_DIA <- GPL_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

GPL_Periods_DIA <- GPL_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

GPL_Periods_DIA <- setDT(expandRows(GPL_Periods_DIA, "weight"))[]

data.frame(GPL_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))


# ----
# SGLs periods duration ----------------------------------------------------
# Calculate the duration of GLPs periods for each patient 
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(52:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "SGLT2"))%>% 
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "SGLT2"))%>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "SGLT2"))%>%
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "SGLT2"))%>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "SGLT2"))%>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "SGLT2"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SGLT2",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"


SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% select(patient, sumDuration) %>% distinct()




DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

SGLT2_Periods_DIA <- setDT(expandRows(SGLT2_Periods_DIA, "weight"))[]

data.frame(SGLT2_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))



# ----
# DPP4s periods duration -------------------------------------------------------------------
# Calculate the duration of GLPs periods for each patient

# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(52:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "DPP4"))%>% 
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "DPP4"))%>% 
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "DPP4"))%>%
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "DPP4"))%>%
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "DPP4"))%>%
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "DPP4"))%>%
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "DPP4"))%>%
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "DPP4"))%>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "DPP4"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="DPP4",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"


DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, sumDuration) %>% distinct()


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()


DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- setDT(expandRows(DPP4_Periods_DIA, "weight"))[]

data.frame(DPP4_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))




# ----
# INsulins periods duration -------------------------------------------------------------------
#  Calculate the duration of Insulins periods for each patient
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(52:63)

# convert no insuilins too zero, and insulins to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"


Insulin_Periods_DIA <- Insulin_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% select(patient, sumDuration) %>% distinct()

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

Insulin_Periods_DIA <- setDT(expandRows(Insulin_Periods_DIA, "weight"))[]


data.frame(Insulin_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))








# ----
# Biguanides periods duration -------------------------------------------------------------------
#  Calculate the duration of Biguanides periods for each patient

# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2,3, 52:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="1","Biguanide",Treat)) %>%
  mutate(Treat = ifelse(Treat=="2","Biguanide",Treat))


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="Biguanide","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Biguanide_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Biguanide_Periods_DIA)[3] <- "Duration"


Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% select(patient, sumDuration) %>% distinct()


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

Biguanide_Periods_DIA <- setDT(expandRows(Biguanide_Periods_DIA, "weight"))[]


data.frame(Biguanide_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))

# ----
# Antidiabetics periods duration -------------------------------------------------------------------
#  Calculate the duration of Antidiabetics periods for each patient

# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2, 3, 52:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="8","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="9","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="10","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="11","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="12","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="13","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="14","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="15","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="16","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="17","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="18","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="19","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="20","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="21","Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="22","Antidiabetic",Treat))


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="Antidiabetic","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Antidiabetic_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Antidiabetic_Periods_DIA)[3] <- "Duration"


Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% select(patient, sumDuration) %>% distinct()


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

Antidiabetic_Periods_DIA <- setDT(expandRows(Antidiabetic_Periods_DIA, "weight"))[]


data.frame(Antidiabetic_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))









# ----
# How long on each of the different Antidiabetics? --------------------------------

# ----
# OTHER ANTIDIABETICS ---------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2, 3, 52:63)


# convert no OTHER DIABETICS too zero, and OTHER DIABETICS to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="8","Other_Antidiabetic",Treat)) %>%
  mutate(Treat = ifelse(Treat=="9","Other_Antidiabetic",Treat)) 


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="Other_Antidiabetic","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Other_Antidiabetic_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Other_Antidiabetic_Periods_DIA)[3] <- "Duration"

Other_Antidiabetic_Periods_DIA <- Other_Antidiabetic_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

Other_Antidiabetic_Periods_DIA <- Other_Antidiabetic_Periods_DIA %>% select(patient, sumDuration) %>% distinct()


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Other_Antidiabetic_Periods_DIA <- Other_Antidiabetic_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

Other_Antidiabetic_Periods_DIA <- Other_Antidiabetic_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

Other_Antidiabetic_Periods_DIA <- setDT(expandRows(Other_Antidiabetic_Periods_DIA, "weight"))[]

data.frame(Other_Antidiabetic_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))



# ----
# AGIs ------------------------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2, 3, 52:63)


# convert no AGIs too zero, and AGIs to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="10","AGI",Treat)) %>%
  mutate(Treat = ifelse(Treat=="11","AGI",Treat)) %>%
  mutate(Treat = ifelse(Treat=="12","AGI",Treat)) 


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="AGI","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
AGI_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(AGI_Periods_DIA)[3] <- "Duration"

AGI_Periods_DIA <- AGI_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

AGI_Periods_DIA <- AGI_Periods_DIA %>% select(patient, sumDuration) %>% distinct()


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

AGI_Periods_DIA <- AGI_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

AGI_Periods_DIA <- AGI_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

AGI_Periods_DIA <- setDT(expandRows(AGI_Periods_DIA, "weight"))[]

data.frame(AGI_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))


# ----
# GLinides -------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2, 3, 52:63)


# convert no GLINIDES too zero, and GLINIDES to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="19","Glinide",Treat)) %>%
  mutate(Treat = ifelse(Treat=="20","Glinide",Treat)) %>%
  mutate(Treat = ifelse(Treat=="21","GLinide",Treat)) 


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="Glinide","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Glinide_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Glinide_Periods_DIA)[3] <- "Duration"

Glinide_Periods_DIA <- Glinide_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

Glinide_Periods_DIA <- Glinide_Periods_DIA %>% select(patient, sumDuration) %>% distinct()


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Glinide_Periods_DIA <- Glinide_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

Glinide_Periods_DIA <- Glinide_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

Glinide_Periods_DIA <- setDT(expandRows(Glinide_Periods_DIA, "weight"))[]

data.frame(Glinide_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))





# ----
# Glitazone -------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2, 3, 52:63)


# convert no Glitazone too zero, and GLitazone to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="22","Glitazone",Treat))


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="Glitazone","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Glitazone_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Glitazone_Periods_DIA)[3] <- "Duration"

Glitazone_Periods_DIA <- Glitazone_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

Glitazone_Periods_DIA <- Glitazone_Periods_DIA %>% select(patient, sumDuration) %>% distinct()


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Glitazone_Periods_DIA <- Glitazone_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

Glitazone_Periods_DIA <- Glitazone_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

Glitazone_Periods_DIA <- setDT(expandRows(Glitazone_Periods_DIA, "weight"))[]

data.frame(Glitazone_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))









# ----
# Sulfonylurea -------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(2, 3, 52:63)


# convert no Sulfonylurea too zero, and Sulfonylurea to one
# convert to numeric everything

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month49:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="13","Sulfonylurea",Treat)) %>%
  mutate(Treat = ifelse(Treat=="14","Sulfonylurea",Treat)) %>%
  mutate(Treat = ifelse(Treat=="15","Sulfonylurea",Treat)) %>%
  mutate(Treat = ifelse(Treat=="16","Sulfonylurea",Treat)) %>%
  mutate(Treat = ifelse(Treat=="17","Sulfonylurea",Treat)) %>%
  mutate(Treat = ifelse(Treat=="18","Sulfonylurea",Treat))


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate(Treat = ifelse(Treat=="Sulfonylurea","1","0"))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% arrange(patient, Month, Treat)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, Month) %>%
  summarize(across(everything(), max))


DIA_Japan_Drug_Histories$Treat <-  as.numeric(DIA_Japan_Drug_Histories$Treat)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Sulfonylurea_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Sulfonylurea_Periods_DIA)[3] <- "Duration"

Sulfonylurea_Periods_DIA <- Sulfonylurea_Periods_DIA %>% group_by(patient) %>%
  mutate(sumDuration = sum(Duration))

Sulfonylurea_Periods_DIA <- Sulfonylurea_Periods_DIA %>% select(patient, sumDuration) %>% distinct()


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Sulfonylurea_Periods_DIA <- Sulfonylurea_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories)

Sulfonylurea_Periods_DIA <- Sulfonylurea_Periods_DIA %>% 
  mutate(weight = as.numeric(weight))

Sulfonylurea_Periods_DIA <- setDT(expandRows(Sulfonylurea_Periods_DIA, "weight"))[]

data.frame(Sulfonylurea_Periods_DIA %>%
             group_by(sumDuration) %>%
             summarise(n = n()))





# ----
#Mono vs combined therapy GLP1 -----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_GLP1 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "38" | month60=="39" | month60=="40" | month60=="41"  | month60=="42" |
           month60=="43") %>%
  select(patient)


Patients_GLP1 <- Patients_GLP1 %>% distinct() 4099
225+3876

# ORAL vs INjectable, just to check 
Patients_GLP1_38 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "38") %>%
  select(patient)

Patients_GLP1_38 <- Patients_GLP1_38 %>% distinct() #225


Patients_GLP1_39_43 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60=="39" | month60=="40" | month60=="41"  | month60=="42" |
           month60=="43") %>%
  select(patient)

Patients_GLP1_39_43 <- Patients_GLP1_39_43 %>% distinct() #3876


Patients_GLP1_39_43 %>% inner_join(Patients_GLP1_38) #2 patients on both


####### ALLL GLP1s patients
Patients_GLP1 <- Patients_GLP1 %>% distinct()

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

Patients_GLP1 <- Patients_GLP1 %>% left_join(DIA_Japan_Drug_Histories)


# ALL GLP1s -> to -> combos
Patients_GLP1 %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))


sum(as.numeric(Patients_GLP1$weight))


# now we have just the combo patients
Patients_GLP1_combos <- Patients_GLP1 %>% ungroup() %>% filter(grepl(",",month60))

sum(as.numeric(Patients_GLP1_combos$weight)) # 260986.3


# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)
# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))


Patients_GLP1_combos <- 
  separate_rows(Patients_GLP1_combos, month60, sep = ",", convert=T )

names(Patients_GLP1_combos)[3] <- "molecule"


Patients_GLP1_combos$molecule <- as.character(Patients_GLP1_combos$molecule)



Patients_GLP1_combos <- 
  Patients_GLP1_combos %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))

Patients_GLP1_combos <- 
  Patients_GLP1_combos %>% 
  select(patient, weight, drug_group)

Patients_GLP1_combos <- 
  Patients_GLP1_combos %>% 
  distinct()



# replace drug_group codes by numbers
Patients_GLP1_combos <- 
  Patients_GLP1_combos %>%
  mutate(drug_group = 
           ifelse(drug_group == "Lapsed", "1", 
                  ifelse(drug_group == "Biguanide", "2", 
                         ifelse(drug_group == "Antiobesity", "3", 
                                ifelse(drug_group == "Antidiabetic", "4",
                                       ifelse(drug_group == "DPP4", "5", 
                                              ifelse(drug_group == "SGLT2", "6",
                                                     ifelse(drug_group == "GLP1 Oral", "7",
                                                            ifelse(drug_group == "GLP1 Injectable", "8",
                                                                   ifelse(drug_group == "Insulin", "9",
                                                                          ifelse(drug_group == "Surgery", "10",
                                                                                 drug_group)))))))))))

Patients_GLP1_combos <- Patients_GLP1_combos %>% filter(drug_group != 10  & drug_group != 8 & drug_group != 7 )

# convert drug_group to numeric
Patients_GLP1_combos$drug_group <-
  as.numeric(Patients_GLP1_combos$drug_group)

Patients_GLP1_combos <- Patients_GLP1_combos %>%
  arrange(patient, drug_group)

# picked the highest number per patients (surgery -> insulin -> GPL inject, etc. as the rank)
Patients_GLP1_combos <- 
  Patients_GLP1_combos %>% 
  group_by(patient) %>%
  summarize(across(everything(), max))

Patients_GLP1_combos <- 
  Patients_GLP1_combos %>%
  mutate(drug_group = 
           ifelse(drug_group == "1", "Lapsed", 
                  ifelse(drug_group == "2", "Biguanide", 
                         ifelse(drug_group == "3", "Antiobesity", 
                                ifelse(drug_group == "4", "Antidiabetic",
                                       ifelse(drug_group == "5", "DPP4", 
                                              ifelse(drug_group == "6", "SGLT2",
                                                     ifelse(drug_group == "7", "GLP1 Oral",
                                                            ifelse(drug_group == "8", "GLP1 Injectable",
                                                                   ifelse(drug_group == "9", "Insulin",
                                                                          ifelse(drug_group == "10", "Surgery",
                                                                                 drug_group)))))))))))

# weights to numeric to sum them up
Patients_GLP1_combos$weight <-
  as.numeric(Patients_GLP1_combos$weight)

sum(Patients_GLP1_combos$weight)
# new sum of weights = 260986.3 (removed surgery, GLP1s)
# sum of weights as % of total weights for diabetes
Patients_GLP1_combos %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(weight)) %>%
  mutate(sum_weight_percentage = (sum_weights / 260986.3)*100)



# ----
# Mono vs combined therapy water fall SGLT2 --------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_SGLT2 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "32" | month60=="33" | month60=="34" | month60=="35"  | month60=="36" |
           month60=="37") %>%
  select(patient)


# ALLL SGLT2 patients 
Patients_SGLT2 <- Patients_SGLT2 %>% distinct()

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)

Patients_SGLT2 <- Patients_SGLT2 %>% left_join(DIA_Japan_Drug_Histories)

# ALL SGLT2 -> to -> combos
Patients_SGLT2 %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))

#2060414 # combos weight

sum(as.numeric(Patients_SGLT2$weight)) # 2363683  all SGLT2

#simple 303269
# combo 2060414

# now we have just the combo patients
Patients_SGLT2_combos <- Patients_SGLT2 %>% ungroup() %>% filter(grepl(",",month60))

sum(as.numeric(Patients_SGLT2_combos$weight)) # 2060414




# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)
# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))


Patients_SGLT2_combos <- 
  separate_rows(Patients_SGLT2_combos, month60, sep = ",", convert=T )

names(Patients_SGLT2_combos)[3] <- "molecule"

Patients_SGLT2_combos$molecule <- as.character(Patients_SGLT2_combos$molecule)



Patients_SGLT2_combos <- 
  Patients_SGLT2_combos %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))

Patients_SGLT2_combos <- 
  Patients_SGLT2_combos %>% 
  select(patient, weight, drug_group)

Patients_SGLT2_combos <- 
  Patients_SGLT2_combos %>% 
  distinct()


# replace drug_group codes by numbers
Patients_SGLT2_combos <- 
  Patients_SGLT2_combos %>%
  mutate(drug_group = 
           ifelse(drug_group == "Lapsed", "1", 
                  ifelse(drug_group == "Biguanide", "2", 
                         ifelse(drug_group == "Antiobesity", "3", 
                                ifelse(drug_group == "Antidiabetic", "4",
                                       ifelse(drug_group == "DPP4", "5", 
                                              ifelse(drug_group == "SGLT2", "6",
                                                     ifelse(drug_group == "GLP1 Oral", "7",
                                                            ifelse(drug_group == "GLP1 Injectable", "8",
                                                                   ifelse(drug_group == "Insulin", "9",
                                                                          ifelse(drug_group == "Surgery", "10",
                                                                                 drug_group)))))))))))

Patients_SGLT2_combos <- Patients_SGLT2_combos %>% filter(drug_group != 10  & drug_group != 6 )


# convert drug_group to numeric
Patients_SGLT2_combos$drug_group <- as.numeric(Patients_SGLT2_combos$drug_group)

Patients_SGLT2_combos <- Patients_SGLT2_combos %>% arrange(patient, drug_group)

# picked the highest number per patients (surgery -> insulin -> GPL inject, etc. as the rank)
Patients_SGLT2_combos <- 
  Patients_SGLT2_combos %>% 
  group_by(patient) %>%
  summarize(across(everything(), max))

Patients_SGLT2_combos <- 
  Patients_SGLT2_combos %>%
  mutate(drug_group = 
           ifelse(drug_group == "1", "Lapsed", 
                  ifelse(drug_group == "2", "Biguanide", 
                         ifelse(drug_group == "3", "Antiobesity", 
                                ifelse(drug_group == "4", "Antidiabetic",
                                       ifelse(drug_group == "5", "DPP4", 
                                              ifelse(drug_group == "6", "SGLT2",
                                                     ifelse(drug_group == "7", "GLP1 Oral",
                                                            ifelse(drug_group == "8", "GLP1 Injectable",
                                                                   ifelse(drug_group == "9", "Insulin",
                                                                          ifelse(drug_group == "10", "Surgery",
                                                                                 drug_group)))))))))))

# weights to numeric to sum them up
Patients_SGLT2_combos$weight <- as.numeric(Patients_SGLT2_combos$weight)

sum(Patients_SGLT2_combos$weight)  #2060368

# sum of weights as % of total weights for diabetes
Patients_SGLT2_combos %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(weight)) %>%
  mutate(sum_weight_percentage = (sum_weights / 2060368)*100)



# ----
# Waterfall constitution of other drugs (combos) ORAL vs INJECT GLPs ----------------------------------------------------------------------------

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


DIA_Japan_Drug_Histories <- 
  separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_GLP1 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "38" | month60=="39" | month60=="40" | month60=="41"  | month60=="42" |
           month60=="43") 

length(unique(Patients_GLP1$patient))

Patients_GLP1_ORAL <- Patients_GLP1  %>% filter(month60 == "38") %>% select(patient)
Patients_GLP1_ORAL <- Patients_GLP1_ORAL %>% distinct()

Patients_GLP1_INJECT <- Patients_GLP1  %>% filter(month60=="39" | month60=="40" | month60=="41"  | month60=="42" | month60=="43") %>% select(patient)
Patients_GLP1_INJECT <- Patients_GLP1_INJECT %>% distinct()




DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- 
  DIA_Japan_Drug_Histories %>% 
  select(patient, weight, month60)


Patients_GLP1_ORAL <- Patients_GLP1_ORAL %>% left_join(DIA_Japan_Drug_Histories)
Patients_GLP1_INJECT <- Patients_GLP1_INJECT %>% left_join(DIA_Japan_Drug_Histories)




# JUST ORAL
Patients_GLP1_ORAL %>% ungroup() %>% filter(grepl(",",month60)) %>%
  summarise(n = sum(as.numeric(weight)))

sum(as.numeric(Patients_GLP1_ORAL$weight))
#total weight 14361.5
# combo weight 13226
# simple weight = 14361.5 - 13226 = 1135.5


Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL %>% ungroup() %>% filter(grepl(",",month60))

sum(as.numeric(Patients_GLP1_ORAL_combos$weight)) # 13225.78


# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)
# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))

Patients_GLP1_ORAL_combos <- separate_rows(Patients_GLP1_ORAL_combos, month60, sep = ",", convert=T )

names(Patients_GLP1_ORAL_combos)[3] <- "molecule"

Patients_GLP1_ORAL_combos$molecule <- as.character(Patients_GLP1_ORAL_combos$molecule)

Patients_GLP1_ORAL_combos <- 
  Patients_GLP1_ORAL_combos %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name, drug_group))

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>%  select(patient, weight, drug_group)

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% distinct()


# replace drug_group codes by numbers
Patients_GLP1_ORAL_combos <- 
  Patients_GLP1_ORAL_combos %>%
  mutate(drug_group = 
           ifelse(drug_group == "Lapsed", "1", 
                  ifelse(drug_group == "Biguanide", "2", 
                         ifelse(drug_group == "Antiobesity", "3", 
                                ifelse(drug_group == "Antidiabetic", "4",
                                       ifelse(drug_group == "DPP4", "5", 
                                              ifelse(drug_group == "SGLT2", "6",
                                                     ifelse(drug_group == "GLP1 Oral", "7",
                                                            ifelse(drug_group == "GLP1 Injectable", "8",
                                                                   ifelse(drug_group == "Insulin", "9",
                                                                          ifelse(drug_group == "Surgery", "10",
                                                                                 drug_group)))))))))))

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% filter(drug_group != 10  & drug_group != 7 )

# convert drug_group to numeric
Patients_GLP1_ORAL_combos$drug_group <- as.numeric(Patients_GLP1_ORAL_combos$drug_group)

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% arrange(patient, drug_group)

# picked the highest number per patients (surgery -> insulin -> GPL inject, etc. as the rank)
Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% group_by(patient) %>% summarize(across(everything(), max))

Patients_GLP1_ORAL_combos <- 
  Patients_GLP1_ORAL_combos %>%
  mutate(drug_group = 
           ifelse(drug_group == "1", "Lapsed", 
                  ifelse(drug_group == "2", "Biguanide", 
                         ifelse(drug_group == "3", "Antiobesity", 
                                ifelse(drug_group == "4", "Antidiabetic",
                                       ifelse(drug_group == "5", "DPP4", 
                                              ifelse(drug_group == "6", "SGLT2",
                                                     ifelse(drug_group == "7", "GLP1 Oral",
                                                            ifelse(drug_group == "8", "GLP1 Injectable",
                                                                   ifelse(drug_group == "9", "Insulin",
                                                                          ifelse(drug_group == "10", "Surgery",
                                                                                 drug_group)))))))))))

# weights to numeric to sum them up
Patients_GLP1_ORAL_combos$weight <- as.numeric(Patients_GLP1_ORAL_combos$weight)

sum(Patients_GLP1_ORAL_combos$weight)
# new sum of weights = 13225.78 (removed surgery, GLP1s oral)
# sum of weights as % of total weights for diabetes
Patients_GLP1_ORAL_combos %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(weight)) %>%
  mutate(sum_weight_percentage = (sum_weights / 13225.78)*100)


# JUST INJECTABLE
Patients_GLP1_INJECT %>% ungroup() %>% filter(grepl(",",month60)) %>% summarise(n = sum(as.numeric(weight)))

sum(as.numeric(Patients_GLP1_INJECT$weight))
#total weight 259963.5
# combo weight 247827
# simple weight = 259963.5 - 247827 = 12136.5


Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT %>% ungroup() %>% filter(grepl(",",month60))

sum(as.numeric(Patients_GLP1_INJECT_combos$weight)) # 247826.5


# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)
# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- 
  DANU_Japan_Ingredients %>% 
  separate(drug_id, c('class', 'molecule'))

Patients_GLP1_INJECT_combos <- separate_rows(Patients_GLP1_INJECT_combos, month60, sep = ",", convert=T )

names(Patients_GLP1_INJECT_combos)[3] <- "molecule"

Patients_GLP1_INJECT_combos$molecule <- as.character(Patients_GLP1_INJECT_combos$molecule)

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% left_join(DANU_Japan_Ingredients %>%  select(molecule, generic_name, drug_group))

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>%  select(patient, weight, drug_group)

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% distinct()


# replace drug_group codes by numbers
Patients_GLP1_INJECT_combos <- 
  Patients_GLP1_INJECT_combos %>%
  mutate(drug_group = 
           ifelse(drug_group == "Lapsed", "1", 
                  ifelse(drug_group == "Biguanide", "2", 
                         ifelse(drug_group == "Antiobesity", "3", 
                                ifelse(drug_group == "Antidiabetic", "4",
                                       ifelse(drug_group == "DPP4", "5", 
                                              ifelse(drug_group == "SGLT2", "6",
                                                     ifelse(drug_group == "GLP1 Oral", "7",
                                                            ifelse(drug_group == "GLP1 Injectable", "8",
                                                                   ifelse(drug_group == "Insulin", "9",
                                                                          ifelse(drug_group == "Surgery", "10",
                                                                                 drug_group)))))))))))

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% filter(drug_group != 10  & drug_group != 8 )

# convert drug_group to numeric
Patients_GLP1_INJECT_combos$drug_group <- as.numeric(Patients_GLP1_INJECT_combos$drug_group)

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% arrange(patient, drug_group)

# picked the highest number per patients (surgery -> insulin -> GPL inject, etc. as the rank)
Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% group_by(patient) %>% summarize(across(everything(), max))

Patients_GLP1_INJECT_combos <- 
  Patients_GLP1_INJECT_combos %>%
  mutate(drug_group = 
           ifelse(drug_group == "1", "Lapsed", 
                  ifelse(drug_group == "2", "Biguanide", 
                         ifelse(drug_group == "3", "Antiobesity", 
                                ifelse(drug_group == "4", "Antidiabetic",
                                       ifelse(drug_group == "5", "DPP4", 
                                              ifelse(drug_group == "6", "SGLT2",
                                                     ifelse(drug_group == "7", "GLP1 Oral",
                                                            ifelse(drug_group == "8", "GLP1 Injectable",
                                                                   ifelse(drug_group == "9", "Insulin",
                                                                          ifelse(drug_group == "10", "Surgery",
                                                                                 drug_group)))))))))))

# weights to numeric to sum them up
Patients_GLP1_INJECT_combos$weight <- as.numeric(Patients_GLP1_INJECT_combos$weight)

sum(Patients_GLP1_INJECT_combos$weight)
# new sum of weights = 247826.5 (removed surgery, GLP1s oral)
# sum of weights as % of total weights for diabetes
Patients_GLP1_INJECT_combos %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(weight)) %>%
  mutate(sum_weight_percentage = (sum_weights / 247826.5)*100)







# ----
# Distribution of COMBOS GLPs Contents DRUG GROUP ---------------------------------------------------------------------

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%  select(patient, weight, month60)

DIA_Japan_Drug_Histories <- separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_GLP1 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "38" | month60=="39" | month60=="40" | month60=="41"  | month60=="42" |
           month60=="43") 

length(unique(Patients_GLP1$patient)) #4099 total GLPs


Patients_GLP1_ORAL <- Patients_GLP1  %>% filter(month60 == "38") %>% select(patient)
Patients_GLP1_ORAL <- Patients_GLP1_ORAL %>% distinct()

Patients_GLP1_INJECT <- Patients_GLP1  %>% filter(month60=="39" | month60=="40" | month60=="41"  | month60=="42" | month60=="43") %>% select(patient)
Patients_GLP1_INJECT <- Patients_GLP1_INJECT %>% distinct()


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, month60)

Patients_GLP1_ORAL <- Patients_GLP1_ORAL %>% left_join(DIA_Japan_Drug_Histories)
Patients_GLP1_INJECT <- Patients_GLP1_INJECT %>% left_join(DIA_Japan_Drug_Histories)




# ----
# Constitution of concomitant drug therapy among ORAL GLP1 patients ------------------------------
Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL %>% ungroup() %>% filter(grepl(",",month60))

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

Patients_GLP1_ORAL_combos <- separate_rows(Patients_GLP1_ORAL_combos, month60, sep = ",", convert=T )

names(Patients_GLP1_ORAL_combos)[3] <- "molecule"

Patients_GLP1_ORAL_combos$molecule <- as.character(Patients_GLP1_ORAL_combos$molecule)

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group))

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>%  select(patient, weight, drug_group)

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% distinct()

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% group_by(patient) %>% arrange(drug_group)

Combos_weights <- Patients_GLP1_ORAL_combos %>% select(patient, weight)

Combos_weights <- Combos_weights %>% distinct()

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% group_by(patient) %>% summarise(drug_groups = toString(drug_group))

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% left_join(Combos_weights)

Patients_GLP1_ORAL_combos_DRUG_GROUPS <- data.frame(Patients_GLP1_ORAL_combos %>% group_by(drug_groups) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n))
write.csv(Patients_GLP1_ORAL_combos_DRUG_GROUPS, "Patients_GLP1_ORAL_combos_DRUG_GROUPS.csv")

Patients_GLP1_ORAL_combos %>% group_by(drug_groups) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n) %>%
  mutate(drug_groups= as.factor(drug_groups)) %>%
  ggplot(aes(x=n, y=reorder(drug_groups,n))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="firebrick" )+
  xlab("\nProjected Population") + ylab("Drug Group Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy among ORAL GLP1 patients")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())




# ----
# Constitution of concomitant drug therapy among INJECTABLE GLP1 patients -------------------
Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT %>% ungroup() %>% filter(grepl(",",month60))

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

Patients_GLP1_INJECT_combos <- separate_rows(Patients_GLP1_INJECT_combos, month60, sep = ",", convert=T )

names(Patients_GLP1_INJECT_combos)[3] <- "molecule"

Patients_GLP1_INJECT_combos$molecule <- as.character(Patients_GLP1_INJECT_combos$molecule)

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group))

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>%  select(patient, weight, drug_group)

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% distinct()

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% group_by(patient) %>% arrange(drug_group)

Combos_weights <- Patients_GLP1_INJECT_combos %>% select(patient, weight)

Combos_weights <- Combos_weights %>% distinct()

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% group_by(patient) %>% summarise(drug_groups = toString(drug_group))

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% left_join(Combos_weights)

Patients_GLP1_INJECT_combos_DRUG_GROUPS <- data.frame(Patients_GLP1_INJECT_combos %>% group_by(drug_groups) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n))
write.csv(Patients_GLP1_INJECT_combos_DRUG_GROUPS, "Patients_GLP1_INJECT_combos_DRUG_GROUPS.csv")
# Patients_GLP1_INJECT_combos %>% group_by(drug_groups) %>% summarise(n = sum(as.numeric(weight))) %>% summarise(n2 = sum(n))
# 247827

Patients_GLP1_INJECT_combos %>% group_by(drug_groups) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n) %>%
  mutate(drug_groups= as.factor(drug_groups)) %>%
  filter(n>1000)%>%
  ggplot(aes(x=n, y=reorder(drug_groups,n))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="midnightblue")+
  xlab("\nProjected Population") + ylab("Drug Group Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy among INJECTABLE GLP1 patients", subtitle = "n>1000")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



# ----
# Distribution of COMBOS GLPs Contents DRUG CLASS ---------------------------------------------------------------------

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%  select(patient, weight, month60)

DIA_Japan_Drug_Histories <- separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_GLP1 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "38" | month60=="39" | month60=="40" | month60=="41"  | month60=="42" |
           month60=="43") 

length(unique(Patients_GLP1$patient)) #4099 total GLPs


Patients_GLP1_ORAL <- Patients_GLP1  %>% filter(month60 == "38") %>% select(patient)
Patients_GLP1_ORAL <- Patients_GLP1_ORAL %>% distinct()

Patients_GLP1_INJECT <- Patients_GLP1  %>% filter(month60=="39" | month60=="40" | month60=="41"  | month60=="42" | month60=="43") %>% select(patient)
Patients_GLP1_INJECT <- Patients_GLP1_INJECT %>% distinct()


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, month60)

Patients_GLP1_ORAL <- Patients_GLP1_ORAL %>% left_join(DIA_Japan_Drug_Histories)
Patients_GLP1_INJECT <- Patients_GLP1_INJECT %>% left_join(DIA_Japan_Drug_Histories)




# ----
# Constitution of concomitant drug therapy classes among ORAL GLP1 patients ------------------------------
Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL %>% ungroup() %>% filter(grepl(",",month60))

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))



Patients_GLP1_ORAL_combos <- separate_rows(Patients_GLP1_ORAL_combos, month60, sep = ",", convert=T )

names(Patients_GLP1_ORAL_combos)[3] <- "molecule"

Patients_GLP1_ORAL_combos$molecule <- as.character(Patients_GLP1_ORAL_combos$molecule)

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_class))

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>%  select(patient, weight, drug_class)

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% distinct()

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% group_by(patient) %>% arrange(drug_class)

Combos_weights <- Patients_GLP1_ORAL_combos %>% select(patient, weight)

Combos_weights <- Combos_weights %>% distinct()

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% group_by(patient) %>% summarise(drug_class = toString(drug_class))

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>% left_join(Combos_weights)

Patients_GLP1_ORAL_combos_DRUG_CLASSES <- data.frame(Patients_GLP1_ORAL_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n))
write.csv(Patients_GLP1_ORAL_combos_DRUG_CLASSES, "Patients_GLP1_ORAL_combos_DRUG_CLASSES.csv")

Patients_GLP1_ORAL_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n) %>%
  mutate(drug_class= as.factor(drug_class)) %>%
  filter(n>50)%>%
  ggplot(aes(x=n, y=reorder(drug_class,n))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="firebrick" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy classes among ORAL GLP1 patients", subtitle = "n>50")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())




# ----
# Constitution of concomitant drug therapy classes among INJECTABLE GLP1 patient ----------------------------
Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT %>% ungroup() %>% filter(grepl(",",month60))

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

Patients_GLP1_INJECT_combos <- separate_rows(Patients_GLP1_INJECT_combos, month60, sep = ",", convert=T )

names(Patients_GLP1_INJECT_combos)[3] <- "molecule"

Patients_GLP1_INJECT_combos$molecule <- as.character(Patients_GLP1_INJECT_combos$molecule)

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_class))

Patients_GLP1_ORAL_combos <- Patients_GLP1_ORAL_combos %>%  select(patient, weight, drug_class)

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% distinct()

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% group_by(patient) %>% arrange(drug_class)

Combos_weights <- Patients_GLP1_INJECT_combos %>% select(patient, weight)

Combos_weights <- Combos_weights %>% distinct()

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% group_by(patient) %>% summarise(drug_class = toString(drug_class))

Patients_GLP1_INJECT_combos <- Patients_GLP1_INJECT_combos %>% left_join(Combos_weights)

Patients_GLP1_INJECT_combos_DRUG_CLASEES <- data.frame(Patients_GLP1_INJECT_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n))
write.csv(Patients_GLP1_INJECT_combos_DRUG_CLASEES, "Patients_GLP1_INJECT_combos_DRUG_CLASEES.csv")
Patients_GLP1_INJECT_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% summarise(n2 = sum(n))
# 247827

Patients_GLP1_INJECT_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n) %>%
  mutate(drug_class= as.factor(drug_class)) %>%
  filter(n>2000)%>%
  ggplot(aes(x=n, y=reorder(drug_class,n))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="midnightblue")+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy classes among INJECTABLE GLP1 patients", subtitle = "n>2000")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())







# ----
# Cnstitution of concomitant drug therapy classes among INJECTABLE GLP1 patients --------------------


Patients_GLP1_ORAL_combos_DRUG_CLASSES <- read.table("Patients_GLP1_ORAL_combos_DRUG_CLASSES.csv", 
                                                     header = T, sep=",", 
                                                     colClasses = "character", stringsAsFactors = FALSE)


sum(as.numeric(Patients_GLP1_ORAL_combos_DRUG_CLASSES$n)) # 13225.78

Patients_GLP1_ORAL_combos_DRUG_CLASSES <- separate_rows(Patients_GLP1_ORAL_combos_DRUG_CLASSES, drug_class, sep = ",", convert=T )

Patients_GLP1_ORAL_combos_DRUG_CLASSES$drug_class <- str_trim(Patients_GLP1_ORAL_combos_DRUG_CLASSES$drug_class, side = c("left"))

Patients_GLP1_ORAL_combos_DRUG_CLASSES %>% group_by(drug_class) %>% summarise(n = n())

Patients_GLP1_ORAL_combos_DRUG_CLASSES %>% group_by(drug_class) %>% summarise(sumn = sum(as.numeric(n))) %>% arrange(sumn) %>%
  mutate(percentage = (sumn / 13225.78)*100)%>%
  mutate(drug_class= as.factor(drug_class)) %>%
  ggplot(aes(x=reorder(drug_class,percentage), y=percentage, label=round(percentage, digits = 0))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="firebrick" )+
  xlab("Class") + ylab("% Share\n")+
  ggtitle("% Share of concomitant drug therapy classes among ORAL GLP1 patients")+
  geom_text(vjust=-1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))


Patients_GLP1_INJECT_combos_DRUG_CLASEES <- read.table("Patients_GLP1_INJECT_combos_DRUG_CLASEES.csv", 
                                                       header = T, sep=",", 
                                                       colClasses = "character", stringsAsFactors = FALSE)

sum(as.numeric(Patients_GLP1_INJECT_combos_DRUG_CLASEES$n)) # 247826.5


Patients_GLP1_INJECT_combos_DRUG_CLASEES <- separate_rows(Patients_GLP1_INJECT_combos_DRUG_CLASEES, drug_class, sep = ",", convert=T )

Patients_GLP1_INJECT_combos_DRUG_CLASEES$drug_class <- str_trim(Patients_GLP1_INJECT_combos_DRUG_CLASEES$drug_class, side = c("left"))

Patients_GLP1_INJECT_combos_DRUG_CLASEES %>% group_by(drug_class) %>% summarise(n = n())

Patients_GLP1_INJECT_combos_DRUG_CLASEES %>% group_by(drug_class) %>% summarise(sumn = sum(as.numeric(n))) %>% arrange(sumn) %>%
  mutate(percentage = (sumn / 247826.5)*100)%>%
  mutate(drug_class= as.factor(drug_class)) %>%
  ggplot(aes(x=reorder(drug_class,percentage), y=percentage, label=round(percentage, digits = 0))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="midnightblue" )+
  xlab("Class") + ylab("% Share\n")+
  ggtitle("% Share of concomitant drug therapy classes among INJECTABLE GLP1 patients")+
  geom_text(vjust=-1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))







# ----
# TOTAL FLOWS TO GPLs ORAL LAST 12 PERIODS (48 - 60)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(s1 != "g") %>% filter(s2 == "g")

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #14230.76

DIA_Flows_Aux._Long %>% filter(s2 == "g") %>% summarise(sumweight = sum(as.numeric(weight))) #14230.76 # Oral

DIA_Flows_Aux._Long %>% filter(s2 == "g") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))



DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(!grepl("38", d1)) %>% filter(grepl("38", d2))

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #1231.68

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))




# ----
# TOTAL FLOWS TO GPLs INJECT LAST 12 PERIODS (48 - 60)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(s1 != "G") %>% filter(s2 == "G")

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #204010.7

DIA_Flows_Aux._Long %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))




DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)


DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d1) & !grepl("40",d1) & !grepl("41",d1) & !grepl("42",d1) & !grepl("43",d1)) & 
         (grepl("39",d2) | grepl("40",d2)| grepl("41",d2)| grepl("42",d2)| grepl("43",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #303229.6

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #169885.8 # INJECT
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #133343.8 # INSULIN
# 169885.8 + 133343.8 = 303229.6

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))




# ----
# TOTAL FLOWS TO GPLs INJECT (36 - 48)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 36 & p1 < 48)

DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d1) & !grepl("40",d1) & !grepl("41",d1) & !grepl("42",d1) & !grepl("43",d1)) & 
           (grepl("39",d2) | grepl("40",d2)| grepl("41",d2)| grepl("42",d2)| grepl("43",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #202147.8

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #115717.1 # INJECT
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #86430.7 # INSULIN
# 115717.1 + 86430.7 = 202147.8

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


# ----
# TOTAL FLOWS TO GPLs INJECT (24 - 36)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 24 & p1 < 36)

DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d1) & !grepl("40",d1) & !grepl("41",d1) & !grepl("42",d1) & !grepl("43",d1)) & 
           (grepl("39",d2) | grepl("40",d2)| grepl("41",d2)| grepl("42",d2)| grepl("43",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #166858.4

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #95166.13 # INJECT
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #71692.31 # INSULIN
# 95166.13 + 71692.31 = 166858.4

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))





# ----
# TOTAL FLOWS TO GPLs INJECT (12 - 24)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 12 & p1 < 24)

DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d1) & !grepl("40",d1) & !grepl("41",d1) & !grepl("42",d1) & !grepl("43",d1)) & 
           (grepl("39",d2) | grepl("40",d2)| grepl("41",d2)| grepl("42",d2)| grepl("43",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #134483.1

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #68671.07 # INJECT
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #65812.07 # INSULIN
# 68671.07 + 65812.07 = 134483.1

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))



# ----
# TOTAL FLOWS TO GPLs INJECT (1 - 12)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 1 & p1 < 12)

DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d1) & !grepl("40",d1) & !grepl("41",d1) & !grepl("42",d1) & !grepl("43",d1)) & 
           (grepl("39",d2) | grepl("40",d2)| grepl("41",d2)| grepl("42",d2)| grepl("43",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #111837.2

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #59160.7 # INJECT
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #52676.52 # INSULIN
# 59160.7 + 52676.52 = 111837.2

DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))





library(data.table)
library(tidyverse)


# ----
# TOTAL FLOWS FROM GPLs INJECT LAST 12 PERIODS (48 - 60)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)


DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d2) & !grepl("40",d2) & !grepl("41",d2) & !grepl("42",d2) & !grepl("43",d2)) & 
           (grepl("39",d1) | grepl("40",d1)| grepl("41",d1)| grepl("42",d1)| grepl("43",d1)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #243426.9

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #143318.8 # inject
DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #100108.1 # insulin

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))

DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))








# ----
# TOTAL FLOWS FROM GPLs INJECT LAST 12 PERIODS (36 - 48)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 36 & p1 < 48)


DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d2) & !grepl("40",d2) & !grepl("41",d2) & !grepl("42",d2) & !grepl("43",d2)) & 
           (grepl("39",d1) | grepl("40",d1)| grepl("41",d1)| grepl("42",d1)| grepl("43",d1)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #169840.1

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #98473.28 # inject
DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #71366.85 # insulin

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))


# ----
# TOTAL FLOWS FROM GPLs INJECT LAST 12 PERIODS (24 - 36)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 24 & p1 < 36)


DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d2) & !grepl("40",d2) & !grepl("41",d2) & !grepl("42",d2) & !grepl("43",d2)) & 
           (grepl("39",d1) | grepl("40",d1)| grepl("41",d1)| grepl("42",d1)| grepl("43",d1)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #123582.3

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #69088.46 # inject
DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #54466.79 # insulin

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))




# ----
# TOTAL FLOWS FROM GPLs INJECT LAST 12 PERIODS (12 -24)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 12 & p1 < 24)


DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d2) & !grepl("40",d2) & !grepl("41",d2) & !grepl("42",d2) & !grepl("43",d2)) & 
           (grepl("39",d1) | grepl("40",d1)| grepl("41",d1)| grepl("42",d1)| grepl("43",d1)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #111353.9

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #57445.04 # inject
DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #53875.58 # insulin

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))

# ----
# TOTAL FLOWS FROM GPLs INJECT LAST 12 PERIODS (1 - 12)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 1 & p1 < 12)


DIA_Flows_Aux._Long <- 
  DIA_Flows_Aux._Long %>% 
  filter((!grepl("39",d2) & !grepl("40",d2) & !grepl("41",d2) & !grepl("42",d2) & !grepl("43",d2)) & 
           (grepl("39",d1) | grepl("40",d1)| grepl("41",d1)| grepl("42",d1)| grepl("43",d1)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #84803.42

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #49220.27 # inject
DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #35583.15 # insulin

DIA_Flows_Aux._Long %>% filter(s1 == "G") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s1 == "I") %>% group_by(s2) %>% summarise(sumweight = sum(as.numeric(weight)))




# ----
# SGLT2 - COMBOs constitution --------------------------------------------------------------------------

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%  select(patient, weight, month60)

DIA_Japan_Drug_Histories <- separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_SGLT2 <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "32" | month60=="33" | month60=="34" | month60=="35"  | month60=="36" |
           month60=="37") 

length(unique(Patients_SGLT2$patient)) #36567 total SGLT2

Patients_SGLT2 <- Patients_SGLT2  %>%select(patient)
Patients_SGLT2 <- Patients_SGLT2 %>% distinct()


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, month60)

Patients_SGLT2 <- Patients_SGLT2 %>% left_join(DIA_Japan_Drug_Histories)


#combos SGLT2
Patients_SGLT2_ORAL_combos <- Patients_SGLT2 %>% ungroup() %>% filter(grepl(",",month60))

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))



Patients_SGLT2_ORAL_combos <- separate_rows(Patients_SGLT2_ORAL_combos, month60, sep = ",", convert=T )

names(Patients_SGLT2_ORAL_combos)[3] <- "molecule"

Patients_SGLT2_ORAL_combos$molecule <- as.character(Patients_SGLT2_ORAL_combos$molecule)

Patients_SGLT2_ORAL_combos <- Patients_SGLT2_ORAL_combos %>% left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_class))

Patients_SGLT2_ORAL_combos <- Patients_SGLT2_ORAL_combos %>%  select(patient, weight, drug_class)

Patients_SGLT2_ORAL_combos <- Patients_SGLT2_ORAL_combos %>% distinct()

Patients_SGLT2_ORAL_combos <- Patients_SGLT2_ORAL_combos %>% group_by(patient) %>% arrange(drug_class)

Combos_weights <- Patients_SGLT2_ORAL_combos %>% select(patient, weight)

Combos_weights <- Combos_weights %>% distinct()

Patients_SGLT2_ORAL_combos <- Patients_SGLT2_ORAL_combos %>% group_by(patient) %>% summarise(drug_class = toString(drug_class))

Patients_SGLT2_ORAL_combos <- Patients_SGLT2_ORAL_combos %>% left_join(Combos_weights)

Patients_SGLT2_ORAL_combos_DRUG_CLASSES <- data.frame(Patients_SGLT2_ORAL_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n))
write.csv(Patients_SGLT2_ORAL_combos_DRUG_CLASSES, "Patients_SGLT2_ORAL_combos_DRUG_CLASSES.csv")

options(scipen = 999)

Patients_SGLT2_ORAL_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n) %>%
  mutate(drug_class= as.factor(drug_class)) %>%
  filter(n>10000)%>%
  ggplot(aes(x=n, y=reorder(drug_class,n))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="deepskyblue4" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy classes among SGLT2 patients", subtitle = "n>10000")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())




# ----
# INSIDE PLOTS WITH PENETRANCE AMONG SGLT2  ----------------------------------------------

Patients_SGLT2L_combos_DRUG_CLASSES <- read.table("Patients_SGLT2_combos_DRUG_CLASSES.csv", 
                                                  header = T, sep=",", 
                                                  colClasses = "character", stringsAsFactors = FALSE)
#459 pats

sum(as.numeric(Patients_SGLT2L_combos_DRUG_CLASSES$n)) # 2060414


Patients_SGLT2L_combos_DRUG_CLASSES <- separate_rows(Patients_SGLT2L_combos_DRUG_CLASSES, drug_class, sep = ",", convert=T )

Patients_SGLT2L_combos_DRUG_CLASSES$drug_class <- str_trim(Patients_SGLT2L_combos_DRUG_CLASSES$drug_class, side = c("left"))

Patients_SGLT2L_combos_DRUG_CLASSES %>% group_by(drug_class) %>% summarise(n = n())



Patients_SGLT2L_combos_DRUG_CLASSES %>% group_by(drug_class) %>% summarise(sumn = sum(as.numeric(n))) %>% arrange(sumn) %>%
  mutate(percentage = (sumn / 2060414)*100)%>%
  mutate(drug_class= as.factor(drug_class)) %>%
  ggplot(aes(x=reorder(drug_class,percentage), y=percentage, label=round(percentage, digits = 0))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="deepskyblue4" )+
  xlab("Class") + ylab("% Share\n")+
  ggtitle("% Share of concomitant drug therapy classes among SGLT2 patients")+
  geom_text(vjust=-1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))





# ----
# TOTAL FLOWS TO SGLT2 LAST 12 PERIODS (48 - 60)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter((!grepl("32",d1) & !grepl("33",d1) & !grepl("34",d1) & !grepl("35",d1) & !grepl("36",d1)  & !grepl("37",d1)) & 
           (grepl("32",d2) | grepl("33",d2)| grepl("34",d2)| grepl("35",d2)| grepl("36",d2) | grepl("37",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #712188.7

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% summarise(sumweight = sum(as.numeric(weight))) #621455.6 # SGLT2
DIA_Flows_Aux._Long %>% filter(s2 == "g") %>% summarise(sumweight = sum(as.numeric(weight))) #148.79 # oral GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #12784.83 # INJECT GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #77799.53 # INSULIN

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


# ----
# TOTAL FLOWS TO SGLT2 LAST 12 PERIODS (36 - 48)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 36 & p1 < 48)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter((!grepl("32",d1) & !grepl("33",d1) & !grepl("34",d1) & !grepl("35",d1) & !grepl("36",d1)  & !grepl("37",d1)) & 
           (grepl("32",d2) | grepl("33",d2)| grepl("34",d2)| grepl("35",d2)| grepl("36",d2) | grepl("37",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #678877.2

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% summarise(sumweight = sum(as.numeric(weight))) #580768.5 # SGLT2
DIA_Flows_Aux._Long %>% filter(s2 == "g") %>% summarise(sumweight = sum(as.numeric(weight))) #0 # oral GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #15418.99 # INJECT GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #82689. # INSULIN
# 580768.5 +  15418.99 + 82689. = 678876.5

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


# ----
# TOTAL FLOWS TO SGLT2 LAST 12 PERIODS (24 - 36)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 24 & p1 < 36)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter((!grepl("32",d1) & !grepl("33",d1) & !grepl("34",d1) & !grepl("35",d1) & !grepl("36",d1)  & !grepl("37",d1)) & 
           (grepl("32",d2) | grepl("33",d2)| grepl("34",d2)| grepl("35",d2)| grepl("36",d2) | grepl("37",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #615445.7

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% summarise(sumweight = sum(as.numeric(weight))) #527549.1 # SGLT2
DIA_Flows_Aux._Long %>% filter(s2 == "g") %>% summarise(sumweight = sum(as.numeric(weight))) #0 # oral GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #13129.6 # INJECT GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #74766.99 # INSULIN

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


# ----
# TOTAL FLOWS TO SGLT2 LAST 12 PERIODS (12 - 24)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 12 & p1 < 24)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter((!grepl("32",d1) & !grepl("33",d1) & !grepl("34",d1) & !grepl("35",d1) & !grepl("36",d1)  & !grepl("37",d1)) & 
           (grepl("32",d2) | grepl("33",d2)| grepl("34",d2)| grepl("35",d2)| grepl("36",d2) | grepl("37",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #554918.

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% summarise(sumweight = sum(as.numeric(weight))) #468917.6 # SGLT2
DIA_Flows_Aux._Long %>% filter(s2 == "g") %>% summarise(sumweight = sum(as.numeric(weight))) #0 # oral GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #13859.09 # INJECT GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #72141.42 # INSULIN
# 468917.6 +  13859.09 + 72141.42 = 554918.1

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))



# ----
# TOTAL FLOWS TO SGLT2 LAST 12 PERIODS (1 - 12)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 1 & p1 < 12)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter((!grepl("32",d1) & !grepl("33",d1) & !grepl("34",d1) & !grepl("35",d1) & !grepl("36",d1)  & !grepl("37",d1)) & 
           (grepl("32",d2) | grepl("33",d2)| grepl("34",d2)| grepl("35",d2)| grepl("36",d2) | grepl("37",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #383000.6

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% summarise(sumweight = sum(as.numeric(weight))) #325702 # SGLT2
DIA_Flows_Aux._Long %>% filter(s2 == "g") %>% summarise(sumweight = sum(as.numeric(weight))) #0 # oral GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "G") %>% summarise(sumweight = sum(as.numeric(weight))) #9616.59 # INJECT GLP1
DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #47681.98 # INSULIN
# 325702 +  9616.59 + 47681.98 = 554918.1

DIA_Flows_Aux._Long %>% filter(s2 == "S") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))











# ----
# INSULIN - COMBOs constitution --------------------------------------------------------------------------

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%  select(patient, weight, month60)

DIA_Japan_Drug_Histories <- separate_rows(DIA_Japan_Drug_Histories, month60, sep = ",", convert=T )

Patients_INSULIN <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  filter(month60 == "44" | month60=="45" | month60=="46" | month60=="47"  | month60=="48" |
           month60=="49" | month60=="50" | month60=="51"  | month60=="52" | month60=="53" | 
           month60=="54" | month60=="55"  | month60=="56" | month60=="57") 

length(unique(Patients_INSULIN$patient)) # 7988 total insulin

Patients_INSULIN <- Patients_INSULIN  %>%select(patient)
Patients_INSULIN <- Patients_INSULIN %>% distinct()


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, month60)

Patients_INSULIN <- Patients_INSULIN %>% left_join(DIA_Japan_Drug_Histories)


#combos insulin
Patients_INSULIN_combos <- Patients_INSULIN %>% ungroup() %>% filter(grepl(",",month60))

# IMPORT DICTIONARY WITH LABELS FOR MOLECULE, DRUG GROUP, AND INDICATION
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

# SEPERATE THE INGREDIENTS X:Y INTO 2 COLUMNS CONTAINING EACH ELEMENT/ DEGREE OF ABSTRACTION
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))



Patients_INSULIN_combos <- separate_rows(Patients_INSULIN_combos, month60, sep = ",", convert=T )

names(Patients_INSULIN_combos)[3] <- "molecule"

Patients_INSULIN_combos$molecule <- as.character(Patients_INSULIN_combos$molecule)

Patients_INSULIN_combos <- Patients_INSULIN_combos %>% left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_class))

Patients_INSULIN_combos <- Patients_INSULIN_combos %>%  select(patient, weight, drug_class)

Patients_INSULIN_combos <- Patients_INSULIN_combos %>% distinct()

Patients_INSULIN_combos <- Patients_INSULIN_combos %>% group_by(patient) %>% arrange(drug_class)

Combos_weights <- Patients_INSULIN_combos %>% select(patient, weight)

Combos_weights <- Combos_weights %>% distinct()

Patients_INSULIN_combos <- Patients_INSULIN_combos %>% group_by(patient) %>% summarise(drug_class = toString(drug_class))

Patients_INSULIN_combos <- Patients_INSULIN_combos %>% left_join(Combos_weights)

Patients_INSULIN_combos_DRUG_CLASSES <- data.frame(Patients_INSULIN_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n))
write.csv(Patients_INSULIN_combos_DRUG_CLASSES, "Patients_INSULIN_combos_DRUG_CLASSES.csv")

options(scipen = 999)

Patients_INSULIN_combos %>% group_by(drug_class) %>% summarise(n = sum(as.numeric(weight))) %>% arrange(n) %>%
  mutate(drug_class= as.factor(drug_class)) %>%
  filter(n>5000)%>%
  ggplot(aes(x=n, y=reorder(drug_class,n))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="darkseagreen" )+
  xlab("\nProjected Population") + ylab("Drug Class Combinations\n")+
  ggtitle("Constitution of concomitant drug therapy classes among INSULIN patients", subtitle = "n>5000")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())




# ----
# INSIDE PLOTS WITH PENETRANCE AMONG INSULIN  ----------------------------------------------

Patients_INSULIN_combos_DRUG_CLASSES <- read.table("Patients_INSULIN_combos_DRUG_CLASSES.csv", 
                                                   header = T, sep=",", 
                                                   colClasses = "character", stringsAsFactors = FALSE)


sum(as.numeric(Patients_INSULIN_combos_DRUG_CLASSES$n)) # 656584.5


Patients_INSULIN_combos_DRUG_CLASSES <- separate_rows(Patients_INSULIN_combos_DRUG_CLASSES, drug_class, sep = ",", convert=T )

Patients_INSULIN_combos_DRUG_CLASSES$drug_class <- str_trim(Patients_INSULIN_combos_DRUG_CLASSES$drug_class, side = c("left"))

Patients_INSULIN_combos_DRUG_CLASSES %>% group_by(drug_class) %>% summarise(n = n())

Patients_INSULIN_combos_DRUG_CLASSES %>% group_by(drug_class) %>% summarise(sumn = sum(as.numeric(n))) %>% arrange(sumn) %>%
  mutate(percentage = (sumn / 656584.5)*100)%>%
  mutate(drug_class= as.factor(drug_class)) %>%
  ggplot(aes(x=reorder(drug_class,percentage), y=percentage, label=round(percentage, digits = 0))) +
  geom_bar(stat="identity", alpha = 0.7, show.legend = FALSE, fill="darkseagreen" )+
  xlab("Class") + ylab("% Share\n")+
  ggtitle("% Share of concomitant drug therapy classes among INSULIN patients")+
  geom_text(vjust=-1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))














# ----
# TOTAL FLOWS TO INSULIN LAST 12 PERIODS (48 - 60)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)


DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(s1 != "I") %>% filter(s2 == "I")

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #1067739

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))



DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) 

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(grepl("44",d2) | grepl("45",d2) | grepl("46",d2) | grepl("47",d2) | grepl("48",d2) | grepl("49",d2) | (grepl("50",d2) | grepl("51",d2) | grepl("52",d2) | grepl("53",d2) | grepl("54",d2) | grepl("55",d2) | grepl("56",d2) | grepl("57",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #1067739

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #1067248 # INSULIN
# there is surgery

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))

# ----
# TOTAL FLOWS TO INSULIN LAST 12 PERIODS (36 - 48)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 36 & p1 < 48)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) 

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(grepl("44",d2) | grepl("45",d2) | grepl("46",d2) | grepl("47",d2) | grepl("48",d2) | grepl("49",d2) | (grepl("50",d2) | grepl("51",d2) | grepl("52",d2) | grepl("53",d2) | grepl("54",d2) | grepl("55",d2) | grepl("56",d2) | grepl("57",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #883626.9

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #883067.8 # INSULIN

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))



# ----
# TOTAL FLOWS TO INSULIN LAST 12 PERIODS (24 - 36)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 24 & p1 < 36)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) 

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(grepl("44",d2) | grepl("45",d2) | grepl("46",d2) | grepl("47",d2) | grepl("48",d2) | grepl("49",d2) | (grepl("50",d2) | grepl("51",d2) | grepl("52",d2) | grepl("53",d2) | grepl("54",d2) | grepl("55",d2) | grepl("56",d2) | grepl("57",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #797682.5

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #797414.6 # INSULIN

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


# ----
# TOTAL FLOWS TO INSULIN LAST 12 PERIODS (12 - 24)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 12 & p1 < 24)


DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) 

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(grepl("44",d2) | grepl("45",d2) | grepl("46",d2) | grepl("47",d2) | grepl("48",d2) | grepl("49",d2) | (grepl("50",d2) | grepl("51",d2) | grepl("52",d2) | grepl("53",d2) | grepl("54",d2) | grepl("55",d2) | grepl("56",d2) | grepl("57",d2)))


DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #745896.2

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #745803.2 # INSULIN

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))


# ----
# TOTAL FLOWS TO INSULIN LAST 12 PERIODS (1 - 12)----------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 1 & p1 < 12)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) 

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(grepl("44",d2) | grepl("45",d2) | grepl("46",d2) | grepl("47",d2) | grepl("48",d2) | grepl("49",d2) | (grepl("50",d2) | grepl("51",d2) | grepl("52",d2) | grepl("53",d2) | grepl("54",d2) | grepl("55",d2) | grepl("56",d2) | grepl("57",d2)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #616703

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) #616703 # INSULIN

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))



# ----
# By tipe of insulin consumption ---------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) 

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(grepl("44",d2) | grepl("45",d2) | grepl("46",d2) | grepl("47",d2) | grepl("48",d2) | grepl("49",d2) | (grepl("50",d2) | grepl("51",d2) | grepl("52",d2) | grepl("53",d2) | grepl("54",d2) | grepl("55",d2) | grepl("56",d2) | grepl("57",d2)))

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(s1 != "I") %>% filter(s2 == "I")

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #1067739

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% summarise(sumweight = sum(as.numeric(weight))) 

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(s1) %>% summarise(sumweight = sum(as.numeric(weight)))
DIA_Flows_Aux._Long$weight <- as.numeric(DIA_Flows_Aux._Long$weight)

Japan_12_months_insulin_segments <- read.csv("Danu Japan_12_months_insulin_segments.csv")
names(Japan_12_months_insulin_segments)[1] <- "patient"

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% left_join(Japan_12_months_insulin_segments, by = c("patient" = "patient", "weight" = "weight"))


DIA_Flows_Aux._Long %>% group_by(Insulin.segmentation) %>% summarise(sumweight = sum(as.numeric(weight)))

# Insulin.segmentation sumweight

DIA_Flows_Aux._Long %>% filter(s2 == "I") %>% group_by(Insulin.segmentation, s1) %>% summarise(sumweight = sum(as.numeric(weight))) %>%
  ungroup%>% group_by(Insulin.segmentation) %>% summarise(sumweight2 = sum(as.numeric(sumweight)))



# ----
# How many drugs per patient on month 60 ? ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% select(patient, weight, month60)

sum(as.numeric(DIA_Japan_Drug_Histories$weight))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(month60 != "-")

------------------------------------------------------
  
  DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p2, d2, s2)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p2 = as.numeric(p2)) %>% filter(p2 == 60)


# ----
# How many drugs per patient on month 60 ?- per CATEGORY ----------------------------------------------------------------

# CREATE VECTORS SELECTING PATIENTS FOR EACH CATEGORY


#INSULIN
INSULIN_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("44",d2) | grepl("45",d2) | grepl("46",d2) | grepl("47",d2) | grepl("48",d2) | grepl("49",d2) | (grepl("50",d2) | grepl("51",d2) | grepl("52",d2) | grepl("53",d2) | grepl("54",d2) | grepl("55",d2) | grepl("56",d2) | grepl("57",d2))) %>%
  select(patient) %>% distinct()
INSULIN_Patients <- INSULIN_Patients %>% left_join(DIA_Japan_Drug_Histories)
INSULIN_Patients <- separate_rows(INSULIN_Patients, month60, sep = ",", convert=T )
INSULIN_Patients <- INSULIN_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
INSULIN_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))


#GLP1 inject
GLP1_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("39",d2) | grepl("40",d2) | grepl("41",d2) | grepl("42",d2) | grepl("43",d2)) %>%
  select(patient) %>% distinct()
GLP1_Patients <- GLP1_Patients %>% left_join(DIA_Japan_Drug_Histories)
GLP1_Patients <- separate_rows(GLP1_Patients, month60, sep = ",", convert=T )
GLP1_Patients <- GLP1_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
GLP1_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))


#SGLT2
SGLT2_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("32",d2) | grepl("33",d2) | grepl("34",d2) | grepl("35",d2) | grepl("36",d2) | grepl("37",d2)) %>%
  select(patient) %>% distinct()
SGLT2_Patients <- SGLT2_Patients %>% left_join(DIA_Japan_Drug_Histories)
SGLT2_Patients <- separate_rows(SGLT2_Patients, month60, sep = ",", convert=T )
SGLT2_Patients <- SGLT2_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
SGLT2_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))


#DPP4
DPP4_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("23",d2) | grepl("24",d2) | grepl("25",d2) | grepl("26",d2) | grepl("27",d2) | grepl("28",d2) | grepl("29",d2) | grepl("30",d2) | grepl("31",d2)) %>%
  select(patient) %>% distinct()
DPP4_Patients <- DPP4_Patients %>% left_join(DIA_Japan_Drug_Histories)
DPP4_Patients <- separate_rows(DPP4_Patients, month60, sep = ",", convert=T )
DPP4_Patients <- DPP4_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
DPP4_Patients %>%  group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

#ANTIDIABETICS
Antidiabetics_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("(^|\\D)(8{1})(\\D|$)",d2) | grepl("(^|\\D)(9{1})(\\D|$)",d2) | grepl("10",d2) | grepl("11",d2) | grepl("12",d2) | grepl("13",d2) | grepl("14",d2) | grepl("15",d2) | grepl("16",d2)
         | grepl("17",d2) | grepl("18",d2) | grepl("19",d2) | grepl("20",d2) | grepl("21",d2) | grepl("22",d2)) %>%
  select(patient) %>% distinct()
Antidiabetics_Patients <- Antidiabetics_Patients %>% left_join(DIA_Japan_Drug_Histories)
Antidiabetics_Patients <- separate_rows(Antidiabetics_Patients, month60, sep = ",", convert=T )
Antidiabetics_Patients <- Antidiabetics_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
Antidiabetics_Patients %>%  group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))


#Biguanide (^|\\D)(1{1})(\\D|$) (^|\\D)(2{1})(\\D|$)
Biguanide_Patients <- DIA_Flows_Aux._Long %>% filter(grepl("(^|\\D)(1{1})(\\D|$)",d2) | grepl("(^|\\D)(2{1})(\\D|$)",d2)) %>% select(patient) %>% distinct()
Biguanide_Patients <- Biguanide_Patients %>% left_join(DIA_Japan_Drug_Histories)
Biguanide_Patients <- separate_rows(Biguanide_Patients, month60, sep = ",", convert=T )
Biguanide_Patients <- Biguanide_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
Biguanide_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))


# ----
# How many drugs per patient on month 60 ? ------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% select(patient, weight, month60)

sum(as.numeric(DIA_Japan_Drug_Histories$weight))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(month60 != "-")


DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p2, s2)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p2 = as.numeric(p2)) %>% filter(p2 == 60)


# ----
# per CATEGORY ----------------------------------------------------------------

# CREATE VECTORS SELECTING PATIENTS FOR EACH CATEGORY
#INSULIN
INSULIN_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("I",s2)) %>%
  select(patient) %>% distinct()
INSULIN_Patients <- INSULIN_Patients %>% left_join(DIA_Japan_Drug_Histories)
INSULIN_Patients <- separate_rows(INSULIN_Patients, month60, sep = ",", convert=T )
INSULIN_Patients <- INSULIN_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
INSULIN_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))


#GLP1 inject
GLP1_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("G",s2)) %>%
  select(patient) %>% distinct()
GLP1_Patients <- GLP1_Patients %>% left_join(DIA_Japan_Drug_Histories)
GLP1_Patients <- separate_rows(GLP1_Patients, month60, sep = ",", convert=T )
GLP1_Patients <- GLP1_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
GLP1_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))


#SGLT2
SGLT2_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("S",s2)) %>%
  select(patient) %>% distinct()
SGLT2_Patients <- SGLT2_Patients %>% left_join(DIA_Japan_Drug_Histories)
SGLT2_Patients <- separate_rows(SGLT2_Patients, month60, sep = ",", convert=T )
SGLT2_Patients <- SGLT2_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
SGLT2_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))



#DPP4
DPP4_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("D",s2)) %>%
  select(patient) %>% distinct()
DPP4_Patients <- DPP4_Patients %>% left_join(DIA_Japan_Drug_Histories)
DPP4_Patients <- separate_rows(DPP4_Patients, month60, sep = ",", convert=T )
DPP4_Patients <- DPP4_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
DPP4_Patients %>%  group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

#ANTIDIABETICS
Antidiabetics_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("d",s2)) %>%
  select(patient) %>% distinct()
Antidiabetics_Patients <- Antidiabetics_Patients %>% left_join(DIA_Japan_Drug_Histories)
Antidiabetics_Patients <- separate_rows(Antidiabetics_Patients, month60, sep = ",", convert=T )
Antidiabetics_Patients <- Antidiabetics_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
Antidiabetics_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

#Biguanide
Biguanide_Patients <- DIA_Flows_Aux._Long %>% filter(grepl("b",s2)) %>% select(patient) %>% distinct()
Biguanide_Patients <- Biguanide_Patients %>% left_join(DIA_Japan_Drug_Histories)
Biguanide_Patients <- separate_rows(Biguanide_Patients, month60, sep = ",", convert=T )
Biguanide_Patients <- Biguanide_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
Biguanide_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

# 1 418863
















# ----
# How many different molecules in the past year ? ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% select(patient, weight, month48:month60)

sum(as.numeric(DIA_Japan_Drug_Histories$weight))



DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p2, s2)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p2 = as.numeric(p2)) %>% filter(p2 == 60)


# CREATE VECTORS SELECTING PATIENTS FOR EACH CATEGORY
#INSULIN
INSULIN_Patients <- DIA_Flows_Aux._Long %>% filter(grepl("I",s2)) %>% select(patient) %>% distinct()
INSULIN_Patients <- INSULIN_Patients %>% left_join(DIA_Japan_Drug_Histories)
INSULIN_Patients <- gather(INSULIN_Patients, Month, Treat, month48:month60, factor_key=TRUE)
INSULIN_Patients <- separate_rows(INSULIN_Patients, Treat, sep = ",", convert=T )
INSULIN_Patients <- INSULIN_Patients %>% select(-c(Month)) %>% filter(Treat != "-")
INSULIN_Patients <- INSULIN_Patients %>% distinct()
INSULIN_Patients <- INSULIN_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
INSULIN_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

weighted.mean(INSULIN_Patients$n, as.numeric(INSULIN_Patients$weight)) #4.049298

#GLP1 inject
GLP1_Patients <- DIA_Flows_Aux._Long %>%  filter(grepl("G",s2)) %>% select(patient) %>% distinct()
GLP1_Patients <- GLP1_Patients %>% left_join(DIA_Japan_Drug_Histories)
GLP1_Patients <- gather(GLP1_Patients, Month, Treat, month48:month60, factor_key=TRUE)
GLP1_Patients <- separate_rows(GLP1_Patients, Treat, sep = ",", convert=T )
GLP1_Patients <- GLP1_Patients %>% select(-c(Month)) %>% filter(Treat != "-")
GLP1_Patients <- GLP1_Patients %>% distinct()
GLP1_Patients <- GLP1_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
GLP1_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

weighted.mean(GLP1_Patients$n, as.numeric(GLP1_Patients$weight)) #4.050561


#SGLT2
SGLT2_Patients <- DIA_Flows_Aux._Long %>% filter(grepl("S",s2)) %>% select(patient) %>% distinct()
SGLT2_Patients <- SGLT2_Patients %>% left_join(DIA_Japan_Drug_Histories)
SGLT2_Patients <- gather(SGLT2_Patients, Month, Treat, month48:month60, factor_key=TRUE)
SGLT2_Patients <- separate_rows(SGLT2_Patients, Treat, sep = ",", convert=T )
SGLT2_Patients <- SGLT2_Patients %>% select(-c(Month)) %>% filter(Treat != "-")
SGLT2_Patients <- SGLT2_Patients %>% distinct()
SGLT2_Patients <- SGLT2_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
SGLT2_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

weighted.mean(SGLT2_Patients$n, as.numeric(SGLT2_Patients$weight)) #3.116779

#DPP4
DPP4_Patients <- DIA_Flows_Aux._Long %>% filter(grepl("D",s2)) %>% select(patient) %>% distinct()
DPP4_Patients <- DPP4_Patients %>% left_join(DIA_Japan_Drug_Histories)
DPP4_Patients <- gather(DPP4_Patients, Month, Treat, month48:month60, factor_key=TRUE)
DPP4_Patients <- separate_rows(DPP4_Patients, Treat, sep = ",", convert=T )
DPP4_Patients <- DPP4_Patients %>% select(-c(Month)) %>% filter(Treat != "-")
DPP4_Patients <- DPP4_Patients %>% distinct()
DPP4_Patients <- DPP4_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
DPP4_Patients %>%  group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

weighted.mean(DPP4_Patients$n, as.numeric(DPP4_Patients$weight)) #2.124595



#ANTIDIABETICS
Antidiabetics_Patients <- DIA_Flows_Aux._Long %>% filter(grepl("d",s2)) %>% select(patient) %>% distinct()
Antidiabetics_Patients <- Antidiabetics_Patients %>% left_join(DIA_Japan_Drug_Histories)
Antidiabetics_Patients <- gather(Antidiabetics_Patients, Month, Treat, month48:month60, factor_key=TRUE)
Antidiabetics_Patients <- separate_rows(Antidiabetics_Patients, Treat, sep = ",", convert=T )
Antidiabetics_Patients <- Antidiabetics_Patients %>% select(-c(Month)) %>% filter(Treat != "-")
Antidiabetics_Patients <- Antidiabetics_Patients %>% distinct()
Antidiabetics_Patients <- Antidiabetics_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
Antidiabetics_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

weighted.mean(Antidiabetics_Patients$n, as.numeric(Antidiabetics_Patients$weight)) #1.790452



#Biguanide
Biguanide_Patients <- DIA_Flows_Aux._Long %>% filter(grepl("b",s2)) %>% select(patient) %>% distinct()
Biguanide_Patients <- Biguanide_Patients %>% left_join(DIA_Japan_Drug_Histories)
Biguanide_Patients <- gather(Biguanide_Patients, Month, Treat, month48:month60, factor_key=TRUE)
Biguanide_Patients <- separate_rows(Biguanide_Patients, Treat, sep = ",", convert=T )
Biguanide_Patients <- Biguanide_Patients %>% select(-c(Month)) %>% filter(Treat != "-")
Biguanide_Patients <- Biguanide_Patients %>% distinct()
Biguanide_Patients <- Biguanide_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
Biguanide_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

weighted.mean(Biguanide_Patients$n, as.numeric(Biguanide_Patients$weight)) #1.184747



#Lapsed
Lapsed_Patients <- DIA_Flows_Aux._Long %>% filter(grepl("x",s2)) %>% select(patient) %>% distinct()
Lapsed_Patients <- Lapsed_Patients %>% left_join(DIA_Japan_Drug_Histories)
Lapsed_Patients <- gather(Lapsed_Patients, Month, Treat, month48:month60, factor_key=TRUE)
Lapsed_Patients <- separate_rows(Lapsed_Patients, Treat, sep = ",", convert=T )
Lapsed_Patients <- Lapsed_Patients %>% select(-c(Month)) %>% filter(Treat != "-")
Lapsed_Patients <- Lapsed_Patients %>% distinct()
Lapsed_Patients <- Lapsed_Patients %>% group_by(patient, weight) %>% summarise(n = n()) 
Lapsed_Patients %>% group_by(n) %>% summarise(n2 = sum(as.numeric(weight)))

weighted.mean(Lapsed_Patients$n, as.numeric(Lapsed_Patients$weight)) #1.538142


  
  
# ----
# DURATION ON INSULIN PATS STARTING INSULIN BETWEEN 12-24MONTHS ----------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

Patients_no_insulin_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) %>%
  select(patient)

Patients_no_insulin_12 <- Patients_no_insulin_12 %>% distinct()

Patients_start_insulin_12_24 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 24) %>%
  filter(grepl("44",d1) | grepl("45",d1) | grepl("46",d1) | grepl("47",d1) | grepl("48",d1) | grepl("49",d1) | (grepl("50",d1) | grepl("51",d1) | grepl("52",d1) | grepl("53",d1) | grepl("54",d1) | grepl("55",d1) | grepl("56",d1) | grepl("57",d1))) %>%
  select(patient)

Patients_start_insulin_12_24 <- Patients_start_insulin_12_24 %>% distinct()
Patients_Insulin_track <- Patients_no_insulin_12 %>% inner_join(Patients_start_insulin_12_24)


# Duration ON Insulin FOR THOSE 12-24 PATS
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(27:63)

# convert no insuilins too zero, and insulins to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- Patients_Insulin_track %>% left_join(DIA_Japan_Drug_Histories)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month24:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, how long it remains on the same line, 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% group_by(patient) %>% mutate(sumDuration = sum(Duration))

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% select(patient, sumDuration) %>% distinct()

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) %>% select(patient, sumDuration, weight) %>% distinct()

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% mutate(weight = as.numeric(weight))

Insulin_Periods_DIA <- setDT(expandRows(Insulin_Periods_DIA, "weight"))[]

data.frame(Insulin_Periods_DIA %>% group_by(sumDuration) %>% summarise(n = n()))

# For each individual period, the very first one only 
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(27:63)

# convert no insuilins too zero, and insulins to one, convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- Patients_Insulin_track %>% left_join(DIA_Japan_Drug_Histories)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month24:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of this lapsed periods!
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% group_by(patient) %>% slice(which.min(grp))

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% select(patient, Duration) %>% distinct()

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) %>% select(patient, Duration, weight) %>% distinct()

Insulin_Periods_DIA <- Insulin_Periods_DIA %>%mutate(weight = as.numeric(weight))

Insulin_Periods_DIA <- setDT(expandRows(Insulin_Periods_DIA, "weight"))[]

data.frame(Insulin_Periods_DIA %>% group_by(Duration) %>% summarise(n = n()))


# ----
# DURATIONS LAPSED PERIODS INSULIN PATS STARTING INSULIN BETWEEN 12-24MONTHS ----------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

Patients_no_insulin_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) %>%
  select(patient)

Patients_no_insulin_12 <- Patients_no_insulin_12 %>% distinct()

Patients_start_insulin_12_24 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 24) %>%
  filter(grepl("44",d1) | grepl("45",d1) | grepl("46",d1) | grepl("47",d1) | grepl("48",d1) | grepl("49",d1) | (grepl("50",d1) | grepl("51",d1) | grepl("52",d1) | grepl("53",d1) | grepl("54",d1) | grepl("55",d1) | grepl("56",d1) | grepl("57",d1))) %>%
  select(patient)

Patients_start_insulin_12_24 <- Patients_start_insulin_12_24 %>% distinct()
Patients_Insulin_track <- Patients_no_insulin_12 %>% inner_join(Patients_start_insulin_12_24)


# Duration ON Insulin FOR THOSE 12-24 PATS 
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(27:63)

# convert no insuilins too zero, and insulins to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- Patients_Insulin_track %>% left_join(DIA_Japan_Drug_Histories)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month24:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, how long it remains on the same line, 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})


# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 0)

# count (how many months) in each of this lapsed periods!
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"

#Insulin_Periods_DIA <- Insulin_Periods_DIA %>% group_by(patient) %>% slice(which.min(grp))

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% select(patient, Duration)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) %>% select(patient, Duration, weight) 

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% mutate(weight = as.numeric(weight))

Insulin_Periods_DIA <- setDT(expandRows(Insulin_Periods_DIA, "weight"))[]

data.frame(Insulin_Periods_DIA %>% group_by(Duration) %>% summarise(n = sum(weight)))















# ----
# LAG PERIODS BETWEEN INSULIN PRESCRIPTIONS --------------------------------------------------------------------
library(data.table)
library(tidyverse)

DIA_Japan_Doses_v2 <- read.table("DIA Japan Doses_v2.txt", 
                                 header = T, sep=",", 
                                 colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Doses_v2 <- DIA_Japan_Doses_v2 %>% filter(grepl("Insulin",drug_class))

data.frame(DIA_Japan_Doses_v2 %>% group_by(dayssup) %>% summarise(n =n()))

DIA_Japan_Doses_v2 <- DIA_Japan_Doses_v2 %>%mutate(from_dt = as.Date(from_dt))
DIA_Japan_Doses_v2 <- DIA_Japan_Doses_v2 %>% arrange(pat_id, from_dt)
DIA_Japan_Doses_v2 <- DIA_Japan_Doses_v2 %>% group_by(pat_id) %>% mutate(diff = from_dt - lag(from_dt))
write.csv(DIA_Japan_Doses_v2, "DIA_Japan_Doses_v2_Insulin_Lag_Dates.csv")





# ----
# HbA1c at time last seen, with stock -----------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

names(HbA1cHist)[1] <- "patient"
HbA1cHist %>% select(X60) %>% group_by(X60) %>% summarise(n=n())


HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )

HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))

HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)

HbA1cHist_month <- HbA1cHist %>% select(-HbA1c)

HbA1cHist_month <- HbA1cHist_month %>% group_by(patient) %>% summarize(across(everything(), max))

HbA1cHist_month <- HbA1cHist_month %>% left_join(HbA1cHist)

HbA1cHist_month <- HbA1cHist_month %>% group_by(patient) %>% summarize(across(everything(), max))

HbA1cHist_month <- HbA1cHist_month %>% select(-weight)



# Files with stocks/boxes, long format
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p2, d2, s2)

names(DIA_Flows_Aux._Long)[3] <- "Month" 

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(Month = as.numeric(Month))

HbA1cHist_month <- HbA1cHist_month %>% inner_join(DIA_Flows_Aux._Long)

HbA1cHist_month$HbA1c <- as.numeric(HbA1cHist_month$HbA1c)

HbA1cHist_month %>% group_by(s2) %>% summarise(n = mean(HbA1c, na.rm = T)) %>% arrange(n)


write.csv(HbA1cHist_month, "HbA1cHist_last_month_stocks.csv")

HbA1cHist_month %>%
  filter(HbA1c >4 & HbA1c <10) %>%
  group_by(s2) %>% 
  ggplot(aes(x=reorder(s2,HbA1c), y =HbA1c, fill=s2, colour=s2))+
  geom_point(position = "jitter",alpha=0.2, show.legend = F)+
  geom_violin(alpha=0.7, position = position_dodge(width = .55),lwd=0.2,show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  xlab("Stock")+
  ylab("HbA1c")





# ----
# HbA1c reductions, Insulin -----------------------
# No insulin first 12months
#started between 12 and 48months, follow from 12 to 60 

library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no insulin before month 12
Patients_insulin_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(grepl("44",d1) | grepl("45",d1) | grepl("46",d1) | grepl("47",d1) | grepl("48",d1)|grepl("49",d1) | grepl("50",d1) | 
           grepl("51",d1) | grepl("52",d1) | grepl("53",d1) | grepl("54",d1)  | grepl("55",d1) | grepl("56",d1)  |grepl("57",d1)) %>%
  select(patient)

Patients_no_insulin_12 <- DIA_Flows_Aux._Long %>% filter(p1<12) %>% anti_join(Patients_insulin_12) %>% select(patient)

# vector of unique patients
Patients_no_insulin_12 <- Patients_no_insulin_12 %>% distinct()

# filter for patient who did have an insulin from 12 to 48
Patients_start_insulin_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("I",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_insulin_12_48 <- Patients_start_insulin_12_48 %>% distinct()

# select patient intersection 
Patients_Insulin_track <- Patients_no_insulin_12 %>% inner_join(Patients_start_insulin_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on insulin status
DIA_Japan_Drug_Histories <- Patients_Insulin_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsnON insulin
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to insulin
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took Insulin
DIA_Japan_Drug_Histories_FIRST_INSULIN <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_INSULIN)[3] <- "Month_First_Insulin"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the insulin criteria
Patient_first_insulin <-  DIA_Japan_Drug_Histories_FIRST_INSULIN %>% select(patient)
HbA1cHist <- Patient_first_insulin %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_INSULIN$Month_First_Insulin <- as.character(DIA_Japan_Drug_Histories_FIRST_INSULIN$Month_First_Insulin)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first insulin month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_INSULIN, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_Insulin <- as.numeric(HbA1cHist$Month_First_Insulin)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before insulin start and months after insulin start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_insulin <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_Insulin)
HbA1cs_before_insulin <- HbA1cs_before_insulin %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_insulin)[2] <- "Month_Prior"
names(HbA1cs_before_insulin)[3] <- "HbA1c_Prior"
names(HbA1cs_before_insulin)[4] <- "weight"

HbA1cs_after_insulin <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_Insulin)
HbA1cs_after_insulin <- HbA1cs_after_insulin %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_insulin)[2] <- "Month_After"
names(HbA1cs_after_insulin)[3] <- "HbA1c_After"
names(HbA1cs_after_insulin)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_insulin_BEFORE_vs_AFTER <-HbA1cs_before_insulin %>% full_join(HbA1cs_after_insulin)

write.csv(HbA1cs_before_insulin_BEFORE_vs_AFTER, "HbA1cs_before_insulin_BEFORE_vs_AFTER.csv")

# calculate n of months until insulin and after insulin until next HbA1c reading
HbA1cs_before_insulin_BEFORE_vs_AFTER <- HbA1cs_before_insulin_BEFORE_vs_AFTER %>% mutate(Months_until_I = Month_First_Insulin - Month_Prior) %>% mutate(Month_from_I_on = Month_After - Month_First_Insulin)
#percentage reduction
HbA1cs_before_insulin_BEFORE_vs_AFTER <- HbA1cs_before_insulin_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
HbA1cs_before_insulin_BEFORE_vs_AFTER %>% 
  select(HbA1c_reduction, weight) %>% 
  arrange(HbA1c_reduction) %>%
  mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                             ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                    ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                           ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                  ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                         ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                       ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))



# mean HbA1cs before and after
HbA1cs_before_insulin_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))

# density distribution before and after therapy
HbA1cs_before_insulin_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")



# ----
# HbA1c reductions, GLP1 inject ------------------
# No GLP1 inject first 12months, started between 12 and 48months, follow from 12 to 60 
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))



#M001059516
#filter for patients who had no GLP1 inject before month 12
Patients_GLP1_inject_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(grepl("39",d1) | grepl("40",d1) | grepl("41",d1) | grepl("42",d1)  |grepl("43",d1)) %>%
  select(patient)

Patients_no_GLP1_inject_12 <- DIA_Flows_Aux._Long %>% filter(p1<12) %>% anti_join(Patients_GLP1_inject_12) %>% select(patient)

# vector of unique patients
Patients_no_GLP1_inject_12 <- Patients_no_GLP1_inject_12 %>% distinct()

# filter for patient who did have an GLP1 inject from 12 to 48
Patients_start_GLP1_inject_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("G",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_GLP1_inject_12_48 <- Patients_start_GLP1_inject_12_48 %>% distinct()

# select patient intersection 
Patients_GLP1_inject_track <- Patients_no_GLP1_inject_12 %>% inner_join(Patients_start_GLP1_inject_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "GLP1_Inject"))%>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "GLP1_Inject"))%>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "GLP1_Inject"))%>% 
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "GLP1_Inject"))%>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "GLP1_Inject"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1_Inject",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on GLP1 inject status
DIA_Japan_Drug_Histories <- Patients_GLP1_inject_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON GLP1 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to GLP1 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took GLP1 inject
DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT)[3] <- "Month_First_GLP1_Inject"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the GLP1 Inject criteria
Patient_first_GLP1_Inject <-  DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT %>% select(patient)
HbA1cHist <- Patient_first_GLP1_Inject %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT$Month_First_GLP1_Inject <- as.character(DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT$Month_First_GLP1_Inject)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first insulin month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_GLP1_Inject <- as.numeric(HbA1cHist$Month_First_GLP1_Inject)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before GLP1 inject start and months after GLP1 inject start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_GLP1_Inject <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_GLP1_Inject)
HbA1cs_before_GLP1_Inject <- HbA1cs_before_GLP1_Inject %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_GLP1_Inject)[2] <- "Month_Prior"
names(HbA1cs_before_GLP1_Inject)[3] <- "HbA1c_Prior"
names(HbA1cs_before_GLP1_Inject)[4] <- "weight"

HbA1cs_after_GLP1_Inject <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_GLP1_Inject)
HbA1cs_after_GLP1_Inject <- HbA1cs_after_GLP1_Inject %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_GLP1_Inject)[2] <- "Month_After"
names(HbA1cs_after_GLP1_Inject)[3] <- "HbA1c_After"
names(HbA1cs_after_GLP1_Inject)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER <-HbA1cs_before_GLP1_Inject %>% full_join(HbA1cs_after_GLP1_Inject)

write.csv(HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER, "HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER.csv")

# calculate n of months until GLP1 inject and after GLP1 inject until next HbA1c reading
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER <- HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% mutate(Months_until_G = Month_First_GLP1_Inject - Month_Prior) %>% mutate(Month_from_G_on = Month_After - Month_First_GLP1_Inject)
#percentage reduction
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER <- HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
data.frame(HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% 
             select(HbA1c_reduction, weight) %>% 
             arrange(HbA1c_reduction) %>%
             mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                                        ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                               ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                                      ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                             ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                                    ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                           ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                                  ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction)))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))



# mean HbA1cs before and after
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))

# density distribution before and after therapy
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")








# ----
# HbA1c reductions SGLT2 ---------------------------------------------
# No SGLT2 inject first 12months, started between 12 and 48months, follow from 12 to 60
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no SGLT2 inject before month 12
Patients_SGLT2_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(grepl("32",d1) | grepl("33",d1) | grepl("34",d1) | grepl("35",d1) | grepl("36",d1)  | grepl("37",d1)) %>%
  select(patient)

Patients_no_SGLT2_12 <- DIA_Flows_Aux._Long %>% filter(p1<12) %>% anti_join(Patients_SGLT2_12) %>% select(patient)

# vector of unique patients
Patients_no_SGLT2_12 <- Patients_no_SGLT2_12 %>% distinct()

# filter for patient who did have an SGLT2 inject from 12 to 48
Patients_start_SGLT2_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("S",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_SGLT2_12_48 <- Patients_start_SGLT2_12_48 %>% distinct()

# select patient intersection 
Patients_SGLT2_track <- Patients_no_SGLT2_12 %>% inner_join(Patients_start_SGLT2_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "SGLT2"))%>% 
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "SGLT2"))%>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "SGLT2"))%>% 
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "SGLT2"))%>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "SGLT2"))%>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "SGLT2"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SGLT2",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on SGLT2 inject status
DIA_Japan_Drug_Histories <- Patients_SGLT2_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON SGLT2 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to SGLT2 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took SGLT2 inject
DIA_Japan_Drug_Histories_FIRST_SGLT2 <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_SGLT2)[3] <- "Month_First_SGLT2"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the SGLT2 Inject criteria
Patient_first_SGLT2 <-  DIA_Japan_Drug_Histories_FIRST_SGLT2 %>% select(patient)
HbA1cHist <- Patient_first_SGLT2 %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_SGLT2$Month_First_SGLT2 <- as.character(DIA_Japan_Drug_Histories_FIRST_SGLT2$Month_First_SGLT2)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first SGLT2 month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_SGLT2, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_SGLT2 <- as.numeric(HbA1cHist$Month_First_SGLT2)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before SGLT2 start and months after SGLT2 start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_SGLT2 <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_SGLT2)
HbA1cs_before_SGLT2 <- HbA1cs_before_SGLT2 %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_SGLT2)[2] <- "Month_Prior"
names(HbA1cs_before_SGLT2)[3] <- "HbA1c_Prior"
names(HbA1cs_before_SGLT2)[4] <- "weight"

HbA1cs_after_SGLT2 <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_SGLT2)
HbA1cs_after_SGLT2 <- HbA1cs_after_SGLT2 %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_SGLT2)[2] <- "Month_After"
names(HbA1cs_after_SGLT2)[3] <- "HbA1c_After"
names(HbA1cs_after_SGLT2)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_SGLT2_BEFORE_vs_AFTER <-HbA1cs_before_SGLT2 %>% full_join(HbA1cs_after_SGLT2)

write.csv(HbA1cs_before_SGLT2_BEFORE_vs_AFTER, "HbA1cs_before_SGLT2_BEFORE_vs_AFTER.csv")

# calculate n of months until SGLT2 and after SGLT2 until next HbA1c reading
HbA1cs_before_SGLT2_BEFORE_vs_AFTER <- HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% mutate(Months_until_S = Month_First_SGLT2 - Month_Prior) %>% mutate(Month_from_S_on = Month_After - Month_First_SGLT2)
#percentage reduction
HbA1cs_before_SGLT2_BEFORE_vs_AFTER <- HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% 
  select(HbA1c_reduction, weight) %>% 
  arrange(HbA1c_reduction) %>%
  mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                             ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                    ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                           ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                  ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                         ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                       ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))


# mean HbA1cs before and after
HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))


# density distribution before and after therapy
HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")












# ----
# HbA1c DPP4 reductions ----------------------------------------------------
# No DPP4 inject first 12months, started between 12 and 48months, follow from 12 to 60 
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))


#filter for patients who had no DPP4 inject before month 12
Patients_DPP4_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(grepl("23",d1)|grepl("24",d1)|grepl("25",d1)|grepl("26",d1)|grepl("27",d1)|grepl("28",d1)|grepl("29",d1)|grepl("30",d1)|grepl("31",d1)) %>%
  select(patient)

Patients_no_DPP4_12 <- DIA_Flows_Aux._Long %>% filter(p1<12) %>% anti_join(Patients_DPP4_12) %>% select(patient)


# vector of unique patients
Patients_no_DPP4_12 <- Patients_no_DPP4_12 %>% distinct()

# filter for patient who did have an DPP4 inject from 12 to 48
Patients_start_DPP4_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("D",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_DPP4_12_48 <- Patients_start_DPP4_12_48 %>% distinct()

# select patient intersection 
Patients_DPP4_track <- Patients_no_DPP4_12 %>% inner_join(Patients_start_DPP4_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "DPP4"))%>% 
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "DPP4"))%>% 
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "DPP4"))%>% 
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "DPP4"))%>%
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "DPP4"))%>%
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "DPP4"))%>%
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "DPP4"))%>%
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "DPP4"))%>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "DPP4"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="DPP4",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on DPP4 inject status
DIA_Japan_Drug_Histories <- Patients_DPP4_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON DPP4 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to DPP4 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took DPP4 inject
DIA_Japan_Drug_Histories_FIRST_DPP4 <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_DPP4)[3] <- "Month_First_DPP4"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the DPP4 Inject criteria
Patient_first_DPP4 <-  DIA_Japan_Drug_Histories_FIRST_DPP4 %>% select(patient)
HbA1cHist <- Patient_first_DPP4 %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_DPP4$Month_First_DPP4 <- as.character(DIA_Japan_Drug_Histories_FIRST_DPP4$Month_First_DPP4)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first DPP4 month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_DPP4, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_DPP4 <- as.numeric(HbA1cHist$Month_First_DPP4)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before DPP4 start and months after DPP4 start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_DPP4 <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_DPP4)
HbA1cs_before_DPP4 <- HbA1cs_before_DPP4 %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_DPP4)[2] <- "Month_Prior"
names(HbA1cs_before_DPP4)[3] <- "HbA1c_Prior"
names(HbA1cs_before_DPP4)[4] <- "weight"

HbA1cs_after_DPP4 <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_DPP4)
HbA1cs_after_DPP4 <- HbA1cs_after_DPP4 %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_DPP4)[2] <- "Month_After"
names(HbA1cs_after_DPP4)[3] <- "HbA1c_After"
names(HbA1cs_after_DPP4)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_DPP4_BEFORE_vs_AFTER <-HbA1cs_before_DPP4 %>% full_join(HbA1cs_after_DPP4)

write.csv(HbA1cs_before_DPP4_BEFORE_vs_AFTER, "HbA1cs_before_DPP4_BEFORE_vs_AFTER.csv")

# calculate n of months until DPP4 and after DPP4 until next HbA1c reading
HbA1cs_before_DPP4_BEFORE_vs_AFTER <- HbA1cs_before_DPP4_BEFORE_vs_AFTER %>% mutate(Months_until_S = Month_First_DPP4 - Month_Prior) %>% mutate(Month_from_S_on = Month_After - Month_First_DPP4)
#percentage reduction
HbA1cs_before_DPP4_BEFORE_vs_AFTER <- HbA1cs_before_DPP4_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
HbA1cs_before_DPP4_BEFORE_vs_AFTER %>% 
  select(HbA1c_reduction, weight) %>% 
  arrange(HbA1c_reduction) %>%
  mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                             ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                    ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                           ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                  ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                         ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                       ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))


# mean HbA1cs before and after
HbA1cs_before_DPP4_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))



# density distribution before and after therapy
HbA1cs_before_DPP4_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")










# ----
# HbA1c Antidiabetics reduction -------------------------------------------------
# No Antidiabetic inject first 12months, started between 12 and 48months, follow from 12 to 60
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))


#filter for patients who had no Antidiabetic inject before month 12
Patients_Antidiabetic_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(grepl("(^|\\D)(8{1})(\\D|$)",d1)|grepl("(^|\\D)(9{1})(\\D|$)",d1) |grepl("(^|\\D)(10{1})(\\D|$)",d1)|grepl("(^|\\D)(11{1})(\\D|$)",d1)|grepl("(^|\\D)(12{1})(\\D|$)",d1)|grepl("(^|\\D)(13{1})(\\D|$)",d1)|grepl("(^|\\D)(14{1})(\\D|$)",d1)|grepl("(^|\\D)(15{1})(\\D|$)",d1)|grepl("(^|\\D)(16{1})(\\D|$)",d1)|grepl("(^|\\D)(17{1})(\\D|$)",d1)|grepl("(^|\\D)(18{1})(\\D|$)",d1)|grepl("(^|\\D)(19{1})(\\D|$)",d1)|grepl("(^|\\D)(20{1})(\\D|$)",d1)|grepl("(^|\\D)(21{1})(\\D|$)",d1)|grepl("(^|\\D)(22{1})(\\D|$)",d1)) %>%
  select(patient)

Patients_no_Antidiabetic_12 <- DIA_Flows_Aux._Long %>% filter(p1<12) %>% anti_join(Patients_Antidiabetic_12) %>% select(patient)


# vector of unique patients
Patients_no_Antidiabetic_12 <- Patients_no_Antidiabetic_12 %>% distinct()

# filter for patient who did have an Antidiabetic inject from 12 to 48
Patients_start_Antidiabetic_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("d",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_Antidiabetic_12_48 <- Patients_start_Antidiabetic_12_48 %>% distinct()

# select patient intersection 
Patients_Antidiabetic_track <- Patients_no_Antidiabetic_12 %>% inner_join(Patients_start_Antidiabetic_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(17{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "Antidiabetic"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antidiabetic",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on Antidiabetic inject status
DIA_Japan_Drug_Histories <- Patients_Antidiabetic_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON Antidiabetic inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Antidiabetic inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took Antidiabetic inject
DIA_Japan_Drug_Histories_FIRST_Antidiabetic <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_Antidiabetic)[3] <- "Month_First_Antidiabetic"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the Antidiabetic Inject criteria
Patient_first_Antidiabetic <-  DIA_Japan_Drug_Histories_FIRST_Antidiabetic %>% select(patient)
HbA1cHist <- Patient_first_Antidiabetic %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_Antidiabetic$Month_First_Antidiabetic <- as.character(DIA_Japan_Drug_Histories_FIRST_Antidiabetic$Month_First_Antidiabetic)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first Antidiabetic month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_Antidiabetic, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_Antidiabetic <- as.numeric(HbA1cHist$Month_First_Antidiabetic)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before Antidiabetic start and months after Antidiabetic start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_Antidiabetic <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_Antidiabetic)
HbA1cs_before_Antidiabetic <- HbA1cs_before_Antidiabetic %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_Antidiabetic)[2] <- "Month_Prior"
names(HbA1cs_before_Antidiabetic)[3] <- "HbA1c_Prior"
names(HbA1cs_before_Antidiabetic)[4] <- "weight"

HbA1cs_after_Antidiabetic <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_Antidiabetic)
HbA1cs_after_Antidiabetic <- HbA1cs_after_Antidiabetic %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_Antidiabetic)[2] <- "Month_After"
names(HbA1cs_after_Antidiabetic)[3] <- "HbA1c_After"
names(HbA1cs_after_Antidiabetic)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER <-HbA1cs_before_Antidiabetic %>% full_join(HbA1cs_after_Antidiabetic)

write.csv(HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER, "HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER.csv")

# calculate n of months until Antidiabetic and after Antidiabetic until next HbA1c reading
HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER <- HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER %>% mutate(Months_until_S = Month_First_Antidiabetic - Month_Prior) %>% mutate(Month_from_S_on = Month_After - Month_First_Antidiabetic)
#percentage reduction
HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER <- HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER %>% 
  select(HbA1c_reduction, weight) %>% 
  arrange(HbA1c_reduction) %>%
  mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                             ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                    ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                           ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                  ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                         ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                       ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))


# mean HbA1cs before and after
HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))



# density distribution before and after therapy
HbA1cs_before_Antidiabetic_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")










# ----
# HbA1c reduction Biguanide ------------------------------------
# No Biguanide inject first 12months, started between 12 and 48months, follow from 12 to 60 
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no Biguanide inject before month 12
Patients_Biguanide_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(grepl("(^|\\D)(2{1})(\\D|$)",d1)|grepl("(^|\\D)(1{1})(\\D|$)",d1)) %>%
  select(patient)

Patients_no_Biguanide_12 <- DIA_Flows_Aux._Long %>% filter(p1<12) %>% anti_join(Patients_Biguanide_12) %>% select(patient)

# vector of unique patients
Patients_no_Biguanide_12 <- Patients_no_Biguanide_12 %>% distinct()

# filter for patient who did have an Biguanide inject from 12 to 48
Patients_start_Biguanide_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("b",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_Biguanide_12_48 <- Patients_start_Biguanide_12_48 %>% distinct()

# select patient intersection 
Patients_Biguanide_track <- Patients_no_Biguanide_12 %>% inner_join(Patients_start_Biguanide_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "Biguanide"))%>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "Biguanide"))



DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Biguanide",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on Biguanide inject status
DIA_Japan_Drug_Histories <- Patients_Biguanide_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON Biguanide inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Biguanide inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took Biguanide inject
DIA_Japan_Drug_Histories_FIRST_Biguanide <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_Biguanide)[3] <- "Month_First_Biguanide"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the Biguanide Inject criteria
Patient_first_Biguanide <-  DIA_Japan_Drug_Histories_FIRST_Biguanide %>% select(patient)
HbA1cHist <- Patient_first_Biguanide %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_Biguanide$Month_First_Biguanide <- as.character(DIA_Japan_Drug_Histories_FIRST_Biguanide$Month_First_Biguanide)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first Biguanide month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_Biguanide, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_Biguanide <- as.numeric(HbA1cHist$Month_First_Biguanide)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before Biguanide start and months after Biguanide start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_Biguanide <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_Biguanide)
HbA1cs_before_Biguanide <- HbA1cs_before_Biguanide %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_Biguanide)[2] <- "Month_Prior"
names(HbA1cs_before_Biguanide)[3] <- "HbA1c_Prior"
names(HbA1cs_before_Biguanide)[4] <- "weight"

HbA1cs_after_Biguanide <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_Biguanide)
HbA1cs_after_Biguanide <- HbA1cs_after_Biguanide %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_Biguanide)[2] <- "Month_After"
names(HbA1cs_after_Biguanide)[3] <- "HbA1c_After"
names(HbA1cs_after_Biguanide)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_Biguanide_BEFORE_vs_AFTER <-HbA1cs_before_Biguanide %>% full_join(HbA1cs_after_Biguanide)

write.csv(HbA1cs_before_Biguanide_BEFORE_vs_AFTER, "HbA1cs_before_Biguanide_BEFORE_vs_AFTER.csv")

# calculate n of months until Biguanide and after Biguanide until next HbA1c reading
HbA1cs_before_Biguanide_BEFORE_vs_AFTER <- HbA1cs_before_Biguanide_BEFORE_vs_AFTER %>% mutate(Months_until_S = Month_First_Biguanide - Month_Prior) %>% mutate(Month_from_S_on = Month_After - Month_First_Biguanide)
#percentage reduction
HbA1cs_before_Biguanide_BEFORE_vs_AFTER <- HbA1cs_before_Biguanide_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
HbA1cs_before_Biguanide_BEFORE_vs_AFTER %>% 
  select(HbA1c_reduction, weight) %>% 
  arrange(HbA1c_reduction) %>%
  mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                             ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                    ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                           ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                  ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                         ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                       ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))


# mean HbA1cs before and after
HbA1cs_before_Biguanide_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))


# density distribution before and after therapy
HbA1cs_before_Biguanide_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")






# ----
# HbA1c evolution, different therapies,line summary --------------------------------------------
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(rstatix)

HbA1c_Evolution_Therapy_Lines <- read.csv("HbA1c_Evolution_Therapy_Lines_PAIRED.csv", sep=",")
names(HbA1c_Evolution_Therapy_Lines)[1] <- "patient"


HbA1c_Evolution_Therapy_Lines_ANOVA <- HbA1c_Evolution_Therapy_Lines %>% group_by(Therapy, Month) %>% 
  filter(Month>=-12 & Month <= 12) 

Patients_to_keep <- HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  group_by(Therapy, patient) %>%
  summarise(n=n())%>%
  filter(n==2) %>%
  select(Therapy, patient)

HbA1c_Evolution_Therapy_Lines_ANOVA <- Patients_to_keep%>% left_join(HbA1c_Evolution_Therapy_Lines_ANOVA)

length(unique(HbA1c_Evolution_Therapy_Lines_ANOVA$patient)) #29464

# HbA1c_Evolution_Therapy_Lines_ANOVA %>% ungroup() %>% filter(Month <0) %>% select(Month) %>%
#   ggplot(aes(Month))+
#   geom_density(aes(fill = Month), alpha =0.6)+
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())

# HbA1c_Evolution_Therapy_Lines_ANOVA%>% ungroup()  %>%  filter(Month >0) %>% select(Month) %>%
#   ggplot(aes(Month))+
#   geom_density(aes(fill = Month), alpha =0.6)+
#   theme(panel.grid.major=element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())


data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>% ungroup() %>% select(Therapy, patient, Month) %>% mutate(Month = abs(Month)) %>% 
  group_by(Therapy, patient) %>% summarise(total_duration = sum(Month)) %>% select(total_duration) %>%
  group_by(total_duration) %>% summarise(n=n()))



data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>% ungroup() %>% filter(Therapy == "GLP1") %>% select(Therapy, patient, Month) %>% mutate(Month = abs(Month)) %>% 
             group_by(Therapy, patient) %>% summarise(total_duration = sum(Month)) %>% select(total_duration) %>%
             group_by(total_duration) %>% summarise(n=n()))



data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>% ungroup() %>% filter(Therapy == "Insulin") %>% select(Therapy, patient, Month) %>% mutate(Month = abs(Month)) %>% 
             group_by(Therapy, patient) %>% summarise(total_duration = sum(Month)) %>% select(total_duration) %>%
             group_by(total_duration) %>% summarise(n=n()))



HbA1c_Evolution_Therapy_Lines_ANOVA %>% 
  group_by(Therapy, Month) %>% 
  ggplot(aes(x=Month, y=HbA1c, fill=Therapy, colour=Therapy))+
  geom_smooth(size=2.5)+
  ylab("HbA1c %\n")+
  xlab("\nMonth")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_colour_jama()+
  ggsci::scale_fill_jama()



ANOVA_results_two_way <- aov(HbA1c ~ Period + Therapy, data = HbA1c_Evolution_Therapy_Lines_ANOVA)
summary(ANOVA_results_two_way)


tukey.two.way<-TukeyHSD(ANOVA_results_two_way)
tukey.two.way


# REPEATED MEASURES: 
HbA1c_Evolution_Therapy_Lines_ANOVA

HbA1c_Evolution_Therapy_Lines_ANOVA %>% 
  mutate(Period = as.factor(Period))%>%
  select(HbA1c, Therapy,Period)%>%
  group_by(Period,Therapy)%>%
  get_summary_stats(HbA1c, type="mean_sd")


HbA1c_Evolution_Therapy_Lines_ANOVA %>% 
  select(HbA1c, Therapy,Period)%>%
  group_by(Therapy)%>%
  do(tidy(t.test(HbA1c ~ Period, data = ., paired=T)))

ggboxplot(HbA1c_Evolution_Therapy_Lines_ANOVA, x="Therapy", y ="HbA1c", color="Period", fill="Period", alpha=0.8)
  

ggqqplot(HbA1c_Evolution_Therapy_Lines_ANOVA, "HbA1c")+facet_grid(Therapy~Period, labeller="label_both")



HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  group_by(Therapy, patient) %>%
  summarise(n=n())%>%
  filter(n==2) #each patient appears only 1x, all good

HbA1c_Evolution_Therapy_Lines_ANOVA %>%
    filter(Therapy=="Biguanide")%>%
    group_by(Therapy) %>%
    pairwise_t_test(HbA1c ~ Period, paired = T,
                    p.adjust.method="bonferroni")


# ----
# Check if the patient is still on therapy after treatment initation (lines summary effect size) ----------------
# fetch the month
# fetch what the therapy is

  
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)


DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(-c(disease, weight, p2, d2, s1, s2,p1_RxExp, stops, flow, starts, re_starts))

HbA1c_Evolution_Therapy_Lines_ANOVA <- HbA1c_Evolution_Therapy_Lines_ANOVA %>% mutate(Exact_month=as.character(Exact_month))

HbA1c_Evolution_Therapy_Lines_ANOVA <- HbA1c_Evolution_Therapy_Lines_ANOVA %>% left_join(DIA_Flows_Aux._Long, by=c("patient"="patient", "Exact_month"="p1"))

rows <- sample(nrow(HbA1c_Evolution_Therapy_Lines_ANOVA))
HbA1c_Evolution_Therapy_Lines_ANOVA <- HbA1c_Evolution_Therapy_Lines_ANOVA[rows, ]

HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  filter(Therapy == "Biguanide") %>%
  mutate(d1 = ifelse(grepl("(^|\\D)(1{1})(\\D|$)",d1), "YES",
                     ifelse(grepl("(^|\\D)(2{1})(\\D|$)",d1), "YES", "NO"))) %>%
  ggplot(aes(Month, HbA1c, fill=d1, color=d1))+
  ylim(0,20)+
  geom_jitter(size=1, show.legend = F, alpha=0.5)+
  geom_smooth(aes(Month, HbA1c, fill="midnightblue", color="midnightblue"), show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_colour_jama()+
  ggsci::scale_fill_jama()


data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  filter(Therapy == "Biguanide") %>%
  mutate(d1 = ifelse(grepl("(^|\\D)(1{1})(\\D|$)",d1), "YES",
                     ifelse(grepl("(^|\\D)(2{1})(\\D|$)",d1), "YES", "NO"))) %>%
  filter(Period == "After") %>%
  ungroup()  %>% select(Month, d1) %>% group_by(Month, d1) %>% summarise(n=n()))

HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  filter(Therapy == "Antidiabetic") %>%
  mutate(d1 = ifelse(grepl("(^|\\D)(8{1})(\\D|$)",d1), "YES",
                     ifelse(grepl("(^|\\D)(9{1})(\\D|$)",d1), "YES",
                            ifelse(grepl("(^|\\D)(10{1})(\\D|$)",d1), "YES",
                                   ifelse(grepl("(^|\\D)(11{1})(\\D|$)",d1), "YES",
                                          ifelse(grepl("(^|\\D)(12{1})(\\D|$)",d1), "YES",
                                                 ifelse(grepl("(^|\\D)(13{1})(\\D|$)",d1), "YES",
                                                        ifelse(grepl("(^|\\D)(14{1})(\\D|$)",d1), "YES",
                                                               ifelse(grepl("(^|\\D)(15{1})(\\D|$)",d1), "YES",
                                                                      ifelse(grepl("(^|\\D)(16{1})(\\D|$)",d1), "YES",
                                                                             ifelse(grepl("(^|\\D)(17{1})(\\D|$)",d1), "YES",
                                                                                    ifelse(grepl("(^|\\D)(18{1})(\\D|$)",d1), "YES",
                                                                                           ifelse(grepl("(^|\\D)(19{1})(\\D|$)",d1), "YES",
                                                                                                  ifelse(grepl("(^|\\D)(20{1})(\\D|$)",d1), "YES",
                                                                                                         ifelse(grepl("(^|\\D)(21{1})(\\D|$)",d1), "YES",
                                                                                                                ifelse(grepl("(^|\\D)(22{1})(\\D|$)",d1), "YES","NO")))))))))))))))) %>%
  ggplot(aes(Month, HbA1c, fill=d1, color=d1))+
  geom_jitter(size=1, show.legend = F, alpha=0.5)+
  ylim(0,20)+
  geom_smooth(aes(Month, HbA1c, fill="midnightblue", color="midnightblue"), show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_colour_jama()+
  ggsci::scale_fill_jama()


data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>%
             filter(Therapy == "Antidiabetic") %>%
             mutate(d1 = ifelse(grepl("(^|\\D)(8{1})(\\D|$)",d1), "YES",
                                ifelse(grepl("(^|\\D)(9{1})(\\D|$)",d1), "YES",
                                       ifelse(grepl("(^|\\D)(10{1})(\\D|$)",d1), "YES",
                                              ifelse(grepl("(^|\\D)(11{1})(\\D|$)",d1), "YES",
                                                     ifelse(grepl("(^|\\D)(12{1})(\\D|$)",d1), "YES",
                                                            ifelse(grepl("(^|\\D)(13{1})(\\D|$)",d1), "YES",
                                                                   ifelse(grepl("(^|\\D)(14{1})(\\D|$)",d1), "YES",
                                                                          ifelse(grepl("(^|\\D)(15{1})(\\D|$)",d1), "YES",
                                                                                 ifelse(grepl("(^|\\D)(16{1})(\\D|$)",d1), "YES",
                                                                                        ifelse(grepl("(^|\\D)(17{1})(\\D|$)",d1), "YES",
                                                                                               ifelse(grepl("(^|\\D)(18{1})(\\D|$)",d1), "YES",
                                                                                                      ifelse(grepl("(^|\\D)(19{1})(\\D|$)",d1), "YES",
                                                                                                             ifelse(grepl("(^|\\D)(20{1})(\\D|$)",d1), "YES",
                                                                                                                    ifelse(grepl("(^|\\D)(21{1})(\\D|$)",d1), "YES",
                                                                                                                           ifelse(grepl("(^|\\D)(22{1})(\\D|$)",d1), "YES","NO")))))))))))))))) %>%
             filter(Period == "After") %>%
             ungroup()  %>% select(Month, d1) %>% group_by(Month, d1) %>% summarise(n=n()))


HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  filter(Therapy == "GLP1") %>%
  mutate(d1 = ifelse(grepl("39",d1), "YES",
                     ifelse(grepl("40",d1), "YES",
                            ifelse(grepl("41",d1), "YES",
                                   ifelse(grepl("42",d1), "YES",
                                          ifelse(grepl("43",d1), "YES", "NO")))))) %>%
  ggplot(aes(Month, HbA1c, fill=d1, color=d1))+
  geom_jitter(size=1, show.legend = F)+
  ylim(0,20)+
  geom_smooth(aes(Month, HbA1c, fill="midnightblue", color="midnightblue"), show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_colour_jama()+
  ggsci::scale_fill_jama()



data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>%
             filter(Therapy == "GLP1") %>%
             mutate(d1 = ifelse(grepl("39",d1), "YES",
                                ifelse(grepl("40",d1), "YES",
                                       ifelse(grepl("41",d1), "YES",
                                              ifelse(grepl("42",d1), "YES",
                                                     ifelse(grepl("43",d1), "YES", "NO")))))) %>%
             filter(Period == "After") %>%
             ungroup()  %>% select(Month, d1) %>% group_by(Month, d1) %>% summarise(n=n()))



HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  filter(Therapy == "DPP4") %>%
  mutate(d1 = ifelse(grepl("23",d1), "YES",
                     ifelse(grepl("24",d1), "YES",
                            ifelse(grepl("25",d1), "YES",
                                   ifelse(grepl("26",d1), "YES",
                                          ifelse(grepl("27",d1), "YES",
                                                 ifelse(grepl("28",d1), "YES",
                                                        ifelse(grepl("29",d1), "YES",
                                                               ifelse(grepl("30",d1), "YES",
                                                                      ifelse(grepl("31",d1), "YES","NO")))))))))) %>%
  ggplot(aes(Month, HbA1c, fill=d1, color=d1))+
  geom_jitter(size=1, show.legend = F, alpha=0.5)+
  ylim(0,20)+
  geom_smooth(aes(Month, HbA1c, fill="midnightblue", color="midnightblue"), show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_colour_jama()+
  ggsci::scale_fill_jama()





data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>%
             filter(Therapy == "DPP4") %>%
             mutate(d1 = ifelse(grepl("23",d1), "YES",
                                ifelse(grepl("24",d1), "YES",
                                       ifelse(grepl("25",d1), "YES",
                                              ifelse(grepl("26",d1), "YES",
                                                     ifelse(grepl("27",d1), "YES",
                                                            ifelse(grepl("28",d1), "YES",
                                                                   ifelse(grepl("29",d1), "YES",
                                                                          ifelse(grepl("30",d1), "YES",
                                                                                 ifelse(grepl("31",d1), "YES","NO")))))))))) %>%
             filter(Period == "After") %>%
             ungroup()  %>% select(Month, d1) %>% group_by(Month, d1) %>% summarise(n=n()))


HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  filter(Therapy == "SGLT2") %>%
  mutate(d1 = ifelse(grepl("32",d1), "YES",
                     ifelse(grepl("33",d1), "YES",
                            ifelse(grepl("34",d1), "YES",
                                   ifelse(grepl("35",d1), "YES",
                                          ifelse(grepl("36",d1), "YES",
                                                 ifelse(grepl("37",d1), "YES","NO"))))))) %>%
  ggplot(aes(Month, HbA1c, fill=d1, color=d1))+
  geom_jitter(size=1, show.legend = F, alpha=0.5)+
  ylim(0,20)+
  geom_smooth(aes(Month, HbA1c, fill="midnightblue", color="midnightblue"), show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_colour_jama()+
  ggsci::scale_fill_jama()




data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>%
             filter(Therapy == "SGLT2") %>%
             mutate(d1 = ifelse(grepl("32",d1), "YES",
                                ifelse(grepl("33",d1), "YES",
                                       ifelse(grepl("34",d1), "YES",
                                              ifelse(grepl("35",d1), "YES",
                                                     ifelse(grepl("36",d1), "YES",
                                                            ifelse(grepl("37",d1), "YES","NO"))))))) %>%
             filter(Period == "After") %>%
             ungroup()  %>% select(Month, d1) %>% group_by(Month, d1) %>% summarise(n=n()))





HbA1c_Evolution_Therapy_Lines_ANOVA %>%
  filter(Therapy == "Insulin") %>%
  mutate(d1 = ifelse(grepl("44",d1), "YES",
                     ifelse(grepl("45",d1), "YES",
                            ifelse(grepl("46",d1), "YES",
                                   ifelse(grepl("47",d1), "YES",
                                          ifelse(grepl("48",d1), "YES",
                                                 ifelse(grepl("49",d1), "YES",
                                                        ifelse(grepl("50",d1), "YES",
                                                               ifelse(grepl("51",d1), "YES",
                                                                      ifelse(grepl("52",d1), "YES",
                                                                             ifelse(grepl("53",d1), "YES",
                                                                                    ifelse(grepl("54",d1), "YES",
                                                                                           ifelse(grepl("55",d1), "YES",
                                                                                                  ifelse(grepl("56",d1), "YES",
                                                                                                         ifelse(grepl("57",d1), "YES","NO"))))))))))))))) %>%
  ggplot(aes(Month, HbA1c, fill=d1, color=d1))+
  geom_jitter(size=1, show.legend = F, alpha=0.5)+
  ylim(0,20)+
  geom_smooth(aes(Month, HbA1c, fill="midnightblue", color="midnightblue"), show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_colour_jama()+
  ggsci::scale_fill_jama()






data.frame(HbA1c_Evolution_Therapy_Lines_ANOVA %>%
             filter(Therapy == "Insulin") %>%
             mutate(d1 = ifelse(grepl("44",d1), "YES",
                                ifelse(grepl("45",d1), "YES",
                                       ifelse(grepl("46",d1), "YES",
                                              ifelse(grepl("47",d1), "YES",
                                                     ifelse(grepl("48",d1), "YES",
                                                            ifelse(grepl("49",d1), "YES",
                                                                   ifelse(grepl("50",d1), "YES",
                                                                          ifelse(grepl("51",d1), "YES",
                                                                                 ifelse(grepl("52",d1), "YES",
                                                                                        ifelse(grepl("53",d1), "YES",
                                                                                               ifelse(grepl("54",d1), "YES",
                                                                                                      ifelse(grepl("55",d1), "YES",
                                                                                                             ifelse(grepl("56",d1), "YES",
                                                                                                                    ifelse(grepl("57",d1), "YES","NO"))))))))))))))) %>%
             filter(Period == "After") %>%
             ungroup()  %>% select(Month, d1) %>% group_by(Month, d1) %>% summarise(n=n()))




  

# ----
# HbA1c by AGE -------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"
HbA1cHist <- HbA1cHist %>% select(patient, X49:X60)
HbA1cHist <- HbA1cHist %>% gather(Month, HbA1c, X49:X60, factor_key=TRUE)
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )

HbA1cHist <- HbA1cHist %>% filter(!is.na(HbA1c))

weighted.mean(HbA1cHist$HbA1c, HbA1cHist$weight, na.rm = T)
# 6.319834


# table with ages
DIA_Japan_Demographics <- read.table("DIA Japan Demographics.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Demographics <- DIA_Japan_Demographics %>% select(patid, weight, age)
names(DIA_Japan_Demographics)[1] <- "patient"
HbA1cHist<- HbA1cHist %>% left_join(DIA_Japan_Demographics)
HbA1cHist <- HbA1cHist %>% filter(!is.na(age))

length(unique(HbA1cHist$patient)) #166283
HbA1cHist$weight <- as.numeric(HbA1cHist$weight)

data.frame(HbA1cHist %>% group_by(age) %>% summarise(n = weighted.mean(HbA1c, weight, na.rm = T)) %>%
             filter(age != 21))













# ----
# Check BMI evolution over time ------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# File with HbA1c over time
BMIHIst <- read.table("BMIHistAll.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

names(BMIHIst)[1] <- "patient"

BMIHIst <- gather(BMIHIst, Month, BMI, month1:month60, factor_key=TRUE)

BMIHIst <- BMIHIst %>% filter(BMI != "")
length(unique(BMIHIst$patient))
BMIHIst <- separate_rows(BMIHIst, BMI, sep = ",", convert=T )

BMIHIst$Month <- str_replace(BMIHIst$Month, "month1", "1")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month2", "2")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month3", "3")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month4", "4")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month5", "5")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month6", "6")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month7", "7")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month8", "8")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month9", "9")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month10", "10")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month11", "11")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month12", "12")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month13", "13")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month14", "14")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month15", "15")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month16", "16")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month17", "17")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month18", "18")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month19", "19")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month20", "20")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month21", "21")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month22", "22")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month23", "23")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month24", "24")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month25", "25")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month26", "26")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month27", "27")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month28", "28")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month29", "29")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month30", "30")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month31", "31")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month32", "32")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month33", "33")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month34", "34")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month35", "35")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month36", "36")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month37", "37")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month38", "38")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month39", "39")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month40", "40")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month41", "41")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month42", "42")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month43", "43")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month44", "44")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month45", "45")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month46", "46")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month47", "47")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month48", "48")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month49", "49")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month50", "50")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month51", "51")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month52", "52")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month53", "53")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month54", "54")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month55", "55")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month56", "56")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month57", "57")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month58", "58")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month59", "59")
BMIHIst$Month <- str_replace(BMIHIst$Month, "month60", "60")

BMIHIst <- BMIHIst %>% mutate(Month = as.numeric(Month))

BMIHIst %>% filter(BMI<60) %>% ggplot(aes(x=Month, y=BMI))+
  geom_smooth(size=1, level=0.99, span=0.1, fill="darkslategrey", color="darkslategrey")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylim(25,30)


BMIHIst %>% filter(Month == 59) %>% summarise(n = mean(BMI)) # 27.5
# the graph is correct, just very narrow


BMIHIst %>% filter(BMI<60) %>% ggplot(aes(x=as.factor(Month), y=BMI, fill=Month, color=Month))+
  geom_violin(show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())





# ----
# BMI by AGE -------------------------------------------------------------------------------
# File with BMI over time
BMIHistAll <- read.table("BMIHistAll.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(BMIHistAll)[1] <- "patient"
BMIHistAll <- BMIHistAll %>% select(patient, month49:month60)
BMIHistAll <- BMIHistAll %>% gather(Month, BMI, month49:month60, factor_key=TRUE)
BMIHistAll <- BMIHistAll %>% filter(BMI != "")
BMIHistAll <- separate_rows(BMIHistAll, BMI, sep = ",", convert=T )

# table with ages
OBE_Japan_Demographics <- read.table("OBE Japan Demographics.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Demographics <- OBE_Japan_Demographics %>% select(patid, weight, age)
names(OBE_Japan_Demographics)[1] <- "patient"
BMIHistAll<- BMIHistAll %>% left_join(OBE_Japan_Demographics)
BMIHistAll <- BMIHistAll %>% filter(!is.na(age))

length(unique(BMIHistAll$patient)) #389150
BMIHistAll$weight <- as.numeric(BMIHistAll$weight)

BMIHistAll %>% group_by(age) %>% summarise(n = weighted.mean(BMI, weight, na.rm = T)) %>%
  ggplot(aes(as.numeric(age), n))+
  geom_line(color="darkslategrey", size=3)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nAge (years)")+
  ylab("BMI")



# ----
# No Antiobesity  first 12months, started between 12 and 48months, follow from 12 to 60 --------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
OBE_Flows_Aux._Long <- read.table("OBE_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(patient, p1, d1, s1)
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no Antiobesity inject before month 12
Patients_no_Antiobesity_12 <- OBE_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(!grepl("3",d1) & !grepl("4",d1) & !grepl("5",d1) & !grepl("6",d1) & !grepl("7",d1)) %>%
  select(patient)

# vector of unique patients
Patients_no_Antiobesity_12 <- Patients_no_Antiobesity_12 %>% distinct()

# filter for patient who did have an Antiobesity inject from 12 to 48
Patients_start_Antiobesity_12_48 <- OBE_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("k",s1) | grepl("o",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_Antiobesity_12_48 <- Patients_start_Antiobesity_12_48 %>% distinct()

# select patient intersection 
Patients_Antiobesity_track <- Patients_no_Antiobesity_12 %>% inner_join(Patients_start_Antiobesity_12_48)

# read table in wide format from months 1 to 60
OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
OBE_Japan_Drug_Histories <-  OBE_Japan_Drug_Histories %>%  select(15:63)

# convert no antiobesity too zero, and antiobesity to one, then convert everything to numeric 
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% 
  mutate_if(grepl('3',.), ~replace(., grepl('3', .), "Antiobesity"))%>% 
  mutate_if(grepl('4',.), ~replace(., grepl('4', .), "Antiobesity"))%>% 
  mutate_if(grepl('5',.), ~replace(., grepl('5', .), "Antiobesity"))%>% 
  mutate_if(grepl('6',.), ~replace(., grepl('6', .), "Antiobesity"))%>%
  mutate_if(grepl('7',.), ~replace(., grepl('7', .), "Antiobesity"))


OBE_Japan_Drug_Histories <-  OBE_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antiobesity",1,0))

OBE_Japan_Drug_Histories[] <-  lapply(OBE_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Japan_Drug_Histories_LONG <- read.table("OBE Japan Drug Histories.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Drug_Histories_LONG <- OBE_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories_LONG %>% bind_cols(OBE_Japan_Drug_Histories)
rm(OBE_Japan_Drug_Histories_LONG)

# filter for the patients selected based on Antiobesity inject status
OBE_Japan_Drug_Histories <- Patients_Antiobesity_track %>% left_join(OBE_Japan_Drug_Histories)

#convert to long format
OBE_Japan_Drug_Histories <- gather(OBE_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON Antiobesity inject
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month1", "1")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month2", "2")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month3", "3")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month4", "4")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month5", "5")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month6", "6")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month7", "7")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month8", "8")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month9", "9")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month10", "10")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month11", "11")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month12", "12")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month13", "13")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month14", "14")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month15", "15")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month16", "16")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month17", "17")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month18", "18")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month19", "19")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month20", "20")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month21", "21")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month22", "22")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month23", "23")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month24", "24")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month25", "25")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month26", "26")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month27", "27")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month28", "28")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month29", "29")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month30", "30")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month31", "31")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month32", "32")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month33", "33")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month34", "34")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month35", "35")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month36", "36")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month37", "37")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month38", "38")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month39", "39")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month40", "40")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month41", "41")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month42", "42")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month43", "43")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month44", "44")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month45", "45")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month46", "46")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month47", "47")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month48", "48")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month49", "49")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month50", "50")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month51", "51")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month52", "52")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month53", "53")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month54", "54")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month55", "55")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month56", "56")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month57", "57")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month58", "58")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month59", "59")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month60", "60")

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Antiobesity inject
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took Antiobesity inject
OBE_Japan_Drug_Histories_FIRST_Antiobesity <- OBE_Japan_Drug_Histories
names(OBE_Japan_Drug_Histories_FIRST_Antiobesity)[3] <- "Month_First_Antiobesity"




# Now get the BMI levels
# File with BMI over time
BMIHistAll <- read.table("BMIHistAll.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(BMIHistAll)[1] <- "patient"

#convert to long format
BMIHistAll <- gather(BMIHistAll, Month, BMI, month1:month60, factor_key=TRUE)

# pick only those months with BMI readings
BMIHistAll <- BMIHistAll %>% filter(BMI != "")

#recode
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month1", "1")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month2", "2")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month3", "3")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month4", "4")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month5", "5")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month6", "6")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month7", "7")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month8", "8")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month9", "9")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month10", "10")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month11", "11")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month12", "12")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month13", "13")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month14", "14")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month15", "15")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month16", "16")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month17", "17")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month18", "18")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month19", "19")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month20", "20")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month21", "21")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month22", "22")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month23", "23")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month24", "24")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month25", "25")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month26", "26")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month27", "27")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month28", "28")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month29", "29")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month30", "30")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month31", "31")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month32", "32")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month33", "33")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month34", "34")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month35", "35")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month36", "36")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month37", "37")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month38", "38")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month39", "39")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month40", "40")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month41", "41")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month42", "42")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month43", "43")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month44", "44")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month45", "45")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month46", "46")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month47", "47")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month48", "48")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month49", "49")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month50", "50")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month51", "51")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month52", "52")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month53", "53")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month54", "54")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month55", "55")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month56", "56")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month57", "57")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month58", "58")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month59", "59")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month60", "60")

#some patients had more than 1 reading, separate based on commas
BMIHistAll <- separate_rows(BMIHistAll, BMI, sep = ",", convert=T )
#numeric
BMIHistAll<- BMIHistAll %>% mutate(Month = as.numeric(Month)) %>% mutate(BMI = as.numeric(BMI))
#group, arrange
BMIHistAll <- BMIHistAll %>% arrange(patient, Month, BMI) %>% group_by(patient)
#filter for the patients that fit the Antiobesity Inject criteria
Patient_first_Antiobesity <-  OBE_Japan_Drug_Histories_FIRST_Antiobesity %>% select(patient)
BMIHistAll <- Patient_first_Antiobesity %>% left_join(BMIHistAll)
#remove those ptients with no HbA1c level readings
BMIHistAll<- BMIHistAll %>% filter(!is.na(weight))

#convert prior to joining
OBE_Japan_Drug_Histories_FIRST_Antiobesity$Month_First_Antiobesity <- as.character(OBE_Japan_Drug_Histories_FIRST_Antiobesity$Month_First_Antiobesity)
BMIHistAll$Month <- as.character(BMIHistAll$Month)
BMIHistAll$BMI <- as.character(BMIHistAll$BMI)

#join the patient first Antiobesity month to his BMI readings
BMIHistAll <- BMIHistAll %>% left_join(OBE_Japan_Drug_Histories_FIRST_Antiobesity, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
BMIHistAll <- BMIHistAll %>% select(-weight.x)

#convert to numeric
BMIHistAll$Month <- as.numeric(BMIHistAll$Month)
BMIHistAll$BMI <- as.numeric(BMIHistAll$BMI)
BMIHistAll$Month_First_Antiobesity <- as.numeric(BMIHistAll$Month_First_Antiobesity)
BMIHistAll$weight.y <- as.numeric(BMIHistAll$weight.y)

#now split into months before Antiobesity start and months after Antiobesity start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
BMI_before_Antiobesity <- BMIHistAll %>% group_by(patient) %>% filter(Month < Month_First_Antiobesity)
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Antiobesity)[2] <- "Month_Prior"
names(BMI_before_Antiobesity)[3] <- "BMI_Prior"
names(BMI_before_Antiobesity)[4] <- "weight"

BMI_after_Antiobesity <- BMIHistAll %>% group_by(patient) %>% filter(Month > Month_First_Antiobesity)
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Antiobesity)[2] <- "Month_After"
names(BMI_after_Antiobesity)[3] <- "BMI_After"
names(BMI_after_Antiobesity)[4] <- "weight"

#join the before and after BMIs
BMI_before_Antiobesity_BEFORE_vs_AFTER <-BMI_before_Antiobesity %>% full_join(BMI_after_Antiobesity)

write.csv(BMI_before_Antiobesity_BEFORE_vs_AFTER, "BMI_before_Antiobesity_BEFORE_vs_AFTER.csv")

BMI_before_Antiobesity_BEFORE_vs_AFTER <- read.csv("BMI_before_Antiobesity_BEFORE_vs_AFTER.csv")

# calculate n of months until Antiobesity and after Antiobesity until next BMI reading
BMI_before_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity_BEFORE_vs_AFTER %>% mutate(Months_until_drug = Month_First_Antiobesity - Month_Prior) %>% mutate(Month_from_drug_on = Month_After - Month_First_Antiobesity)
#percentage reduction
BMI_before_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity_BEFORE_vs_AFTER %>% mutate(BMI_reduction = ((BMI_After/BMI_Prior)-1)*100)

# image, by buckets of reduction
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% 
  select(BMI_reduction, weight) %>% 
  arrange(BMI_reduction) %>%
  mutate(stocks_red = ifelse(BMI_reduction < -60, "-100% to -60%",
                             ifelse(BMI_reduction >= -60 & BMI_reduction < -30, "-60% to -30%",
                                    ifelse(BMI_reduction >= -30 & BMI_reduction < -10, "-30% to -10%",
                                           ifelse(BMI_reduction >= -10 & BMI_reduction <0, "-10% to 0%",
                                                  ifelse(BMI_reduction >= 0 & BMI_reduction < 10, "0% to 10%",
                                                         ifelse(BMI_reduction >=10 & BMI_reduction < 30, "10% to 30%", 
                                                                ifelse(BMI_reduction >= 30 & BMI_reduction < 60 , "30% to 60%", 
                                                                       ifelse(BMI_reduction >= 60, "60% to 100%", BMI_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))



# mean BMIs before and after
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% select(BMI_Prior, BMI_After) %>%
  gather(Time, value, BMI_Prior:BMI_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))



# density distribution before and after therapy
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% select(BMI_Prior, BMI_After) %>%
  gather(Time, value, BMI_Prior:BMI_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nBMI level")+ ylab("Proportion of patients \n")



# ----
# No Antiobesity inject first 12months, started between 12 and 48months, follow from 12 to 60 --------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
OBE_Flows_Aux._Long <- read.table("OBE_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(patient, p1, d1, s1)
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no Antiobesity inject before month 12
Patients_no_Antiobesity_12 <- OBE_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(!grepl("5",d1) & !grepl("6",d1) & !grepl("7",d1)) %>%
  select(patient)

# vector of unique patients
Patients_no_Antiobesity_12 <- Patients_no_Antiobesity_12 %>% distinct()

# filter for patient who did have an Antiobesity inject from 12 to 48
Patients_start_Antiobesity_12_48 <- OBE_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("o",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_Antiobesity_12_48 <- Patients_start_Antiobesity_12_48 %>% distinct()

# select patient intersection 
Patients_Antiobesity_track <- Patients_no_Antiobesity_12 %>% inner_join(Patients_start_Antiobesity_12_48)

# read table in wide format from months 1 to 60
OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
OBE_Japan_Drug_Histories <-  OBE_Japan_Drug_Histories %>%  select(15:63)

# convert no antiobesity too zero, and antiobesity to one, then convert everything to numeric 
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% 
  mutate_if(grepl('5',.), ~replace(., grepl('5', .), "Antiobesity"))%>% 
  mutate_if(grepl('6',.), ~replace(., grepl('6', .), "Antiobesity"))%>%
  mutate_if(grepl('7',.), ~replace(., grepl('7', .), "Antiobesity"))


OBE_Japan_Drug_Histories <-  OBE_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antiobesity",1,0))

OBE_Japan_Drug_Histories[] <-  lapply(OBE_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Japan_Drug_Histories_LONG <- read.table("OBE Japan Drug Histories.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Drug_Histories_LONG <- OBE_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories_LONG %>% bind_cols(OBE_Japan_Drug_Histories)
rm(OBE_Japan_Drug_Histories_LONG)

# filter for the patients selected based on Antiobesity inject status
OBE_Japan_Drug_Histories <- Patients_Antiobesity_track %>% left_join(OBE_Japan_Drug_Histories)

#convert to long format
OBE_Japan_Drug_Histories <- gather(OBE_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON Antiobesity inject
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month1", "1")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month2", "2")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month3", "3")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month4", "4")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month5", "5")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month6", "6")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month7", "7")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month8", "8")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month9", "9")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month10", "10")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month11", "11")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month12", "12")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month13", "13")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month14", "14")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month15", "15")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month16", "16")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month17", "17")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month18", "18")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month19", "19")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month20", "20")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month21", "21")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month22", "22")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month23", "23")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month24", "24")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month25", "25")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month26", "26")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month27", "27")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month28", "28")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month29", "29")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month30", "30")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month31", "31")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month32", "32")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month33", "33")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month34", "34")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month35", "35")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month36", "36")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month37", "37")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month38", "38")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month39", "39")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month40", "40")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month41", "41")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month42", "42")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month43", "43")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month44", "44")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month45", "45")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month46", "46")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month47", "47")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month48", "48")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month49", "49")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month50", "50")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month51", "51")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month52", "52")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month53", "53")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month54", "54")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month55", "55")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month56", "56")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month57", "57")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month58", "58")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month59", "59")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month60", "60")

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Antiobesity inject
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took Antiobesity inject
OBE_Japan_Drug_Histories_FIRST_Antiobesity <- OBE_Japan_Drug_Histories
names(OBE_Japan_Drug_Histories_FIRST_Antiobesity)[3] <- "Month_First_Antiobesity"




# Now get the BMI levels
# File with BMI over time
BMIHistAll <- read.table("BMIHistAll.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(BMIHistAll)[1] <- "patient"

#convert to long format
BMIHistAll <- gather(BMIHistAll, Month, BMI, month1:month60, factor_key=TRUE)

# pick only those months with BMI readings
BMIHistAll <- BMIHistAll %>% filter(BMI != "")

#recode
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month1", "1")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month2", "2")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month3", "3")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month4", "4")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month5", "5")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month6", "6")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month7", "7")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month8", "8")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month9", "9")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month10", "10")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month11", "11")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month12", "12")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month13", "13")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month14", "14")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month15", "15")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month16", "16")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month17", "17")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month18", "18")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month19", "19")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month20", "20")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month21", "21")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month22", "22")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month23", "23")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month24", "24")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month25", "25")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month26", "26")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month27", "27")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month28", "28")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month29", "29")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month30", "30")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month31", "31")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month32", "32")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month33", "33")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month34", "34")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month35", "35")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month36", "36")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month37", "37")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month38", "38")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month39", "39")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month40", "40")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month41", "41")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month42", "42")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month43", "43")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month44", "44")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month45", "45")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month46", "46")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month47", "47")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month48", "48")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month49", "49")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month50", "50")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month51", "51")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month52", "52")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month53", "53")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month54", "54")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month55", "55")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month56", "56")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month57", "57")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month58", "58")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month59", "59")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month60", "60")

#some patients had more than 1 reading, separate based on commas
BMIHistAll <- separate_rows(BMIHistAll, BMI, sep = ",", convert=T )
#numeric
BMIHistAll<- BMIHistAll %>% mutate(Month = as.numeric(Month)) %>% mutate(BMI = as.numeric(BMI))
#group, arrange
BMIHistAll <- BMIHistAll %>% arrange(patient, Month, BMI) %>% group_by(patient)
#filter for the patients that fit the Antiobesity Inject criteria
Patient_first_Antiobesity <-  OBE_Japan_Drug_Histories_FIRST_Antiobesity %>% select(patient)
BMIHistAll <- Patient_first_Antiobesity %>% left_join(BMIHistAll)
#remove those ptients with no HbA1c level readings
BMIHistAll<- BMIHistAll %>% filter(!is.na(weight))

#convert prior to joining
OBE_Japan_Drug_Histories_FIRST_Antiobesity$Month_First_Antiobesity <- as.character(OBE_Japan_Drug_Histories_FIRST_Antiobesity$Month_First_Antiobesity)
BMIHistAll$Month <- as.character(BMIHistAll$Month)
BMIHistAll$BMI <- as.character(BMIHistAll$BMI)

#join the patient first Antiobesity month to his BMI readings
BMIHistAll <- BMIHistAll %>% left_join(OBE_Japan_Drug_Histories_FIRST_Antiobesity, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
BMIHistAll <- BMIHistAll %>% select(-weight.x)

#convert to numeric
BMIHistAll$Month <- as.numeric(BMIHistAll$Month)
BMIHistAll$BMI <- as.numeric(BMIHistAll$BMI)
BMIHistAll$Month_First_Antiobesity <- as.numeric(BMIHistAll$Month_First_Antiobesity)
BMIHistAll$weight.y <- as.numeric(BMIHistAll$weight.y)

#now split into months before Antiobesity start and months after Antiobesity start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
BMI_before_Antiobesity <- BMIHistAll %>% group_by(patient) %>% filter(Month < Month_First_Antiobesity)
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Antiobesity)[2] <- "Month_Prior"
names(BMI_before_Antiobesity)[3] <- "BMI_Prior"
names(BMI_before_Antiobesity)[4] <- "weight"

BMI_after_Antiobesity <- BMIHistAll %>% group_by(patient) %>% filter(Month > Month_First_Antiobesity)
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Antiobesity)[2] <- "Month_After"
names(BMI_after_Antiobesity)[3] <- "BMI_After"
names(BMI_after_Antiobesity)[4] <- "weight"

#join the before and after BMIs
BMI_before_Antiobesity_BEFORE_vs_AFTER <-BMI_before_Antiobesity %>% full_join(BMI_after_Antiobesity)

write.csv(BMI_before_Antiobesity_BEFORE_vs_AFTER, "BMI_before_ANY_O_BEFORE_vs_AFTER.csv")


# calculate n of months until Antiobesity and after Antiobesity until next BMI reading
BMI_before_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity_BEFORE_vs_AFTER %>% mutate(Months_until_drug = Month_First_Antiobesity - Month_Prior) %>% mutate(Month_from_drug_on = Month_After - Month_First_Antiobesity)
#percentage reduction
BMI_before_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity_BEFORE_vs_AFTER %>% mutate(BMI_reduction = ((BMI_After/BMI_Prior)-1)*100)

# image, by buckets of reduction
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% 
  select(BMI_reduction, weight) %>% 
  arrange(BMI_reduction) %>%
  mutate(stocks_red = ifelse(BMI_reduction < -60, "-100% to -60%",
                             ifelse(BMI_reduction >= -60 & BMI_reduction < -30, "-60% to -30%",
                                    ifelse(BMI_reduction >= -30 & BMI_reduction < -10, "-30% to -10%",
                                           ifelse(BMI_reduction >= -10 & BMI_reduction <0, "-10% to 0%",
                                                  ifelse(BMI_reduction >= 0 & BMI_reduction < 10, "0% to 10%",
                                                         ifelse(BMI_reduction >=10 & BMI_reduction < 30, "10% to 30%", 
                                                                ifelse(BMI_reduction >= 30 & BMI_reduction < 60 , "30% to 60%", 
                                                                       ifelse(BMI_reduction >= 60, "60% to 100%", BMI_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))


# mean BMIs before and after
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% select(BMI_Prior, BMI_After) %>%
  gather(Time, value, BMI_Prior:BMI_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))

# density distribution before and after therapy
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% select(BMI_Prior, BMI_After) %>%
  gather(Time, value, BMI_Prior:BMI_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nBMI level")+ ylab("Proportion of patients \n")




# ----
# No Antiobesity inject first 12months, started between 12 and 48months, follow from 12 to 60 --------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
OBE_Flows_Aux._Long <- read.table("OBE_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% select(patient, p1, d1, s1)
OBE_Flows_Aux._Long <- OBE_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no Antiobesity inject before month 12
Patients_no_Antiobesity_12 <- OBE_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(!grepl("3",d1) & !grepl("4",d1)) %>%
  select(patient)

# vector of unique patients
Patients_no_Antiobesity_12 <- Patients_no_Antiobesity_12 %>% distinct()

# filter for patient who did have an Antiobesity inject from 12 to 48
Patients_start_Antiobesity_12_48 <- OBE_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("k",s1)) %>%
  select(patient)

# vector of unique patients
Patients_start_Antiobesity_12_48 <- Patients_start_Antiobesity_12_48 %>% distinct()

# select patient intersection 
Patients_Antiobesity_track <- Patients_no_Antiobesity_12 %>% inner_join(Patients_start_Antiobesity_12_48)

# read table in wide format from months 1 to 60
OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
OBE_Japan_Drug_Histories <-  OBE_Japan_Drug_Histories %>%  select(15:63)

# convert no antiobesity too zero, and antiobesity to one, then convert everything to numeric 
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% 
  mutate_if(grepl('3',.), ~replace(., grepl('3', .), "Antiobesity"))%>% 
  mutate_if(grepl('4',.), ~replace(., grepl('4', .), "Antiobesity"))

OBE_Japan_Drug_Histories <-  OBE_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antiobesity",1,0))

OBE_Japan_Drug_Histories[] <-  lapply(OBE_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
OBE_Japan_Drug_Histories_LONG <- read.table("OBE Japan Drug Histories.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Drug_Histories_LONG <- OBE_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories_LONG %>% bind_cols(OBE_Japan_Drug_Histories)
rm(OBE_Japan_Drug_Histories_LONG)

# filter for the patients selected based on Antiobesity inject status
OBE_Japan_Drug_Histories <- Patients_Antiobesity_track %>% left_join(OBE_Japan_Drug_Histories)

#convert to long format
OBE_Japan_Drug_Histories <- gather(OBE_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON Antiobesity inject
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month1", "1")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month2", "2")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month3", "3")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month4", "4")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month5", "5")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month6", "6")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month7", "7")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month8", "8")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month9", "9")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month10", "10")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month11", "11")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month12", "12")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month13", "13")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month14", "14")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month15", "15")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month16", "16")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month17", "17")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month18", "18")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month19", "19")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month20", "20")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month21", "21")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month22", "22")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month23", "23")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month24", "24")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month25", "25")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month26", "26")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month27", "27")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month28", "28")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month29", "29")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month30", "30")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month31", "31")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month32", "32")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month33", "33")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month34", "34")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month35", "35")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month36", "36")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month37", "37")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month38", "38")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month39", "39")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month40", "40")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month41", "41")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month42", "42")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month43", "43")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month44", "44")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month45", "45")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month46", "46")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month47", "47")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month48", "48")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month49", "49")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month50", "50")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month51", "51")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month52", "52")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month53", "53")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month54", "54")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month55", "55")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month56", "56")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month57", "57")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month58", "58")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month59", "59")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month60", "60")

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to Antiobesity inject
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took Antiobesity inject
OBE_Japan_Drug_Histories_FIRST_Antiobesity <- OBE_Japan_Drug_Histories
names(OBE_Japan_Drug_Histories_FIRST_Antiobesity)[3] <- "Month_First_Antiobesity"




# Now get the BMI levels
# File with BMI over time
BMIHistAll <- read.table("BMIHistAll.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(BMIHistAll)[1] <- "patient"

#convert to long format
BMIHistAll <- gather(BMIHistAll, Month, BMI, month1:month60, factor_key=TRUE)

# pick only those months with BMI readings
BMIHistAll <- BMIHistAll %>% filter(BMI != "")

#recode
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month1", "1")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month2", "2")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month3", "3")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month4", "4")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month5", "5")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month6", "6")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month7", "7")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month8", "8")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month9", "9")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month10", "10")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month11", "11")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month12", "12")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month13", "13")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month14", "14")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month15", "15")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month16", "16")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month17", "17")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month18", "18")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month19", "19")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month20", "20")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month21", "21")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month22", "22")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month23", "23")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month24", "24")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month25", "25")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month26", "26")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month27", "27")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month28", "28")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month29", "29")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month30", "30")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month31", "31")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month32", "32")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month33", "33")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month34", "34")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month35", "35")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month36", "36")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month37", "37")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month38", "38")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month39", "39")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month40", "40")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month41", "41")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month42", "42")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month43", "43")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month44", "44")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month45", "45")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month46", "46")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month47", "47")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month48", "48")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month49", "49")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month50", "50")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month51", "51")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month52", "52")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month53", "53")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month54", "54")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month55", "55")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month56", "56")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month57", "57")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month58", "58")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month59", "59")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month60", "60")

#some patients had more than 1 reading, separate based on commas
BMIHistAll <- separate_rows(BMIHistAll, BMI, sep = ",", convert=T )
#numeric
BMIHistAll<- BMIHistAll %>% mutate(Month = as.numeric(Month)) %>% mutate(BMI = as.numeric(BMI))
#group, arrange
BMIHistAll <- BMIHistAll %>% arrange(patient, Month, BMI) %>% group_by(patient)
#filter for the patients that fit the Antiobesity Inject criteria
Patient_first_Antiobesity <-  OBE_Japan_Drug_Histories_FIRST_Antiobesity %>% select(patient)
BMIHistAll <- Patient_first_Antiobesity %>% left_join(BMIHistAll)
#remove those ptients with no HbA1c level readings
BMIHistAll<- BMIHistAll %>% filter(!is.na(weight))

#convert prior to joining
OBE_Japan_Drug_Histories_FIRST_Antiobesity$Month_First_Antiobesity <- as.character(OBE_Japan_Drug_Histories_FIRST_Antiobesity$Month_First_Antiobesity)
BMIHistAll$Month <- as.character(BMIHistAll$Month)
BMIHistAll$BMI <- as.character(BMIHistAll$BMI)

#join the patient first Antiobesity month to his BMI readings
BMIHistAll <- BMIHistAll %>% left_join(OBE_Japan_Drug_Histories_FIRST_Antiobesity, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
BMIHistAll <- BMIHistAll %>% select(-weight.x)

#convert to numeric
BMIHistAll$Month <- as.numeric(BMIHistAll$Month)
BMIHistAll$BMI <- as.numeric(BMIHistAll$BMI)
BMIHistAll$Month_First_Antiobesity <- as.numeric(BMIHistAll$Month_First_Antiobesity)
BMIHistAll$weight.y <- as.numeric(BMIHistAll$weight.y)

#now split into months before Antiobesity start and months after Antiobesity start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
BMI_before_Antiobesity <- BMIHistAll %>% group_by(patient) %>% filter(Month < Month_First_Antiobesity)
BMI_before_Antiobesity <- BMI_before_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), max))
names(BMI_before_Antiobesity)[2] <- "Month_Prior"
names(BMI_before_Antiobesity)[3] <- "BMI_Prior"
names(BMI_before_Antiobesity)[4] <- "weight"

BMI_after_Antiobesity <- BMIHistAll %>% group_by(patient) %>% filter(Month > Month_First_Antiobesity)
BMI_after_Antiobesity <- BMI_after_Antiobesity %>% group_by(patient) %>% summarize(across(everything(), min))
names(BMI_after_Antiobesity)[2] <- "Month_After"
names(BMI_after_Antiobesity)[3] <- "BMI_After"
names(BMI_after_Antiobesity)[4] <- "weight"

#join the before and after BMIs
BMI_before_Antiobesity_BEFORE_vs_AFTER <-BMI_before_Antiobesity %>% full_join(BMI_after_Antiobesity)

write.csv(BMI_before_Antiobesity_BEFORE_vs_AFTER, "BMI_before_ANY_k_BEFORE_vs_AFTER.csv")


# calculate n of months until Antiobesity and after Antiobesity until next BMI reading
BMI_before_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity_BEFORE_vs_AFTER %>% mutate(Months_until_drug = Month_First_Antiobesity - Month_Prior) %>% mutate(Month_from_drug_on = Month_After - Month_First_Antiobesity)
#percentage reduction
BMI_before_Antiobesity_BEFORE_vs_AFTER <- BMI_before_Antiobesity_BEFORE_vs_AFTER %>% mutate(BMI_reduction = ((BMI_After/BMI_Prior)-1)*100)

# image, by buckets of reduction
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% 
  select(BMI_reduction, weight) %>% 
  arrange(BMI_reduction) %>%
  mutate(stocks_red = ifelse(BMI_reduction < -60, "-100% to -60%",
                             ifelse(BMI_reduction >= -60 & BMI_reduction < -30, "-60% to -30%",
                                    ifelse(BMI_reduction >= -30 & BMI_reduction < -10, "-30% to -10%",
                                           ifelse(BMI_reduction >= -10 & BMI_reduction <0, "-10% to 0%",
                                                  ifelse(BMI_reduction >= 0 & BMI_reduction < 10, "0% to 10%",
                                                         ifelse(BMI_reduction >=10 & BMI_reduction < 30, "10% to 30%", 
                                                                ifelse(BMI_reduction >= 30 & BMI_reduction < 60 , "30% to 60%", 
                                                                       ifelse(BMI_reduction >= 60, "60% to 100%", BMI_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))


# mean BMIs before and after
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% select(BMI_Prior, BMI_After) %>%
  gather(Time, value, BMI_Prior:BMI_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))


# density distribution before and after therapy
BMI_before_Antiobesity_BEFORE_vs_AFTER %>% select(BMI_Prior, BMI_After) %>%
  gather(Time, value, BMI_Prior:BMI_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nBMI level")+ ylab("Proportion of patients \n")



# ----
# How has BMI evovled over time? --------------------------------------------
BMI_Evolution_Therapy_Lines <- read.csv("BMI_Evolution_Therapy_Lines.csv", sep=",")
names(BMI_Evolution_Therapy_Lines)[1] <- "Month"
names(BMI_Evolution_Therapy_Lines)[3] <- "Therapy"

BMI_Evolution_Therapy_Lines %>% group_by(Therapy, Month) %>% summarise(n = mean(BMI)) %>%
  ggplot(aes(x=Month, y=n, fill=Therapy, colour=Therapy))+
  geom_smooth(se=F, size=2.5)+
  ylab("BMI\n")+
  xlab("\nMonth")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggsci::scale_colour_jama()


# ----
# How much is HbA1c reduced by monotherapies?------------------------------------------------------------------------------------------------

# HbA1c reductions, Insulin 
# No insulin first 12months
# started between 12 and 48months, follow from 12 to 60 
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no insulin before month 12
Patients_no_insulin_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(!grepl("44",d1) & !grepl("45",d1) & !grepl("46",d1) & !grepl("47",d1) & !grepl("48",d1)& !grepl("49",d1) & !grepl("50",d1) & 
           !grepl("51",d1) & !grepl("52",d1) & !grepl("53",d1) & !grepl("54",d1)  & !grepl("55",d1) & !grepl("56",d1)  & !grepl("57",d1)) %>%
  select(patient)

# vector of unique patients
Patients_no_insulin_12 <- Patients_no_insulin_12 %>% distinct()

# filter for patient who did have an insulin from 12 to 48
Patients_start_insulin_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("44",d1) | grepl("45",d1) | grepl("46",d1) | grepl("47",d1) | grepl("48",d1) | grepl("49",d1) | (grepl("50",d1) | grepl("51",d1) | grepl("52",d1) | grepl("53",d1) | grepl("54",d1) | grepl("55",d1) | grepl("56",d1) | grepl("57",d1))) %>%
  filter(!grepl(",",d1)) %>%
  select(patient)

# vector of unique patients
Patients_start_insulin_12_48 <- Patients_start_insulin_12_48 %>% distinct()

# select patient intersection 
Patients_Insulin_track <- Patients_no_insulin_12 %>% inner_join(Patients_start_insulin_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on insulin status
DIA_Japan_Drug_Histories <- Patients_Insulin_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsnON insulin
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to insulin
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took Insulin
DIA_Japan_Drug_Histories_FIRST_INSULIN <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_INSULIN)[3] <- "Month_First_Insulin"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the insulin criteria
Patient_first_insulin <-  DIA_Japan_Drug_Histories_FIRST_INSULIN %>% select(patient)
HbA1cHist <- Patient_first_insulin %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_INSULIN$Month_First_Insulin <- as.character(DIA_Japan_Drug_Histories_FIRST_INSULIN$Month_First_Insulin)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first insulin month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_INSULIN, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_Insulin <- as.numeric(HbA1cHist$Month_First_Insulin)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before insulin start and months after insulin start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_insulin <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_Insulin)
HbA1cs_before_insulin <- HbA1cs_before_insulin %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_insulin)[2] <- "Month_Prior"
names(HbA1cs_before_insulin)[3] <- "HbA1c_Prior"
names(HbA1cs_before_insulin)[4] <- "weight"

HbA1cs_after_insulin <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_Insulin)
HbA1cs_after_insulin <- HbA1cs_after_insulin %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_insulin)[2] <- "Month_After"
names(HbA1cs_after_insulin)[3] <- "HbA1c_After"
names(HbA1cs_after_insulin)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_insulin_BEFORE_vs_AFTER <-HbA1cs_before_insulin %>% full_join(HbA1cs_after_insulin)

write.csv(HbA1cs_before_insulin_BEFORE_vs_AFTER, "HbA1cs_before_insulin_BEFORE_vs_AFTER_ONLY_MONOS.csv")

# calculate n of months until insulin and after insulin until next HbA1c reading
HbA1cs_before_insulin_BEFORE_vs_AFTER <- HbA1cs_before_insulin_BEFORE_vs_AFTER %>% mutate(Months_until_I = Month_First_Insulin - Month_Prior) %>% mutate(Month_from_I_on = Month_After - Month_First_Insulin)
#percentage reduction
HbA1cs_before_insulin_BEFORE_vs_AFTER <- HbA1cs_before_insulin_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
HbA1cs_before_insulin_BEFORE_vs_AFTER %>% 
  select(HbA1c_reduction, weight) %>% 
  arrange(HbA1c_reduction) %>%
  mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                             ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                    ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                           ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                  ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                         ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                       ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))


# mean HbA1cs before and after
HbA1cs_before_insulin_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))


# density distribution before and after therapy
HbA1cs_before_insulin_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")









# HbA1c reductions, GLP1 inject
# No GLP1 inject first 12months
#started between 12 and 48months, follow from 12 to 60 
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no GLP1 inject before month 12
Patients_no_GLP1_inject_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(!grepl("39",d1) & !grepl("40",d1) & !grepl("41",d1) & !grepl("42",d1) & !grepl("43",d1)) %>%
  select(patient)

# vector of unique patients
Patients_no_GLP1_inject_12 <- Patients_no_GLP1_inject_12 %>% distinct()

# filter for patient who did have an GLP1 inject from 12 to 48
Patients_start_GLP1_inject_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("G",s1)) %>%
  filter(!(grepl(",",d1))) %>%
  select(patient)

# vector of unique patients
Patients_start_GLP1_inject_12_48 <- Patients_start_GLP1_inject_12_48 %>% distinct()

# select patient intersection 
Patients_GLP1_inject_track <- Patients_no_GLP1_inject_12 %>% inner_join(Patients_start_GLP1_inject_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "GLP1_Inject"))%>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "GLP1_Inject"))%>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "GLP1_Inject"))%>% 
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "GLP1_Inject"))%>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "GLP1_Inject"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1_Inject",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on GLP1 inject status
DIA_Japan_Drug_Histories <- Patients_GLP1_inject_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON GLP1 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to GLP1 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took GLP1 inject
DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT)[3] <- "Month_First_GLP1_Inject"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the GLP1 Inject criteria
Patient_first_GLP1_Inject <-  DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT %>% select(patient)
HbA1cHist <- Patient_first_GLP1_Inject %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT$Month_First_GLP1_Inject <- as.character(DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT$Month_First_GLP1_Inject)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first insulin month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_GLP1_INJECT, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_GLP1_Inject <- as.numeric(HbA1cHist$Month_First_GLP1_Inject)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before GLP1 inject start and months after GLP1 inject start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_GLP1_Inject <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_GLP1_Inject)
HbA1cs_before_GLP1_Inject <- HbA1cs_before_GLP1_Inject %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_GLP1_Inject)[2] <- "Month_Prior"
names(HbA1cs_before_GLP1_Inject)[3] <- "HbA1c_Prior"
names(HbA1cs_before_GLP1_Inject)[4] <- "weight"

HbA1cs_after_GLP1_Inject <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_GLP1_Inject)
HbA1cs_after_GLP1_Inject <- HbA1cs_after_GLP1_Inject %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_GLP1_Inject)[2] <- "Month_After"
names(HbA1cs_after_GLP1_Inject)[3] <- "HbA1c_After"
names(HbA1cs_after_GLP1_Inject)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER <-HbA1cs_before_GLP1_Inject %>% full_join(HbA1cs_after_GLP1_Inject)

write.csv(HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER, "HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER_MONOs.csv")

# calculate n of months until GLP1 inject and after GLP1 inject until next HbA1c reading
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER <- HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% mutate(Months_until_G = Month_First_GLP1_Inject - Month_Prior) %>% mutate(Month_from_G_on = Month_After - Month_First_GLP1_Inject)
#percentage reduction
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER <- HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
data.frame(HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% 
             select(HbA1c_reduction, weight) %>% 
             arrange(HbA1c_reduction) %>%
             mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                                        ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                               ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                                      ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                             ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                                    ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                           ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                                  ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction)))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))

# mean HbA1cs before and after
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))


# density distribution before and after therapy
HbA1cs_before_GLP1_inject_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")



# SGLT2
# No SGLT2 inject first 12months
# started between 12 and 48months, follow from 12 to 60 
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)

# table long format, from Pedro
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, d1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1))

#filter for patients who had no SGLT2 inject before month 12
Patients_no_SGLT2_12 <- DIA_Flows_Aux._Long %>% 
  filter(p1 < 12) %>%
  filter(!grepl("32",d1) & !grepl("33",d1) & !grepl("34",d1) & !grepl("35",d1) & !grepl("36",d1)  & !grepl("37",d1)) %>%
  select(patient)

# vector of unique patients
Patients_no_SGLT2_12 <- Patients_no_SGLT2_12 %>% distinct()

# filter for patient who did have an SGLT2 inject from 12 to 48
Patients_start_SGLT2_12_48 <- DIA_Flows_Aux._Long %>% 
  filter(p1 >= 12 &  p1 < 48) %>%
  filter(grepl("S",s1)) %>%
  filter(!grepl(",",d1)) %>%
  select(patient)

# vector of unique patients
Patients_start_SGLT2_12_48 <- Patients_start_SGLT2_12_48 %>% distinct()

# select patient intersection 
Patients_SGLT2_track <- Patients_no_SGLT2_12 %>% inner_join(Patients_start_SGLT2_12_48)

# read table in wide format from months 1 to 60
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(15:63)

# convert no insuilins too zero, and insulins to one, then convert everything to numeric 
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "SGLT2"))%>% 
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "SGLT2"))%>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "SGLT2"))%>% 
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "SGLT2"))%>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "SGLT2"))%>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "SGLT2"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SGLT2",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

# original table again, to go fetch the patient ID and weight
DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

#add those columns
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

# filter for the patients selected based on SGLT2 inject status
DIA_Japan_Drug_Histories <- Patients_SGLT2_track %>% left_join(DIA_Japan_Drug_Histories)

#convert to long format
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month12:month60, factor_key=TRUE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient, Month)

#select those monthsn ON SGLT2 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

#recode the months, so that we can do comparisions/sortings
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Month = as.numeric(Month))

# select the min month, i.e. the month of first exposure to SGLT2 inject
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% summarize(across(everything(), min))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Treat)

# When each patient first took SGLT2 inject
DIA_Japan_Drug_Histories_FIRST_SGLT2 <- DIA_Japan_Drug_Histories
names(DIA_Japan_Drug_Histories_FIRST_SGLT2)[3] <- "Month_First_SGLT2"

# Now get the HbA1x levels
# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"

#convert to long format
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)

# pick only those months with HbA1c readings
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")

#recode
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X1", "1")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X2", "2")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X3", "3")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X4", "4")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X5", "5")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X6", "6")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X7", "7")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X8", "8")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X9", "9")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X10", "10")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X11", "11")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X12", "12")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X13", "13")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X14", "14")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X15", "15")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X16", "16")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X17", "17")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X18", "18")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X19", "19")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X20", "20")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X21", "21")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X22", "22")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X23", "23")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X24", "24")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X25", "25")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X26", "26")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X27", "27")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X28", "28")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X29", "29")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X30", "30")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X31", "31")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X32", "32")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X33", "33")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X34", "34")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X35", "35")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X36", "36")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X37", "37")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X38", "38")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X39", "39")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X40", "40")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X41", "41")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X42", "42")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X43", "43")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X44", "44")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X45", "45")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X46", "46")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X47", "47")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X48", "48")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X49", "49")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X50", "50")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X51", "51")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X52", "52")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X53", "53")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X54", "54")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X55", "55")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X56", "56")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X57", "57")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X58", "58")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X59", "59")
HbA1cHist$Month <- str_replace(HbA1cHist$Month, "X60", "60")

#some patients had more than 1 reading, separate based on commas
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
#numeric
HbA1cHist<- HbA1cHist %>% mutate(Month = as.numeric(Month)) %>% mutate(HbA1c = as.numeric(HbA1c))
#group, arrange
HbA1cHist <- HbA1cHist %>% arrange(patient, Month, HbA1c) %>% group_by(patient)
#filter for the patients that fit the SGLT2 Inject criteria
Patient_first_SGLT2 <-  DIA_Japan_Drug_Histories_FIRST_SGLT2 %>% select(patient)
HbA1cHist <- Patient_first_SGLT2 %>% left_join(HbA1cHist)
#remove those ptients with no HbA1c level readings
HbA1cHist<- HbA1cHist %>% filter(!is.na(weight))

#convert prior to joining
DIA_Japan_Drug_Histories_FIRST_SGLT2$Month_First_SGLT2 <- as.character(DIA_Japan_Drug_Histories_FIRST_SGLT2$Month_First_SGLT2)
HbA1cHist$Month <- as.character(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.character(HbA1cHist$HbA1c)

#join the patient first SGLT2 month to his HbA1c readings
HbA1cHist <- HbA1cHist %>% left_join(DIA_Japan_Drug_Histories_FIRST_SGLT2, by = c("patient" = "patient"))
#remove the weight Pedro created, use Mark's
HbA1cHist <- HbA1cHist %>% select(-weight.x)

#convert to numeric
HbA1cHist$Month <- as.numeric(HbA1cHist$Month)
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$Month_First_SGLT2 <- as.numeric(HbA1cHist$Month_First_SGLT2)
HbA1cHist$weight.y <- as.numeric(HbA1cHist$weight.y)

#now split into months before SGLT2 start and months after SGLT2 start
# pick the max of the months before (the closest)
# pick the min of the months after (the closest)
#rename
HbA1cs_before_SGLT2 <- HbA1cHist %>% group_by(patient) %>% filter(Month < Month_First_SGLT2)
HbA1cs_before_SGLT2 <- HbA1cs_before_SGLT2 %>% group_by(patient) %>% summarize(across(everything(), max))
names(HbA1cs_before_SGLT2)[2] <- "Month_Prior"
names(HbA1cs_before_SGLT2)[3] <- "HbA1c_Prior"
names(HbA1cs_before_SGLT2)[4] <- "weight"

HbA1cs_after_SGLT2 <- HbA1cHist %>% group_by(patient) %>% filter(Month > Month_First_SGLT2)
HbA1cs_after_SGLT2 <- HbA1cs_after_SGLT2 %>% group_by(patient) %>% summarize(across(everything(), min))
names(HbA1cs_after_SGLT2)[2] <- "Month_After"
names(HbA1cs_after_SGLT2)[3] <- "HbA1c_After"
names(HbA1cs_after_SGLT2)[4] <- "weight"

#join the before and after HbA1cs
HbA1cs_before_SGLT2_BEFORE_vs_AFTER <-HbA1cs_before_SGLT2 %>% full_join(HbA1cs_after_SGLT2)

write.csv(HbA1cs_before_SGLT2_BEFORE_vs_AFTER, "HbA1cs_before_SGLT2_BEFORE_vs_AFTER_ONLY_MONOS.csv")

# calculate n of months until SGLT2 and after SGLT2 until next HbA1c reading
HbA1cs_before_SGLT2_BEFORE_vs_AFTER <- HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% mutate(Months_until_S = Month_First_SGLT2 - Month_Prior) %>% mutate(Month_from_S_on = Month_After - Month_First_SGLT2)
#percentage reduction
HbA1cs_before_SGLT2_BEFORE_vs_AFTER <- HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% mutate(HbA1c_reduction = ((HbA1c_After/HbA1c_Prior)-1)*100)

# image, by buckets of reduction
HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% 
  select(HbA1c_reduction, weight) %>% 
  arrange(HbA1c_reduction) %>%
  mutate(stocks_red = ifelse(HbA1c_reduction < -60, "-100% to -60%",
                             ifelse(HbA1c_reduction >= -60 & HbA1c_reduction < -30, "-60% to -30%",
                                    ifelse(HbA1c_reduction >= -30 & HbA1c_reduction < -10, "-30% to -10%",
                                           ifelse(HbA1c_reduction >= -10 & HbA1c_reduction <=0, "-10% to 0%",
                                                  ifelse(HbA1c_reduction > 0 & HbA1c_reduction < 10, "0% to 10%",
                                                         ifelse(HbA1c_reduction >=10 & HbA1c_reduction < 30, "10% to 30%", 
                                                                ifelse(HbA1c_reduction >= 30 & HbA1c_reduction < 60 , "30% to 60%", 
                                                                       ifelse(HbA1c_reduction >= 60, "60% to 100%", HbA1c_reduction))))))))) %>%
  group_by(stocks_red) %>% mutate(sum_weights = sum(weight)) %>%
  select(stocks_red, sum_weights) %>% distinct() %>% filter(!is.na(stocks_red))

# mean HbA1cs before and after
HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>% group_by(Time) %>% summarise(n = mean(value, na.rm = T))


7.34-6.57

# density distribution before and after therapy
HbA1cs_before_SGLT2_BEFORE_vs_AFTER %>% select(HbA1c_Prior, HbA1c_After) %>%
  gather(Time, value, HbA1c_Prior:HbA1c_After) %>%
  ggplot(aes(value))+
  geom_density(aes(fill = Time), alpha =0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values= c("firebrick", "deepskyblue4"))+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")









# ----
# How many flows based on HbA1c level? --------------------------------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p2, s2, flow)
DIA_Flows_Aux._Long$p2 <- as.numeric(DIA_Flows_Aux._Long$p2)

HbA1cHist_last_month_stocks <- read.csv("HbA1cHist_last_month_stocks.csv")
HbA1cHist_last_month_stocks <- HbA1cHist_last_month_stocks %>% arrange(s2, HbA1c)

HbA1cHist_Long <- HbA1cHist_last_month_stocks %>% 
  left_join(DIA_Flows_Aux._Long, by = c("patient"="patient"))

HbA1cHist_Long <- HbA1cHist_Long %>% group_by(patient) %>% filter(p2 >= (Month-6) & p2 <= (Month+6))
HbA1cHist_Long <- HbA1cHist_Long %>% select(-p2, -s2.y)

HbA1cHist_Long %>% group_by(s2.x) %>% summarise(n= (sum(as.numeric(flow))/length(s2.x))*100)
length(unique(HbA1cHist_Long$patient)) #79895

TOP_25perc_HbA1cHist_Long <- HbA1cHist_Long %>% group_by(s2.x) %>% filter(HbA1c > quantile(HbA1c, .75))

TOP_25perc_HbA1cHist_Long %>% group_by(s2.x) %>% summarise(n= (sum(as.numeric(flow))/length(s2.x))*100)
length(unique(TOP_25perc_HbA1cHist_Long$patient)) #19165

HbA1cHist_Long %>% group_by(patient, HbA1c) %>% summarise(n = sum(as.numeric(flow))) %>%
  group_by(n) %>%
  summarise(n2 = n()) %>%
  mutate(percent=(n2/sum(n2))*100)


TOP_25perc_HbA1cHist_Long %>% group_by(patient, HbA1c) %>% summarise(n = sum(as.numeric(flow))) %>%
  group_by(n) %>%
  summarise(n2 = n()) %>%
  mutate(percent=(n2/sum(n2))*100)


HbA1cHist_Long %>% group_by(patient, HbA1c) %>% summarise(n = sum(as.numeric(flow))) %>%
  ggplot(aes(HbA1c, n))+
  geom_point(color="midnightblue")+
  scale_y_continuous(breaks = seq(0, 12, 2))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


TOP_25perc_HbA1cHist_Long %>% group_by(patient, HbA1c) %>% summarise(n = sum(as.numeric(flow))) %>%
  ggplot(aes(HbA1c, n))+
  geom_point(color="firebrick")+
  scale_y_continuous(breaks = seq(0, 12, 2))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



CUMSUM_FLOWS_HBA1c <- HbA1cHist_Long %>% group_by(patient, HbA1c) %>% summarise(n = sum(as.numeric(flow)))%>%
  arrange(-n) %>%
  ungroup() %>%
  mutate(cumSUM = cumsum(n))%>%
  mutate(cumPerc = (cumSUM/sum(n))*100)%>%
  mutate(index = seq.int(nrow(.))) %>% 
  mutate(percent_index = (index/max(index))*100)




HbA1cHist_Long %>% group_by(patient, HbA1c) %>% summarise(n = sum(as.numeric(flow))) %>%
  group_by(n)%>%
  ggplot(aes(HbA1c))+
  geom_density(color="deepskyblue4", size=2, fill="deepskyblue4", alpha=0.8)+
  geom_vline(xintercept = 5)+
  geom_vline(xintercept = 7.5)+
  geom_vline(xintercept = 10)+
  facet_wrap(~n)+
  ylab("Proportion of Patients\n")



HbA1cHist_Long %>% group_by(patient, HbA1c) %>% summarise(n = sum(as.numeric(flow))) %>%
  group_by(n)%>%
  summarise(median=median(HbA1c))


HbA1cHist_Long %>% group_by(patient, HbA1c) %>% summarise(n = sum(as.numeric(flow))) %>%
  group_by(n)%>%
  ggplot(aes(as.factor(n),HbA1c, fill=n))+
  geom_boxplot(show.legend = F, outlier.colour = "white")+
  ylim(5,10)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nNumber of Flows \nwithin the 12 months flanking HbA1c read")



# ----
# How many drugs per hba1c level? ----------------------------------------------------------------------------
HbA1cHist_last_month_stocks <- read.csv("HbA1cHist_last_month_stocks.csv")
HbA1cHist_last_month_stocks <- HbA1cHist_last_month_stocks %>% select(-s2)

HbA1cHist_last_month_stocks <- separate_rows(HbA1cHist_last_month_stocks, d2, sep = ",", convert=T)

HbA1cHist_last_month_stocks %>% filter(d2 != "-") %>% group_by(patient, HbA1c) %>% summarise(n =n()) %>%
  group_by(n)%>%
  summarise(median=median(HbA1c)) 


HbA1cHist_last_month_stocks %>% filter(d2 != "-") %>% group_by(patient, HbA1c) %>% summarise(n =n()) %>%
  ggplot(aes(as.factor(n), HbA1c, fill=n))+
  geom_boxplot(show.legend = F, outlier.colour = NA)+
  coord_cartesian(ylim = c(5, 10))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nNumber of different drugs at the time of last HbA1c read")

# HbA1c cutoffs
HbA1cHist_last_month_stocks <- read.csv("HbA1cHist_last_month_stocks.csv")

data.frame(HbA1cHist_last_month_stocks %>% 
             mutate(HbA1c = ifelse(HbA1c > 8, "Poor_Control_(>8)",
                                   ifelse(HbA1c >= 7, "Medium_Control_(7_to_8)", "Acceptable_Control_(<7)")))%>%
             group_by(s2, HbA1c, patient) %>%
             summarise(n=n()) %>%
             mutate(percent = (n/sum(n))*100))


x <- data.frame(HbA1cHist_last_month_stocks %>% 
                  mutate(HbA1c = ifelse(HbA1c > 8, "Poor_Control_(>8)",
                                        ifelse(HbA1c >= 7, "Medium_Control_(7_to_8)", "Acceptable_Control_(<7)")))%>%
                  group_by(patient, weight, s2, HbA1c)%>%
                  ungroup()%>%
                  group_by(s2, HbA1c)%>%
                  summarise(n=sum(weight)))

write.csv(x, "Acceptance_Level_HbA1c.csv")







# ----
# How long to patients stay on insulin before moving to lapsed? ---------------------------------

DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 >= 48)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% 
  filter(!grepl("44",d2) & !grepl("45",d2) & !grepl("46",d2) & !grepl("47",d2) & !grepl("48",d2)& !grepl("49",d2) & !grepl("50",d2) & 
           !grepl("51",d2) & !grepl("52",d2) & !grepl("53",d2) & !grepl("54",d2)  & !grepl("55",d2) & !grepl("56",d2)  & !grepl("57",d2)) 

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(grepl("44",d1) | grepl("45",d1) | grepl("46",d1) | grepl("47",d1) | grepl("48",d1) | grepl("49",d1) | (grepl("50",d1) | grepl("51",d1) | grepl("52",d1) | grepl("53",d1) | grepl("54",d1) | grepl("55",d1) | grepl("56",d1) | grepl("57",d1)))

DIA_Flows_Aux._Long %>% summarise(sumweight = sum(as.numeric(weight))) #1011129

####
Insulin_pats_to_track_duration <- DIA_Flows_Aux._Long %>% filter(s2 == "x") %>% select(patient, p2) %>% 
  group_by(patient) %>% summarize(across(everything(), max))
names(Insulin_pats_to_track_duration)[2] <- "max_month"
###

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")


DIA_Japan_Drug_Histories <- Insulin_pats_to_track_duration %>% left_join(DIA_Japan_Drug_Histories)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% filter(Month < max_month)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

# count (how many months) in each of this insulin periods occuring prior to his last shift to lapsed
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"

data.frame(Insulin_Periods_DIA %>% group_by(Duration) %>% summarise(n = n()))

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% group_by(patient) %>% filter(grp == max(grp))
Insulin_Periods_DIA <- Insulin_Periods_DIA %>% select(-c(grp))

data.frame(Insulin_Periods_DIA %>% group_by(Duration) %>% summarise(n = n()))










# ----
# How many patients in each stock across lines of therapy over time? --------------------------

library(data.table)
library(tidyverse)
library(hacksaw)
library(splitstackshape)
options(scipen = 999)
DIA_nrLines_Histories <- read.table("DIA_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_nrLines_Histories <- gather(DIA_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month1", "1")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month2", "2")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month3", "3")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month4", "4")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month5", "5")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month6", "6")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month7", "7")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month8", "8")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month9", "9")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month10", "10")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month11", "11")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month12", "12")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month13", "13")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month14", "14")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month15", "15")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month16", "16")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month17", "17")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month18", "18")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month19", "19")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month20", "20")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month21", "21")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month22", "22")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month23", "23")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month24", "24")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month25", "25")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month26", "26")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month27", "27")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month28", "28")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month29", "29")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month30", "30")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month31", "31")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month32", "32")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month33", "33")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month34", "34")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month35", "35")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month36", "36")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month37", "37")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month38", "38")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month39", "39")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month40", "40")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month41", "41")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month42", "42")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month43", "43")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month44", "44")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month45", "45")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month46", "46")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month47", "47")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month48", "48")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month49", "49")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month50", "50")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month51", "51")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month52", "52")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month53", "53")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month54", "54")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month55", "55")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month56", "56")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month57", "57")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month58", "58")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month59", "59")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month60", "60")


DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p2, s2)

DIA_nrLines_Histories_M2_after <- DIA_nrLines_Histories %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
DIA_nrLines_Histories_M2_after <- DIA_nrLines_Histories_M2_after %>% mutate(Month = as.numeric(Month)) %>% filter(Month >1)
names(DIA_nrLines_Histories_M2_after)[6] <- "stock"


DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, s1)

DIA_nrLines_Histories_M1 <- DIA_nrLines_Histories %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p1"))
DIA_nrLines_Histories_M1 <- DIA_nrLines_Histories_M1 %>% mutate(Month = as.numeric(Month)) %>% filter(Month == 1)
names(DIA_nrLines_Histories_M1)[6] <- "stock"

DIA_nrLines_Histories <- DIA_nrLines_Histories_M1 %>% bind_rows(DIA_nrLines_Histories_M2_after)

rm(DIA_nrLines_Histories_M1, DIA_nrLines_Histories_M2_after)

DIA_nrLines_Histories$Treat <- as.numeric(DIA_nrLines_Histories$Treat)

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "6+", Treat))

DIA_nrLines_Histories_SUMMARY <- data.frame(DIA_nrLines_Histories %>% group_by(Month, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

DIA_nrLines_Histories_SUMMARY <- DIA_nrLines_Histories_SUMMARY %>% arrange(stock, Treat, Month)

DIA_nrLines_Histories_SUMMARY$Index <- paste(DIA_nrLines_Histories_SUMMARY$stock, DIA_nrLines_Histories_SUMMARY$Treat)


plots <- DIA_nrLines_Histories_SUMMARY %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                    aes(x=Month, y=sum) + geom_line(size=1, color="midnightblue") + ylim(0,1000000) + xlim(0,62)+ ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank()))

plots[[2]]

write.csv(DIA_nrLines_Histories_SUMMARY, "DIA_nrLines_Histories_SUMMARY.csv")

data.frame(DIA_nrLines_Histories_SUMMARY %>% group_by(Treat, stock) %>% summarise(total = sum(sum)) %>%
             group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))


data.frame(DIA_nrLines_Histories_SUMMARY %>% group_by(Treat) %>% filter(Month ==60) %>% mutate(total = sum(sum))  %>% mutate(percent = (sum/total)*100))



# ----
# How many patients in each stock across lines of therapy over time? FILTERED --------------------------
# no drug 0-6months
#start 6-12months
# follow for 48months for each patient

library(data.table)
library(tidyverse)
library(hacksaw)
library(splitstackshape)
options(scipen = 999)
DIA_nrLines_Histories <- read.table("DIA_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_nrLines_Histories <- gather(DIA_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month1", "1")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month2", "2")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month3", "3")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month4", "4")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month5", "5")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month6", "6")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month7", "7")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month8", "8")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month9", "9")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month10", "10")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month11", "11")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month12", "12")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month13", "13")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month14", "14")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month15", "15")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month16", "16")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month17", "17")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month18", "18")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month19", "19")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month20", "20")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month21", "21")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month22", "22")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month23", "23")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month24", "24")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month25", "25")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month26", "26")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month27", "27")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month28", "28")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month29", "29")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month30", "30")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month31", "31")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month32", "32")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month33", "33")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month34", "34")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month35", "35")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month36", "36")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month37", "37")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month38", "38")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month39", "39")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month40", "40")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month41", "41")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month42", "42")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month43", "43")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month44", "44")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month45", "45")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month46", "46")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month47", "47")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month48", "48")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month49", "49")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month50", "50")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month51", "51")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month52", "52")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month53", "53")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month54", "54")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month55", "55")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month56", "56")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month57", "57")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month58", "58")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month59", "59")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month60", "60")

DIA_nrLines_Histories_pats <- DIA_nrLines_Histories
DIA_nrLines_Histories_pats<- DIA_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 6 were on 0 Lines (lapsed/naive)
DIA_nrLines_Histories_pats_naive <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=6) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 6) %>% select(patient) %>% distinct()


#pats that after Month 6 were started on some line
DIA_nrLines_Histories_pats_start_6_to_12 <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >6 & Month <=12) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
DIA_nrLines_Histories_pats_track <- DIA_nrLines_Histories_pats_start_6_to_12  %>% inner_join(DIA_nrLines_Histories_pats_naive)

rm(DIA_nrLines_Histories_pats_start_6_to_12, DIA_nrLines_Histories_pats_naive, DIA_nrLines_Histories)

# lines of the selected patients
DIA_nrLines_Histories_pats <- DIA_nrLines_Histories_pats_track %>% left_join(DIA_nrLines_Histories_pats)
DIA_nrLines_Histories <- DIA_nrLines_Histories_pats
rm(DIA_nrLines_Histories_pats)
rm(DIA_nrLines_Histories_pats_track)
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Treat != "0")


###
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p2, s2)

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(DIA_nrLines_Histories)[5] <- "stock"

DIA_nrLines_Histories$Treat <- as.numeric(DIA_nrLines_Histories$Treat)


#recode line number
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Month_new <49)

DIA_nrLines_Histories_SUMMARY <- data.frame(DIA_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

DIA_nrLines_Histories_SUMMARY <- DIA_nrLines_Histories_SUMMARY %>% arrange(stock, Treat, Month_new)

DIA_nrLines_Histories_SUMMARY$Index <- paste(DIA_nrLines_Histories_SUMMARY$stock, DIA_nrLines_Histories_SUMMARY$Treat)


plots <- DIA_nrLines_Histories_SUMMARY %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                    aes(x=Month_new, y=sum) + geom_area(fill="midnightblue") + xlim(0,48)+ ylim(0,50000)+ ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
)

plots[[2]]

write.csv(DIA_nrLines_Histories_SUMMARY, "DIA_nrLines_Histories_SUMMARY_FILTERED.csv")

data.frame(DIA_nrLines_Histories_SUMMARY %>% filter(Month_new == 48) %>%group_by(Treat, stock) %>% 
             summarise(total = sum(sum)) %>% group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))


# ----
# How many patients in each stock across lines of therapy over time? FILTERED non-MECE --------------------------
# no drug 0-6months
#start 6-12months
# follow for 48months for each patient

library(data.table)
library(tidyverse)
library(hacksaw)
library(splitstackshape)
options(scipen = 999)

DIA_nrLines_Histories <- read.table("DIA_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_nrLines_Histories <- gather(DIA_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month1", "1")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month2", "2")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month3", "3")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month4", "4")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month5", "5")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month6", "6")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month7", "7")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month8", "8")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month9", "9")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month10", "10")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month11", "11")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month12", "12")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month13", "13")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month14", "14")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month15", "15")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month16", "16")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month17", "17")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month18", "18")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month19", "19")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month20", "20")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month21", "21")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month22", "22")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month23", "23")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month24", "24")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month25", "25")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month26", "26")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month27", "27")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month28", "28")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month29", "29")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month30", "30")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month31", "31")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month32", "32")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month33", "33")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month34", "34")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month35", "35")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month36", "36")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month37", "37")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month38", "38")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month39", "39")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month40", "40")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month41", "41")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month42", "42")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month43", "43")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month44", "44")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month45", "45")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month46", "46")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month47", "47")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month48", "48")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month49", "49")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month50", "50")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month51", "51")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month52", "52")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month53", "53")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month54", "54")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month55", "55")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month56", "56")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month57", "57")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month58", "58")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month59", "59")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month60", "60")

DIA_nrLines_Histories_pats <- DIA_nrLines_Histories
DIA_nrLines_Histories_pats<- DIA_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 6 were on 0 Lines (lapsed/naive)
DIA_nrLines_Histories_pats_naive <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=6) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 6) %>% select(patient) %>% distinct()


#pats that after Month 6 were started on some line
DIA_nrLines_Histories_pats_start_6_to_12 <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >6 & Month <=12) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
DIA_nrLines_Histories_pats_track <- DIA_nrLines_Histories_pats_start_6_to_12  %>% inner_join(DIA_nrLines_Histories_pats_naive)

rm(DIA_nrLines_Histories_pats_start_6_to_12, DIA_nrLines_Histories_pats_naive, DIA_nrLines_Histories)

# lines of the selected patients
DIA_nrLines_Histories_pats <- DIA_nrLines_Histories_pats_track %>% left_join(DIA_nrLines_Histories_pats)
DIA_nrLines_Histories <- DIA_nrLines_Histories_pats
rm(DIA_nrLines_Histories_pats)
rm(DIA_nrLines_Histories_pats_track)
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Treat != "0")


###
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p2, d2)

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(DIA_nrLines_Histories)[5] <- "stock"

DIA_nrLines_Histories$Treat <- as.numeric(DIA_nrLines_Histories$Treat)


#recode line number
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Month_new <49)


DIA_nrLines_Histories <- separate_rows(DIA_nrLines_Histories, stock, sep = ",", convert=T )

length(unique(DIA_nrLines_Histories$patient)) #5191, all good




DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
names(DANU_Japan_Ingredients)[2] <- "stock"


DIA_nrLines_Histories <-  DIA_nrLines_Histories %>% left_join(DANU_Japan_Ingredients %>% select(stock, drug_group))

rm(DANU_Japan_Ingredients)
rm(DIA_Flows_Aux._Long)

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% mutate(drug_group = ifelse(is.na(drug_group), "Lapsed", drug_group))

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% select(-c(stock)) %>% distinct()


DIA_nrLines_Histories_SUMMARY <- data.frame(DIA_nrLines_Histories %>% group_by(Month_new, Treat, drug_group) %>% summarise(sum = sum(as.numeric(weight))))

DIA_nrLines_Histories_SUMMARY <- DIA_nrLines_Histories_SUMMARY %>% arrange(drug_group, Treat, Month_new)

DIA_nrLines_Histories_SUMMARY$Index <- paste(DIA_nrLines_Histories_SUMMARY$drug_group, DIA_nrLines_Histories_SUMMARY$Treat)


plots <- DIA_nrLines_Histories_SUMMARY %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                    aes(x=Month_new, y=sum) + geom_area(fill="midnightblue") + xlim(0,48)+ ylim(0,78880) + ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
)

plots[[2]]

write.csv(DIA_nrLines_Histories_SUMMARY, "DIA_nrLines_Histories_SUMMARY_FILTERED_nonMECE.csv")


data.frame(DIA_nrLines_Histories_SUMMARY %>% filter(Month_new == 48) %>%group_by(Treat, drug_group) %>% 
             summarise(total = sum(sum)) %>% group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))


# ----
# How many patients in each stock across lines of therapy over time? 6 to 12 FILTERED --------------------------
# no drug 0-6months
#start 6-12months
# follow for 48months for each patient

library(data.table)
library(tidyverse)
library(hacksaw)
library(splitstackshape)
options(scipen = 999)
DIA_nrLines_Histories <- read.table("DIA_nrLines_Histories.txt", 
                                    header = T, sep=",", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_nrLines_Histories <- gather(DIA_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month1", "1")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month2", "2")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month3", "3")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month4", "4")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month5", "5")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month6", "6")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month7", "7")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month8", "8")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month9", "9")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month10", "10")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month11", "11")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month12", "12")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month13", "13")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month14", "14")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month15", "15")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month16", "16")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month17", "17")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month18", "18")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month19", "19")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month20", "20")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month21", "21")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month22", "22")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month23", "23")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month24", "24")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month25", "25")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month26", "26")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month27", "27")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month28", "28")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month29", "29")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month30", "30")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month31", "31")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month32", "32")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month33", "33")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month34", "34")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month35", "35")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month36", "36")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month37", "37")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month38", "38")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month39", "39")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month40", "40")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month41", "41")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month42", "42")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month43", "43")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month44", "44")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month45", "45")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month46", "46")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month47", "47")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month48", "48")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month49", "49")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month50", "50")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month51", "51")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month52", "52")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month53", "53")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month54", "54")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month55", "55")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month56", "56")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month57", "57")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month58", "58")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month59", "59")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month60", "60")

DIA_nrLines_Histories_pats <- DIA_nrLines_Histories
DIA_nrLines_Histories_pats<- DIA_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 6 were on 0 Lines (lapsed/naive)
DIA_nrLines_Histories_pats_naive <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=6) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 6) %>% select(patient) %>% distinct()


#pats that after Month 6 were started on some line
DIA_nrLines_Histories_pats_start_6_to_12 <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >6 & Month <=12) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
DIA_nrLines_Histories_pats_track <- DIA_nrLines_Histories_pats_start_6_to_12  %>% inner_join(DIA_nrLines_Histories_pats_naive)

rm(DIA_nrLines_Histories_pats_start_6_to_12, DIA_nrLines_Histories_pats_naive, DIA_nrLines_Histories)

# lines of the selected patients
DIA_nrLines_Histories_pats <- DIA_nrLines_Histories_pats_track %>% left_join(DIA_nrLines_Histories_pats)
DIA_nrLines_Histories <- DIA_nrLines_Histories_pats
rm(DIA_nrLines_Histories_pats)
rm(DIA_nrLines_Histories_pats_track)
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Treat != "0")


###
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p2, s2)

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(DIA_nrLines_Histories)[5] <- "stock"

DIA_nrLines_Histories$Treat <- as.numeric(DIA_nrLines_Histories$Treat)


#recode line number
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Month_new <49)

DIA_nrLines_Histories_SUMMARY <- data.frame(DIA_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

DIA_nrLines_Histories_SUMMARY <- DIA_nrLines_Histories_SUMMARY %>% arrange(stock, Treat, Month_new)

DIA_nrLines_Histories_SUMMARY$Index <- paste(DIA_nrLines_Histories_SUMMARY$stock, DIA_nrLines_Histories_SUMMARY$Treat)


plots <- DIA_nrLines_Histories_SUMMARY %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                    aes(x=Month_new, y=sum) + geom_area(fill="midnightblue") + xlim(0,48)+ ylim(0,2700)+ ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
)

plots[[2]]

write.csv(DIA_nrLines_Histories_SUMMARY, "DIA_nrLines_Histories_SUMMARY_FILTERED_start_6_12.csv")



data.frame(DIA_nrLines_Histories_SUMMARY %>% filter(stock == "G") %>% 
             mutate(total=sum(sum)) %>% group_by(Treat) %>% mutate(n_line = sum(sum)) %>% mutate(percent=n_line/total) %>% select(percent) %>% distinct())

# data.frame(DIA_nrLines_Histories_SUMMARY %>% filter(Month_new == 48) %>%group_by(Treat, stock) %>% 
#              summarise(total = sum(sum)) %>% group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))









# ----
# How many patients in each stock across lines of therapy over time? 24 to 36 FILTERED --------------------------
# no drug 0-24months
#start 24-36months
# follow for 24months for each patient
library(data.table)
library(tidyverse)
library(hacksaw)
library(splitstackshape)
options(scipen = 999)
DIA_nrLines_Histories <- read.table("DIA_nrLines_Histories.txt",header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

DIA_nrLines_Histories <- gather(DIA_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month1", "1")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month2", "2")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month3", "3")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month4", "4")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month5", "5")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month6", "6")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month7", "7")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month8", "8")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month9", "9")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month10", "10")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month11", "11")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month12", "12")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month13", "13")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month14", "14")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month15", "15")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month16", "16")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month17", "17")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month18", "18")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month19", "19")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month20", "20")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month21", "21")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month22", "22")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month23", "23")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month24", "24")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month25", "25")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month26", "26")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month27", "27")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month28", "28")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month29", "29")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month30", "30")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month31", "31")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month32", "32")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month33", "33")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month34", "34")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month35", "35")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month36", "36")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month37", "37")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month38", "38")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month39", "39")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month40", "40")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month41", "41")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month42", "42")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month43", "43")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month44", "44")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month45", "45")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month46", "46")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month47", "47")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month48", "48")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month49", "49")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month50", "50")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month51", "51")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month52", "52")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month53", "53")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month54", "54")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month55", "55")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month56", "56")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month57", "57")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month58", "58")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month59", "59")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month60", "60")

DIA_nrLines_Histories_pats <- DIA_nrLines_Histories
DIA_nrLines_Histories_pats<- DIA_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 24 were on 0 Lines (lapsed/naive)
DIA_nrLines_Histories_pats_naive <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=24) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 24) %>% select(patient) %>% distinct()

#pats that after Month 24 were started on some line
DIA_nrLines_Histories_pats_start_24_to_36 <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >24 & Month <=36) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
DIA_nrLines_Histories_pats_track <- DIA_nrLines_Histories_pats_start_24_to_36  %>% inner_join(DIA_nrLines_Histories_pats_naive)

rm(DIA_nrLines_Histories_pats_start_24_to_36, DIA_nrLines_Histories_pats_naive, DIA_nrLines_Histories)

# lines of the selected patients
DIA_nrLines_Histories_pats <- DIA_nrLines_Histories_pats_track %>% left_join(DIA_nrLines_Histories_pats)
DIA_nrLines_Histories <- DIA_nrLines_Histories_pats
rm(DIA_nrLines_Histories_pats)
rm(DIA_nrLines_Histories_pats_track)
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Treat != "0")


###
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p2, s2)

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(DIA_nrLines_Histories)[5] <- "stock"

DIA_nrLines_Histories$Treat <- as.numeric(DIA_nrLines_Histories$Treat)


#recode line number
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Month_new <25)

DIA_nrLines_Histories_SUMMARY <- data.frame(DIA_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

DIA_nrLines_Histories_SUMMARY <- DIA_nrLines_Histories_SUMMARY %>% arrange(stock, Treat, Month_new)

DIA_nrLines_Histories_SUMMARY$Index <- paste(DIA_nrLines_Histories_SUMMARY$stock, DIA_nrLines_Histories_SUMMARY$Treat)


plots <- DIA_nrLines_Histories_SUMMARY %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                    aes(x=Month_new, y=sum) + geom_area(fill="midnightblue") + xlim(0,48) + ylim(0,2800) + ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
)

plots[[2]]


write.csv(DIA_nrLines_Histories_SUMMARY, "DIA_nrLines_Histories_SUMMARY_FILTERED_start_24_36.csv")




data.frame(DIA_nrLines_Histories_SUMMARY %>% filter(stock == "G") %>% 
             mutate(total=sum(sum)) %>% group_by(Treat) %>% mutate(n_line = sum(sum)) %>% mutate(percent=n_line/total) %>% select(percent) %>% distinct())


data.frame(DIA_nrLines_Histories_SUMMARY %>% filter(Month_new == 24) %>%group_by(Treat, stock) %>% 
             summarise(total = sum(sum)) %>% group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))




# ----
# How many patients in each stock across lines of therapy over time? 36 to 48 FILTERED --------------------------
# no drug 0-36months
#start 36-48months
# follow for 12months for each patient
library(data.table)
library(tidyverse)
library(hacksaw)
library(splitstackshape)
options(scipen = 999)
DIA_nrLines_Histories <- read.table("DIA_nrLines_Histories.txt",header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

DIA_nrLines_Histories <- gather(DIA_nrLines_Histories, Month, Treat, month1:month60, factor_key=TRUE)

DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month1", "1")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month2", "2")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month3", "3")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month4", "4")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month5", "5")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month6", "6")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month7", "7")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month8", "8")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month9", "9")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month10", "10")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month11", "11")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month12", "12")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month13", "13")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month14", "14")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month15", "15")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month16", "16")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month17", "17")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month18", "18")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month19", "19")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month20", "20")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month21", "21")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month22", "22")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month23", "23")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month24", "24")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month25", "25")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month26", "26")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month27", "27")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month28", "28")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month29", "29")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month30", "30")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month31", "31")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month32", "32")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month33", "33")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month34", "34")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month35", "35")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month36", "36")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month37", "37")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month38", "38")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month39", "39")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month40", "40")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month41", "41")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month42", "42")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month43", "43")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month44", "44")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month45", "45")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month46", "46")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month47", "47")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month48", "48")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month49", "49")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month50", "50")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month51", "51")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month52", "52")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month53", "53")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month54", "54")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month55", "55")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month56", "56")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month57", "57")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month58", "58")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month59", "59")
DIA_nrLines_Histories$Month <- str_replace(DIA_nrLines_Histories$Month, "month60", "60")

DIA_nrLines_Histories_pats <- DIA_nrLines_Histories
DIA_nrLines_Histories_pats<- DIA_nrLines_Histories_pats %>% select(-c(disease))

#pats that up until Month 24 were on 0 Lines (lapsed/naive)
DIA_nrLines_Histories_pats_naive <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month <=36) %>% 
  filter(Treat == 0) %>% group_by(patient) %>% summarise(n = n()) %>% filter(n == 36) %>% select(patient) %>% distinct()

#pats that after Month 24 were started on some line
DIA_nrLines_Histories_pats_start_36_to_48 <- DIA_nrLines_Histories_pats %>% select(patient, weight, Month, Treat) %>% 
  mutate(Month = as.numeric(Month)) %>% mutate(Treat = as.numeric(Treat)) %>% filter(Month >36 & Month <=48) %>% 
  filter(Treat != 0) %>% select(patient) %>% distinct()

#pats to track
DIA_nrLines_Histories_pats_track <- DIA_nrLines_Histories_pats_start_36_to_48  %>% inner_join(DIA_nrLines_Histories_pats_naive)

rm(DIA_nrLines_Histories_pats_start_24_to_36, DIA_nrLines_Histories_pats_naive, DIA_nrLines_Histories)

# lines of the selected patients
DIA_nrLines_Histories_pats <- DIA_nrLines_Histories_pats_track %>% left_join(DIA_nrLines_Histories_pats)
DIA_nrLines_Histories <- DIA_nrLines_Histories_pats
rm(DIA_nrLines_Histories_pats)
rm(DIA_nrLines_Histories_pats_track)
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Treat != "0")


###
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p2, s2)

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "weight"="weight", "Month"="p2"))
names(DIA_nrLines_Histories)[5] <- "stock"

DIA_nrLines_Histories$Treat <- as.numeric(DIA_nrLines_Histories$Treat)


#recode line number
DIA_nrLines_Histories <- DIA_nrLines_Histories %>% mutate(Treat = ifelse(Treat>6, "7+", Treat))

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% group_by(patient) %>% mutate(Month_new = row_number())

DIA_nrLines_Histories <- DIA_nrLines_Histories %>% filter(Month_new <12)

DIA_nrLines_Histories_SUMMARY <- data.frame(DIA_nrLines_Histories %>% group_by(Month_new, Treat, stock) %>% summarise(sum = sum(as.numeric(weight))))

DIA_nrLines_Histories_SUMMARY <- DIA_nrLines_Histories_SUMMARY %>% arrange(stock, Treat, Month_new)

DIA_nrLines_Histories_SUMMARY$Index <- paste(DIA_nrLines_Histories_SUMMARY$stock, DIA_nrLines_Histories_SUMMARY$Treat)


plots <- DIA_nrLines_Histories_SUMMARY %>% group_by(Index) %>% do(plots=ggplot(data=.) +
                                                                    aes(x=Month_new, y=sum) + geom_area(fill="midnightblue") + xlim(0,48) + ylim(0,2800) + ylab("Total Population") + ggtitle(unique(.$Index)) + theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
)

plots[[2]]

write.csv(DIA_nrLines_Histories_SUMMARY, "DIA_nrLines_Histories_SUMMARY_FILTERED_start_36_48.csv")


data.frame(DIA_nrLines_Histories_SUMMARY %>% filter(stock == "G") %>% 
             mutate(total=sum(sum)) %>% group_by(Treat) %>% mutate(n_line = sum(sum)) %>% mutate(percent=n_line/total))


data.frame(DIA_nrLines_Histories_SUMMARY %>% filter(Month_new == 11) %>%group_by(Treat, stock) %>% 
             summarise(total = sum(sum)) %>% group_by(Treat) %>% mutate(global = sum(total)) %>% mutate(percent = (total/global)*100))






# ----
# How long do patients stay on SGLT2, GLP1 AND DPP4, etc. over the 5 year period? ----------------------------

#SGLT2
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2 too zero, and SGLT2 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "SGLT2"))%>% 
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "SGLT2"))%>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "SGLT2"))%>% 
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "SGLT2"))%>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "SGLT2"))%>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "SGLT2"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SGLT2",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% mutate(weight = as.numeric(weight))

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% distinct()

# 
# library(spatstat)
weighted.median(SGLT2_Periods_DIA$Total_Duration, SGLT2_Periods_DIA$weight) #24.5

data.frame(SGLT2_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))

SGLT2_Periods_DIA %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))


# GLP1
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLP1 too zero, and GLP1 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "GLP1"))%>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "GLP1"))%>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "GLP1"))%>% 
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "GLP1"))%>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "GLP1"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these GLP1 periods!
GLP1_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(GLP1_Periods_DIA)[3] <- "Duration"

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% mutate(weight = as.numeric(weight))

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% distinct() 

weighted.median(GLP1_Periods_DIA$Total_Duration, GLP1_Periods_DIA$weight) #17.5

data.frame(GLP1_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))

GLP1_Periods_DIA %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))



weighted.mean(GLP1_Periods_DIA$Total_Duration, GLP1_Periods_DIA$weight) #22.98089



#DPP4
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "DPP4"))%>% 
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "DPP4"))%>% 
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "DPP4"))%>% 
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "DPP4"))%>%
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "DPP4"))%>%
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "DPP4"))%>% 
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "DPP4"))%>% 
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "DPP4"))%>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "DPP4"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="DPP4",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()


weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) # 49.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


# 

DPP4_Periods_DIA %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))



weighted.mean(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #40.91224






#Insulin
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>% 
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>% 
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>% 
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>% 
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these Insulin periods!
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% mutate(weight = as.numeric(weight))

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% distinct()


weighted.median(Insulin_Periods_DIA$Total_Duration, Insulin_Periods_DIA$weight) #2.5

data.frame(Insulin_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))

# 

Insulin_Periods_DIA %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))


weighted.mean(Insulin_Periods_DIA$Total_Duration, Insulin_Periods_DIA$weight) #18.49044










#Biguanide
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no biguanide too zero, and biguanide to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "Biguanide"))%>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "Biguanide"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Biguanide",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these Biguanide periods!
Biguanide_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Biguanide_Periods_DIA)[3] <- "Duration"

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% mutate(weight = as.numeric(weight))

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% distinct()

weighted.median(Biguanide_Periods_DIA$Total_Duration, Biguanide_Periods_DIA$weight) #45.5

data.frame(Biguanide_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



Biguanide_Periods_DIA %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))


# 
weighted.mean(Biguanide_Periods_DIA$Total_Duration, Biguanide_Periods_DIA$weight) #39.11489









#Antidiabetic
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

sum(as.numeric(DIA_Japan_Drug_Histories$weight))
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no Antidiabetic too zero, and Antidiabetic to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(17{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "Antidiabetic"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antidiabetic",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these antidiabetic periods!
Antidiabetic_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(Antidiabetic_Periods_DIA)[3] <- "Duration"

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% mutate(weight = as.numeric(weight))

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% distinct()


weighted.median(Antidiabetic_Periods_DIA$Total_Duration, Antidiabetic_Periods_DIA$weight) #45.5

data.frame(Antidiabetic_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


Antidiabetic_Periods_DIA %>% 
  mutate(Total_Duration_bucket = ifelse(Total_Duration == 1, "1", 
                                        ifelse(Total_Duration >1 & Total_Duration < 6, "2 to 6",
                                               ifelse(Total_Duration>=6 & Total_Duration <12, "6 to 12",
                                                      ifelse(Total_Duration>=12&Total_Duration<24, "12 to 24",
                                                             ifelse(Total_Duration>=24&Total_Duration<36, "24 to 36",
                                                                    ifelse(Total_Duration>=36&Total_Duration<48, "36 to 48",
                                                                           ifelse(Total_Duration>=48&Total_Duration<60, "48 to 60", "60")))))))) %>%
  group_by(Total_Duration_bucket) %>%
  summarise(pats = sum(weight))



weighted.mean(Antidiabetic_Periods_DIA$Total_Duration, Antidiabetic_Periods_DIA$weight) #36.54818







# ----
# Ternary plot GLP1s -------------------

install.packages('Ternary')
library(Ternary)


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(52:63)

# convert no GLP1 too zero, and GLP1 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "GLP1"))%>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "GLP1"))%>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "GLP1"))%>% 
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "GLP1"))%>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "GLP1"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP1",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(TOTAL = rowSums(DIA_Japan_Drug_Histories))

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)

GLP1_pats_12m <- DIA_Japan_Drug_Histories %>% filter(TOTAL != 0) %>% select(patient)


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)


GLP1_pats_12m <- GLP1_pats_12m %>% left_join(DIA_Japan_Drug_Histories) %>% select(patient, weight, month49:month60)

GLP1_pats_12m <- gather(GLP1_pats_12m, Month, Treat, month49:month60, factor_key=TRUE)

GLP1_pats_12m <- separate_rows(GLP1_pats_12m, Treat, sep = ",", convert=T )
names(GLP1_pats_12m)[4] <- "molecule"

DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% separate(drug_id, c('class', 'molecule'))
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% select(molecule, drug_group)

GLP1_pats_12m <- GLP1_pats_12m %>% left_join(DANU_Japan_Ingredients)
GLP1_pats_12m <- GLP1_pats_12m %>% select(-c(Month, molecule))

GLP1_pats_12m <- GLP1_pats_12m %>% mutate(drug_group = ifelse(drug_group == "GLP1 Injectable", drug_group,
                                                              ifelse(drug_group == "Insulin", drug_group, "other")))

GLP1_pats_12m <- GLP1_pats_12m %>% filter(!is.na(drug_group))

GLP1_pats_12m_SUMMARY <- GLP1_pats_12m %>% group_by(patient, weight, drug_group) %>% count() %>%
  ungroup() %>% group_by(patient, weight) %>% mutate(total = sum(n)) %>% mutate(percent=(n/total)*100)

GLP1_pats_12m_SUMMARY_wide <- GLP1_pats_12m_SUMMARY %>% select(patient, drug_group, percent)

GLP1_pats_12m_SUMMARY_wide <- GLP1_pats_12m_SUMMARY_wide %>% pivot_wider(names_from = drug_group, values_from = percent)

GLP1_pats_12m_SUMMARY_wide[is.na(GLP1_pats_12m_SUMMARY_wide)] <- 0

coordinates <- GLP1_pats_12m_SUMMARY_wide %>% ungroup %>% select(`GLP1 Injectable`, Insulin, other)

write.csv(GLP1_pats_12m_SUMMARY_wide, "GLP1_pats_12m_SUMMARY_wide.csv")
write.csv(coordinates, "GLP1_pats_12m_SUMMARY_wide_coordinates_Ternary.csv")

TernaryPlot(axis.labels = seq(0, 100, by = 10))
ColourTernary(TernaryDensity(coordinates, resolution = 50L))
TernaryPoints(coordinates, col = 'white', pch = 1, cex = 2)
TernaryDensityContour(coordinates, resolution = 60L)
?TernaryDensityContour

library(plotly)
names(coordinates)[1] <- "GLP1"

fig <- coordinates %>% plot_ly()
fig <- fig %>% add_trace(type = 'scatterternary',mode = 'markers',a = ~GLP1,
                         b = ~Insulin,
                         c = ~other,
                         marker = list( 
                           symbol = 100,
                           color = '#DB7365',
                           size = 14,
                           line = list('width' = 2)
                         )
)


fig <- fig %>% layout(
  title = "Simple Ternary Plot with Markers",
  ternary = list(
    aaxis = axis('GLP1'),
    baxis = axis('Insulin'),
    caxis = axis('other')
  )
)

fig

# ----
# Chord diagrams --------------------------------------------------------------------
Chord_Dig_m60 <- read.csv("Chord_Dig_m60.csv")
names(Chord_Dig_m60)[1] <- "Lapsed"

colnames(Chord_Dig_m60) <- c("Lapsed", "Biguanide", "Antidiabetic", "DPP4", "SGLT2", "GLP1 O", "GLP1 I", "Insulin")
rownames(Chord_Dig_m60) <- colnames(Chord_Dig_m60)

Chord_Dig_m60 <- as.matrix(Chord_Dig_m60)

circos.clear()

cols <- hcl.colors(8, "Viridis")

chordDiagram(Chord_Dig_m60, 
             grid.col = cols, 
             directional = 1, 
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.04, 
             annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow", 
             link.sort = TRUE, 
             link.largest.ontop = TRUE,
             annotationTrack = c("grid","name"),  
             transparency = 0.2)


Chord_Dig_m60_0diag <- read.csv("Chord_Dig_m60_0diag.csv")
names(Chord_Dig_m60_0diag)[1] <- "Lapsed"

colnames(Chord_Dig_m60_0diag) <- c("Lapsed", "Biguanide", "Antidiabetic", "DPP4", "SGLT2", "GLP1 O", "GLP1 I", "Insulin")
rownames(Chord_Dig_m60_0diag) <- colnames(Chord_Dig_m60_0diag)

Chord_Dig_m60_0diag <- as.matrix(Chord_Dig_m60_0diag)

circos.clear()

cols <- hcl.colors(8, "Viridis")

chordDiagram(Chord_Dig_m60_0diag, 
             grid.col = cols, 
             directional = 1, 
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.04, 
             annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow", 
             link.sort = TRUE, 
             link.largest.ontop = TRUE,
             annotationTrack = c("grid","name"),  
             transparency = 0.2)


# ----
# How many patients tried each of the classes ? ----------------------------
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% separate(drug_id, c('class', 'molecule'))


DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, 
                                       sep="\t", colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-c(disease))

length(unique(DIA_Japan_Drug_Histories$patient)) # 96910
sum(as.numeric(DIA_Japan_Drug_Histories$weight)) #7967485

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)
DIA_Japan_Drug_Histories <- separate_rows(DIA_Japan_Drug_Histories, Treat, sep = ",", convert=T )
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat != "-")

names(DIA_Japan_Drug_Histories)[4] <- "molecule"

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-c(Month))

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, drug_group)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% distinct()

DIA_Japan_Drug_Histories %>%
  group_by(drug_group) %>%
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  mutate(sum_weights_percent = (sum_weights / 7967485)*100) %>%
  filter(!is.na(drug_group))




# ----
# How many patients tried 2, 3, 4, 5 classes (any class) ? ------------------------------------------
# (cont from above)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(drug_group)$lengths %>% {rep(seq(length(.)), .)})

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight, grp)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient, weight) %>%
  summarize(across(everything(), max))

DIA_Japan_Drug_Histories %>% ungroup() %>% group_by(grp) %>% summarise(total=sum(as.numeric(weight)))





# ----
#  What's the share of fixed combos (/s) ? Over time / By class--------------------------------------------
DIA_Japan_Medications <- read.table("DANU Japan Medications.txt", 
                                    header = T, sep="\t", quote="", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Medications <- DIA_Japan_Medications %>% select(drug_id, med_ingredient)


Dia_Japan_Doses <- read.table("Dia Japan Doses_v2.txt", 
                              header = T, sep=",", quote="", 
                              colClasses = "character", stringsAsFactors = FALSE)

Dia_Japan_Doses <- Dia_Japan_Doses %>% select(drug_id, pat_id, weight, from_dt)
Dia_Japan_Doses$from_dt <- as.Date(Dia_Japan_Doses$from_dt)
Dia_Japan_Doses$new_date <- format(Dia_Japan_Doses$from_dt , "%Y-%m")

Dia_Japan_Doses <- Dia_Japan_Doses %>% left_join(DIA_Japan_Medications)

Dia_Japan_Doses <- Dia_Japan_Doses %>% arrange(new_date)

data.frame(Dia_Japan_Doses %>% group_by(new_date)   %>% summarise(n = sum(as.numeric(weight))))

data.frame(Dia_Japan_Doses %>% group_by(new_date) %>%filter(grepl("/",med_ingredient)) %>% summarise(n = sum(as.numeric(weight))))



DIA_Japan_Medications <- read.table("DANU Japan Medications.txt", 
                                    header = T, sep="\t", quote="", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Medications <- DIA_Japan_Medications %>% select(drug_id, drug_group, med_ingredient)


Dia_Japan_Doses <- read.table("Dia Japan Doses_v2.txt", 
                              header = T, sep=",", quote="", 
                              colClasses = "character", stringsAsFactors = FALSE)

Dia_Japan_Doses <- Dia_Japan_Doses %>% select(drug_id, pat_id, weight, from_dt)
Dia_Japan_Doses$from_dt <- as.Date(Dia_Japan_Doses$from_dt)
Dia_Japan_Doses <- Dia_Japan_Doses %>% arrange(from_dt)

library(tibbletime)
Dia_Japan_Doses <- as_tbl_time(Dia_Japan_Doses, index = from_dt)
Dia_Japan_Doses <- filter_time(Dia_Japan_Doses, time_formula = '2016-05' ~ '2021-04')

Dia_Japan_Doses$new_date <- format(Dia_Japan_Doses$from_dt , "%Y-%m")

Dia_Japan_Doses <- Dia_Japan_Doses %>% left_join(DIA_Japan_Medications)



data.frame(Dia_Japan_Doses %>% group_by(drug_group) %>%filter(!grepl("/",med_ingredient)) %>% summarise(n = sum(as.numeric(weight))))


data.frame(Dia_Japan_Doses %>% group_by(drug_group) %>%filter(grepl("/",med_ingredient)) %>% summarise(n = sum(as.numeric(weight))))



# ----
# PATHS TO GLP1 INJECTBALE ----------------------------------------------------------------

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, p2, d1, d2, s1, s2)

GLP1_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("39",d2) | grepl("40",d2) | grepl("41",d2) | grepl("42",d2) | grepl("43",d2) |
           grepl("39",d1) | grepl("40",d1) | grepl("41",d1) | grepl("42",d1) | grepl("43",d1)) %>% select(patient) %>% distinct()


GLP1_Patients <- GLP1_Patients %>% left_join(DIA_Japan_Drug_Histories)
GLP1_Patients <- GLP1_Patients %>% select(-c(disease))
GLP1_Patients <- gather(GLP1_Patients, Month, Treat, month1:month60, factor_key=TRUE)
GLP1_Patients <- separate_rows(GLP1_Patients, Treat, sep = ",", convert=T )
names(GLP1_Patients)[4] <- "molecule"

DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% select(molecule, drug_group)

GLP1_Patients<- GLP1_Patients %>% left_join(DANU_Japan_Ingredients) %>%  arrange(patient)



GLP1_Patients_first_48m <- GLP1_Patients %>% group_by(patient) %>% filter(Month == "month1" | Month == "month2" | Month == "month3" | Month == "month4" |
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
  filter(drug_group == "GLP1 Injectable") %>% select(patient) %>% distinct()

GLP1_Patients <- GLP1_Patients %>% anti_join(GLP1_Patients_first_48m)

GLP1_Patients2 <- GLP1_Patients %>% group_by(patient, weight) %>% 
  slice(if(any(drug_group == "GLP1 Injectable")) 1:which.max(drug_group == "GLP1 Injectable") else row_number())   


# Paths_to_GLP1 <- GLP1_Patients2 %>% ungroup %>% select(-c(molecule)) %>% group_by(patient, Month, drug_group) %>% 
#   distinct() %>% ungroup %>% group_by(patient, Month) %>% 
#   mutate(paths_month = paste(drug_group, collapse=" + ")) %>% 
#   ungroup() %>% group_by(patient, weight, paths_month) %>% 
#   select(patient,weight, paths_month) %>% distinct() %>%
#   select(patient, weight, paths_month) %>% ungroup()
# 
# write.csv(Paths_to_GLP1, "Paths_to_GLP1.csv")
# 
# Month_prior_to_GLP1_combos <- data.frame(GLP1_Patients2 %>% ungroup %>% select(-c(molecule)) %>% group_by(patient, Month, drug_group) %>% 
#                                            distinct() %>% ungroup %>% group_by(patient, Month) %>% 
#                                            mutate(paths_month = paste(drug_group, collapse=" + ")) %>% 
#                                            ungroup() %>% group_by(patient, weight, paths_month) %>% 
#                                            select(patient,weight, paths_month) %>% distinct() %>%
#                                            select(patient, weight, paths_month) %>% ungroup() %>% group_by(patient) %>% 
#                                            filter(row_number() == (n() - 1)) %>% ungroup() %>% group_by(paths_month) %>%
#                                            summarise(n = sum(as.numeric(weight))) %>% arrange(-n))
# 
# write.csv(Month_prior_to_GLP1_combos, "Month_prior_to_GLP1_combos.csv")
# 

GLP1_Patients2_stocks <- GLP1_Patients2

GLP1_Patients2_stocks$Month <- as.character(GLP1_Patients2_stocks$Month)
GLP1_Patients2_stocks$Month <- parse_number(GLP1_Patients2_stocks$Month)


DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>%  mutate(p2=as.numeric(p2)) %>% select(patient, p2, s2)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 ==1)

GLP1_Patients2_stocks <- GLP1_Patients2_stocks %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "Month"="p2"))

GLP1_Patients2_stocks <- GLP1_Patients2_stocks %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "Month"="p1"))

GLP1_Patients2_stocks <- GLP1_Patients2_stocks %>% mutate(s2 = ifelse(is.na(s2), s1, s2))
GLP1_Patients2_stocks <- GLP1_Patients2_stocks%>%select(-c(s1))
names(GLP1_Patients2_stocks)[6] <- "stock"



GLP1_Patients2_stocks <- GLP1_Patients2_stocks %>% filter(stock!="x")  %>% filter(stock!="g")

GLP1_Patients2_stocks_COPY <- GLP1_Patients2_stocks

GLP1_Patients2_stocks <- GLP1_Patients2_stocks %>% filter(stock!="I")




data.frame(GLP1_Patients2_stocks %>% ungroup %>% select(-c(molecule, drug_group)) %>% group_by(patient, weight, Month, stock) %>% 
             distinct() %>% ungroup %>% group_by(patient) %>% 
             filter(row_number() >= (n() - 12)) %>%
             ungroup() %>% group_by(patient) %>%
             mutate(num = row_number()) %>% ungroup() %>% group_by(num, stock) %>%
             summarise(total = sum(as.numeric(weight))))


# Stocks_evol_to_GLP1_13m_each_box <- data.frame(GLP1_Patients2_stocks %>% ungroup %>% select(-c(molecule, drug_group)) %>% group_by(patient, weight, Month, stock) %>% 
#                                                  distinct() %>% ungroup %>% group_by(patient) %>% 
#                                                  filter(row_number() >= (n() - 12)) %>%
#                                                  ungroup() %>% 
#                                                  select(patient, weight, stock) %>% 
#                                                  group_by(patient, weight) %>% 
#                                                  mutate(paths_stocks = paste(stock, collapse="->")) %>% 
#                                                  ungroup() %>% select(-c(stock)) %>% distinct() %>% group_by(paths_stocks) %>%
#                                                  summarise(total = sum(as.numeric(weight))) %>% arrange(-total))
# 
# write.csv(Stocks_evol_to_GLP1_13m_each_box, "Stocks_evol_to_GLP1_13m_each_box.csv")




Stocks_evol_to_GLP1_13m <- data.frame(GLP1_Patients2_stocks %>% ungroup %>% select(-c(molecule, drug_group)) %>% group_by(patient, weight, Month, stock) %>% 
                                        distinct() %>% ungroup %>% group_by(patient) %>% 
                                        filter(row_number() >= (n() - 12)) %>%
                                        ungroup() %>% 
                                        select(patient, weight, stock) %>% 
                                        group_by(patient, weight) %>% 
                                        mutate(paths_stocks = paste(stock, collapse="->")) %>% 
                                        ungroup() %>% select(-c(stock)) %>% distinct() %>% group_by(paths_stocks) %>%
                                        summarise(total = sum(as.numeric(weight))) %>% arrange(-total))


Stocks_evol_to_GLP1_13m <- Stocks_evol_to_GLP1_13m %>% mutate(ID = row_number())

Stocks_evol_to_GLP1_13m <- separate_rows(Stocks_evol_to_GLP1_13m, paths_stocks, sep = "->", convert=T )

Stocks_evol_to_GLP1_13m <- data.frame(Stocks_evol_to_GLP1_13m %>% group_by(ID) %>% filter(row_number()==1 | lag(paths_stocks) != paths_stocks))

Stocks_evol_to_GLP1_13m <- Stocks_evol_to_GLP1_13m %>% group_by(ID) %>% mutate(paths_stocks = paste(paths_stocks, collapse="->")) %>% 
  ungroup() %>% group_by(ID, total, paths_stocks) %>% distinct()

data.frame(Stocks_evol_to_GLP1_13m)

Stocks_evol_to_GLP1_13m <- Stocks_evol_to_GLP1_13m %>% ungroup() %>% select(-c(ID)) %>% group_by(paths_stocks) %>% summarise(pats=sum(total))


write.csv(Stocks_evol_to_GLP1_13m, "Stocks_evol_to_GLP1_13m_unique_shifts_Stocks_evol_to_GLP1_Inj_EXC_LAPSED_ORAL_INSULIN_Japan.csv")





# ----
# PATHS TO GLP1 INJECTBALE + Insuilin status + Flows  ------------- 
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, p2, d1, d2, s1, s2)

GLP1_Patients <- DIA_Flows_Aux._Long %>% 
  filter(grepl("39",d2) | grepl("40",d2) | grepl("41",d2) | grepl("42",d2) | grepl("43",d2) |
           grepl("39",d1) | grepl("40",d1) | grepl("41",d1) | grepl("42",d1) | grepl("43",d1)) %>% select(patient) %>% distinct()

GLP1_Patients <- GLP1_Patients %>% left_join(DIA_Japan_Drug_Histories)
GLP1_Patients <- GLP1_Patients %>% select(-c(disease))
GLP1_Patients <- gather(GLP1_Patients, Month, Treat, month1:month60, factor_key=TRUE)
GLP1_Patients <- separate_rows(GLP1_Patients, Treat, sep = ",", convert=T )
names(GLP1_Patients)[4] <- "molecule"

DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% select(molecule, drug_group)

GLP1_Patients<- GLP1_Patients %>% left_join(DANU_Japan_Ingredients) %>%  arrange(patient)

GLP1_Patients_first_48m <- GLP1_Patients %>% group_by(patient) %>% filter(Month == "month1" | Month == "month2" | Month == "month3" | Month == "month4" |
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
  filter(drug_group == "GLP1 Injectable") %>% select(patient) %>% distinct()

# patients with NO GLP1 in the first 48m
GLP1_Patients <- GLP1_Patients %>% anti_join(GLP1_Patients_first_48m)

#pick all months up until first GLP1, icnluding it
GLP1_Patients2 <- GLP1_Patients %>% group_by(patient, weight) %>% 
  slice(if(any(drug_group == "GLP1 Injectable")) 1:which.max(drug_group == "GLP1 Injectable") else row_number())   

Paths_to_GLP1 <- data.frame(GLP1_Patients2 %>% ungroup %>% select(-c(molecule)) %>% group_by(patient, Month, drug_group) %>% 
                              distinct() %>% ungroup %>% group_by(patient, Month) %>% 
                              mutate(paths_month = paste(drug_group, collapse=" + ")) %>% 
                              ungroup() %>% group_by(patient, weight, paths_month) %>% 
                              select(patient,weight, paths_month) %>% distinct() %>%
                              select(patient, weight, paths_month) %>% ungroup())

write.csv(Paths_to_GLP1, "Paths_to_GLP1.csv")

length(unique(GLP1_Patients2$patient)) #1421

Month_prior_to_GLP1_combos <- data.frame(GLP1_Patients2 %>% ungroup %>% select(-c(molecule)) %>% group_by(patient, Month, drug_group) %>% 
                                           distinct() %>% ungroup %>% group_by(patient, Month) %>% 
                                           mutate(paths_month = paste(drug_group, collapse=" + ")) %>% 
                                           ungroup() %>% group_by(patient, weight, paths_month) %>% 
                                           select(patient,weight, paths_month) %>% distinct() %>%
                                           select(patient, weight, paths_month) %>% ungroup() %>% group_by(patient) %>% 
                                           filter(row_number() == (n() - 1)) %>% ungroup() %>% group_by(paths_month) %>%
                                           summarise(n = sum(as.numeric(weight))) %>% arrange(-n))

write.csv(Month_prior_to_GLP1_combos, "Month_prior_to_GLP1_combos.csv")

GLP1_Patients2_INSULIN_STATUS <- GLP1_Patients2 %>% ungroup %>% select(-c(molecule)) %>% group_by(patient, Month, drug_group) %>% 
  distinct() %>% ungroup %>% group_by(patient, Month) %>% 
  mutate(paths_month = paste(drug_group, collapse=" + ")) %>% 
  ungroup() %>% group_by(patient, weight, paths_month) %>% 
  select(patient,weight, paths_month) %>% distinct() %>%
  select(patient, weight, paths_month) %>% ungroup() %>% group_by(patient) %>% 
  filter(row_number() == (n())) %>% mutate(InsulinStatus = ifelse(grepl("Insulin",paths_month), "YES", "NO")) %>%
  select(patient, InsulinStatus)

Month_prior_to_GLP1_combos_w_INSULIN_STATUS <- data.frame(GLP1_Patients2 %>% ungroup %>% select(-c(molecule)) %>% group_by(patient, Month, drug_group) %>% 
                                                            distinct() %>% ungroup %>% group_by(patient, Month) %>% 
                                                            mutate(paths_month = paste(drug_group, collapse=" + ")) %>% 
                                                            ungroup() %>% group_by(patient, weight, paths_month) %>% 
                                                            select(patient,weight, paths_month) %>% distinct() %>%
                                                            select(patient, weight, paths_month) %>% ungroup() %>% group_by(patient) %>% 
                                                            filter(row_number() == (n() - 1)) %>% left_join(GLP1_Patients2_INSULIN_STATUS) %>% ungroup() %>% group_by(paths_month, InsulinStatus) %>%
                                                            summarise(n = sum(as.numeric(weight))) %>% arrange(paths_month))

write.csv(Month_prior_to_GLP1_combos_w_INSULIN_STATUS, "Month_prior_to_GLP1_combos_w_INSULIN_STATUS.csv")

GLP1_Patients2_stocks <- GLP1_Patients2

GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month1", "1")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month2", "2")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month3", "3")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month4", "4")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month5", "5")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month6", "6")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month7", "7")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month8", "8")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month9", "9")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month10", "10")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month11", "11")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month12", "12")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month13", "13")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month14", "14")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month15", "15")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month16", "16")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month17", "17")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month18", "18")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month19", "19")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month20", "20")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month21", "21")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month22", "22")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month23", "23")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month24", "24")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month25", "25")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month26", "26")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month27", "27")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month28", "28")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month29", "29")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month30", "30")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month31", "31")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month32", "32")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month33", "33")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month34", "34")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month35", "35")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month36", "36")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month37", "37")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month38", "38")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month39", "39")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month40", "40")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month41", "41")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month42", "42")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month43", "43")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month44", "44")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month45", "45")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month46", "46")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month47", "47")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month48", "48")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month49", "49")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month50", "50")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month51", "51")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month52", "52")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month53", "53")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month54", "54")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month55", "55")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month56", "56")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month57", "57")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month58", "58")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month59", "59")
GLP1_Patients2_stocks$Month <- str_replace(GLP1_Patients2_stocks$Month, "month60", "60")


DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p1, s1)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1 ==1)

GLP1_Patients2_stocks <- GLP1_Patients2_stocks %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "Month"="p2"))

GLP1_Patients2_stocks <- GLP1_Patients2_stocks %>% left_join(DIA_Flows_Aux._Long, by = c("patient"="patient", "Month"="p1"))

GLP1_Patients2_stocks <- GLP1_Patients2_stocks %>% mutate(s2 = ifelse(is.na(s2), s1, s2))
GLP1_Patients2_stocks <- GLP1_Patients2_stocks%>%select(-c(s1))
names(GLP1_Patients2_stocks)[6] <- "stock"



data.frame(GLP1_Patients2_stocks %>% ungroup %>% select(-c(molecule, drug_group)) %>% group_by(patient, weight, Month, stock) %>% 
             distinct() %>% ungroup %>% group_by(patient) %>% 
             filter(row_number() >= (n() - 12)) %>%
             ungroup() %>% group_by(patient) %>%
             mutate(num = row_number()) %>% ungroup() %>% group_by(num, stock) %>%
             summarise(total = sum(as.numeric(weight))))


DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, p2, flow)



Stocks_evol_to_GLP1_13m_PATIENT_LABELS <- GLP1_Patients2_stocks %>% ungroup %>% 
  select(-c(molecule, drug_group)) %>% 
  group_by(patient, weight, Month, stock) %>% 
  distinct() %>% ungroup %>% group_by(patient) %>% 
  filter(row_number() >= (n() - 12)) %>%
  ungroup() %>% 
  left_join(DIA_Flows_Aux._Long, by=c("patient"="patient", "Month"="p2")) %>%
  mutate(flow = ifelse(is.na(flow), "0", flow)) %>%
  group_by(patient) %>%
  mutate(sumFLows = sum(as.numeric(flow))) %>%
  select(patient, weight, stock, sumFLows) %>% 
  group_by(patient, weight) %>% 
  mutate(paths_stocks = paste(stock, collapse="->")) %>% 
  ungroup() %>% select(-c(stock)) %>% distinct() %>%
  mutate(sumFLows = ifelse(sumFLows-1 == 0, "None", "Flow"))

write.csv(Stocks_evol_to_GLP1_13m_PATIENT_LABELS, "Stocks_evol_to_GLP1_13m_PATIENT_LABELS.csv")


Stocks_evol_to_GLP1_13m_each_box <- data.frame(GLP1_Patients2_stocks %>% ungroup %>% select(-c(molecule, drug_group)) %>% 
                                                 group_by(patient, weight, Month, stock) %>% 
                                                 distinct() %>% ungroup %>% group_by(patient) %>% 
                                                 filter(row_number() >= (n() - 12)) %>%
                                                 ungroup() %>% 
                                                 left_join(DIA_Flows_Aux._Long, by=c("patient"="patient", "Month"="p2")) %>%
                                                 mutate(flow = ifelse(is.na(flow), "0", flow)) %>%
                                                 group_by(patient) %>%
                                                 mutate(sumFLows = sum(as.numeric(flow))) %>%
                                                 select(patient, weight, stock, sumFLows) %>% 
                                                 group_by(patient, weight) %>% 
                                                 mutate(paths_stocks = paste(stock, collapse="->")) %>% 
                                                 ungroup() %>% select(-c(stock)) %>% distinct() %>%
                                                 mutate(sumFLows = ifelse(sumFLows-1 == 0, "None", "Flow")) %>%
                                                 group_by(paths_stocks, sumFLows) %>% 
                                                 summarise(total = sum(as.numeric(weight))) %>% arrange(-total))

write.csv(Stocks_evol_to_GLP1_13m_each_box, "Stocks_evol_to_GLP1_13m_each_box_FLOWS.csv")


Stocks_evol_to_GLP1_13m <- Stocks_evol_to_GLP1_13m_each_box %>% mutate(ID = row_number())

Stocks_evol_to_GLP1_13m <- separate_rows(Stocks_evol_to_GLP1_13m, paths_stocks, sep = "->", convert=T )

Stocks_evol_to_GLP1_13m <- data.frame(Stocks_evol_to_GLP1_13m %>% group_by(ID) %>% filter(row_number()==1 | lag(paths_stocks) != paths_stocks))

Stocks_evol_to_GLP1_13m <- Stocks_evol_to_GLP1_13m %>% group_by(ID) %>% mutate(paths_stocks = paste(paths_stocks, collapse="->")) %>% 
  ungroup() %>% group_by(ID, total, paths_stocks) %>% distinct()

data.frame(Stocks_evol_to_GLP1_13m)

write.csv(Stocks_evol_to_GLP1_13m, "Stocks_evol_to_GLP1_13m_unique_shifts_FLOW_INDICATOR.csv")





# ----
# From the stocks on m60, how many pats have lapsed within the previous 12 months? ----------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% filter(p1>=48)

Patient_Stocks_m60 <- DIA_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, s2)

Patients_lapsed_last_year <- DIA_Flows_Aux._Long %>% select(patient, s1, s2) %>% filter(s1 != "x") %>% 
  filter(s2 == "x") %>% select(patient) %>% distinct

Patients_lapsed_last_year$Lapsed <- "Lapsed"

Patient_Stocks_m60 %>% left_join(Patients_lapsed_last_year) %>% group_by(s2, Lapsed) %>%
  summarise(total = sum(as.numeric(weight))) %>% mutate(TOTAL = sum(total))



# ----
# From the stocks on m60, how many pats have switched within the previous 12 months? ----------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% filter(p1>=48)

Patient_Stocks_m60 <- DIA_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, s2)


Patients_swithced_last_year <- DIA_Flows_Aux._Long %>% filter(flow == "1")

Patients_switched <- Patients_swithced_last_year %>% select(patient) %>% distinct()
Patients_switched$switched <- "switched"

Patient_Stocks_m60 %>% left_join(Patients_switched) %>% group_by(s2, switched) %>%
  summarise(total = sum(as.numeric(weight))) %>% mutate(TOTAL = sum(total))




Patients_switched_INTRA <- Patients_swithced_last_year %>% filter(s1 == s2)  %>% select(patient) %>% distinct()
Patients_switched_INTRA$switched_INTRA <- "switched_INTRA"

Patient_Stocks_m60 %>% left_join(Patients_switched_INTRA) %>% group_by(s2, switched_INTRA) %>%
  summarise(total = sum(as.numeric(weight))) %>% mutate(TOTAL = sum(total))



Patients_switched_EXTRA <- Patients_swithced_last_year %>% filter(s1 != s2)  %>% select(patient) %>% distinct()
Patients_switched_EXTRA$switched_EXTRA <- "switched_EXTRA"

Patient_Stocks_m60 %>% left_join(Patients_switched_EXTRA) %>% group_by(s2, switched_EXTRA) %>%
  summarise(total = sum(as.numeric(weight))) %>% mutate(TOTAL = sum(total))


# ----
# Restarts profilings ? ----------------------------------
library(tidyverse)
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)


Pats_restarts <- DIA_Flows_Aux._Long %>%filter(re_starts == "1") %>% select(patient) %>% distinct()
Pats_restarts <- Pats_restarts %>% left_join(DIA_Flows_Aux._Long)
Pats_restarts <- Pats_restarts %>% filter(flow=="1")
Pats_restarts <- Pats_restarts %>% mutate(INDEX = row_number())

Pats_restarts_index <- Pats_restarts %>% filter(re_starts == "1") %>% select(patient, INDEX)
Pats_restarts_index_lag <- Pats_restarts_index 
Pats_restarts_index_lag$INDEX <- (Pats_restarts_index_lag$INDEX)-1
Pats_restarts_index <- Pats_restarts_index %>% left_join(Pats_restarts)
Pats_restarts_index_lag <- Pats_restarts_index_lag %>% left_join(Pats_restarts)
TO_KEEP <- Pats_restarts_index_lag %>%bind_rows(Pats_restarts_index)
TO_KEEP <- TO_KEEP %>% arrange(INDEX)

TO_KEEP %>% group_by(re_starts) %>% summarise(n=n())

TO_KEEP$p1 <- as.numeric(TO_KEEP$p1)
TO_KEEP$p2 <- as.numeric(TO_KEEP$p2)

TO_KEEP %>% filter(re_starts == "1")  %>% filter(p1>=48) %>% summarise(total=sum(as.numeric(weight))) #743272.4


TO_KEEP %>% filter(re_starts == "1")  %>% filter(p1>=48) %>% select(patient) %>% distinct()

#restarts to the same TOP CLASS as before
TO_KEEP  %>%  group_by(patient) %>% filter(s2 == lag(s1)) %>% filter(s2 != "x") %>% filter(p1>=48) %>% ungroup() %>%
  summarise(total = sum (as.numeric(weight))) #528121

#restarts to the a differet TOP CLASS as before
TO_KEEP %>% group_by(patient) %>% filter(s2 != lag(s1)) %>% filter(s2 != "x") %>% filter(p1>=48) %>% ungroup() %>%
  summarise(total = sum (as.numeric(weight))) #215152

#restarts to the same TOP CLASS as before, same drugs 
TO_KEEP %>% filter(s2 == lag(s1)) %>% filter(d2 == lag(d1)) %>% filter(d2 != "-") %>% filter(p1>=48) %>%
  summarise(total = sum (as.numeric(weight))) #438202.2

#restarts to the same TOP CLASS as before, different drugs 
TO_KEEP %>% filter(s2 == lag(s1)) %>% filter(d2 != lag(d1)) %>% filter(d2 != "-") %>% filter(p1>=48) %>%
  summarise(total = sum (as.numeric(weight))) #89918.29




# ----
# HbA1c reads distribution, entire cohort -------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)


DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient)

# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"


HbA1cHist <- DIA_Japan_Drug_Histories_LONG %>% left_join(HbA1cHist)


HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )

HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist$weight <- as.numeric(HbA1cHist$weight)


length(unique(HbA1cHist$patient)) #79936
library(spatstat)
weighted.median(HbA1cHist$HbA1c, HbA1cHist$weight) #5.65
median(HbA1cHist$HbA1c) #6.8
mean(HbA1cHist$HbA1c)


HbA1cHist %>% 
  ggplot(aes(HbA1c, fill="HbA1c"))+
  geom_density(adjust=2, show.legend = F)+
  xlim(3,12)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values="deepskyblue4")+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")


HbA1cHist %>% group_by(patient) %>% slice_tail() %>% ungroup() %>% summarise(n = mean(HbA1c)) # 6.9

HbA1cHist %>% group_by(patient) %>% slice_tail() %>%
  ggplot(aes(HbA1c, fill="HbA1c"))+
  geom_density(adjust=2, show.legend = F)+
  xlim(3,12)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values="deepskyblue4")+
  xlab("\nHbA1c level")+ ylab("Proportion of patients \n")






# ----
# % of patients on each stock above HbA1c > 1 FLow vs no flow -------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% filter(p1>=48)

Patient_Stocks_m60 <- DIA_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, s2)
Patient_flows_12m <- DIA_Flows_Aux._Long %>% group_by(patient) %>% summarise(flows = sum(as.numeric(flow)))
Patient_Stocks_m60 <- Patient_Stocks_m60 %>% left_join(Patient_flows_12m)

HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"
HbA1cHist <- HbA1cHist %>% select(-c(weight))
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist <- HbA1cHist %>% group_by(patient) %>% slice_tail()

Patient_Stocks_m60 <- Patient_Stocks_m60 %>% mutate(flows = ifelse(flows == 0, "NONE", "FLOW"))

Patient_Stocks_m60 <- Patient_Stocks_m60 %>% left_join(HbA1cHist)
Patient_Stocks_m60 <- Patient_Stocks_m60 %>% filter(!is.na(HbA1c))

Patient_Stocks_m60 <- Patient_Stocks_m60 %>% mutate(HbA1c = ifelse(HbA1c > 7, "More than 7", "Less than 7"))
Patient_Stocks_m60 <- Patient_Stocks_m60 %>% mutate(HbA1c = ifelse(HbA1c == "More than 7", "More7", "Less7"))

data.frame(Patient_Stocks_m60 %>% group_by(s2, HbA1c, flows) %>% summarise(total = sum(as.numeric(weight))))


## # # # # # # 
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% filter(p1>=48)

# STOCK m60
Patient_Stocks_m60 <- DIA_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, s2)
# FLOWS Last 12m
Patient_flows_12m <- DIA_Flows_Aux._Long %>% group_by(patient, weight) %>% summarise(flows = sum(as.numeric(flow)))
# STOCK vs Flows
PatientStocksFlows <- Patient_Stocks_m60 %>% left_join(Patient_flows_12m)
# HbA1c
HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"
HbA1cHist <- HbA1cHist %>% select(-c(weight))
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)
HbA1cHist <- HbA1cHist %>% arrange(patient) %>% group_by(patient) %>% slice_tail()

PatientStocksFlows <- PatientStocksFlows %>% left_join(HbA1cHist)
PatientStocksFlows <- PatientStocksFlows %>% filter(!is.na(HbA1c))

PatientStocksFlows_original <- PatientStocksFlows
PatientStocksFlows <- PatientStocksFlows_original


PatientStocksFlows <- PatientStocksFlows %>% mutate(flows = ifelse(flows == 0, "NONE", 
                                                                   ifelse(flows ==1, "1", "+2")))

PatientStocksFlows <- PatientStocksFlows %>% mutate(HbA1c = ifelse(HbA1c > 8, ">8",
                                                                   ifelse(HbA1c >7 & HbA1c <=8, "7_to_8",
                                                                          ifelse(HbA1c >6 & HbA1c <=7, "6_to_7", "<6"))))

PatientStocksFlows <- PatientStocksFlows %>% mutate(s2 = ifelse(s2 == "d" | s2 == "b", "Early_Therapies", s2))

data.frame(PatientStocksFlows %>% group_by(s2, flows, HbA1c) %>% summarise(n=sum(as.numeric(weight))) %>% arrange(flows))




###
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% filter(p1>=48)

# STOCK m60
Patient_Stocks_m60 <- DIA_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, s2)
# FLOWS Last 12m
Patient_flows_12m <- DIA_Flows_Aux._Long %>% group_by(patient, weight) %>% summarise(flows = sum(as.numeric(flow)))
# STOCK vs Flows
PatientStocksFlows <- Patient_Stocks_m60 %>% left_join(Patient_flows_12m)

PatientStocksFlows <- PatientStocksFlows %>% mutate(flows = ifelse(flows == 0, "NONE", 
                                                                   ifelse(flows ==1, "1", "+2")))

PatientStocksFlows <- PatientStocksFlows %>% mutate(s2 = ifelse(s2 == "d" | s2 == "b", "Early_Therapies", s2))

data.frame(PatientStocksFlows %>% group_by(s2, flows) %>% summarise(n=sum(as.numeric(weight))) %>% arrange(flows))


# ----
# % of patients on each acceptance group increasing vs decreasing HbA1c -------------------------------------------
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1, p2, d1, d2, s1, s2, flow)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% filter(p1>=48)

Patient_Stocks_m60 <- DIA_Flows_Aux._Long %>% filter(p2 == "60") %>% select(patient, weight, s2)
rm(DIA_Flows_Aux._Long)

HbA1cHist <- read.table("HbA1cHist.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
names(HbA1cHist)[1] <- "patient"
HbA1cHist <- HbA1cHist %>% select(-c(weight))
HbA1cHist <- gather(HbA1cHist, Month, HbA1c, X1:X60, factor_key=TRUE)
HbA1cHist <- HbA1cHist %>% filter(HbA1c != "")
HbA1cHist <- separate_rows(HbA1cHist, HbA1c, sep = ",", convert=T )
HbA1cHist$HbA1c <- as.numeric(HbA1cHist$HbA1c)

Patient_Stocks_m60 <- Patient_Stocks_m60 %>% left_join(HbA1cHist) %>% filter(!is.na(HbA1c))

Patient_Stocks_m60_Acceptance <- Patient_Stocks_m60 %>% group_by(patient) %>% slice_tail() %>% 
  select(patient, HbA1c) %>% mutate(HbA1c_Acceptance = ifelse(HbA1c > 7, "NOT_Acceptable", "Acceptable")) %>% 
  select(patient, HbA1c_Acceptance)

Patient_Stocks_m60 <- Patient_Stocks_m60 %>% left_join(Patient_Stocks_m60_Acceptance)

Patient_Stocks_m60_TREND <- Patient_Stocks_m60 %>% group_by(patient) %>% mutate(MeanHbA1c = mean(HbA1c)) %>% 
  mutate(Trend = ifelse(HbA1c>MeanHbA1c, "Increasing", "Decreasing")) %>% slice_tail()

data.frame(Patient_Stocks_m60_TREND)

Patient_Stocks_m60_TREND %>% group_by(HbA1c_Acceptance, Trend) %>% summarise(total = sum(as.numeric(weight)))

# ----
# Total Flow vs CAGR ----------------------------------------------
library(tidyverse)
options(scipen = 999)

DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long_v2.1.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(patient, weight, p1,p2, s1, s2, flow)



DIA_Flows_Aux._Long_LastYear <- data.frame(DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% 
                                             filter(p1>=48) %>% filter(flow == "1") %>% group_by(s1, s2) %>% 
                                             summarise(total = sum(as.numeric(weight))))

names(DIA_Flows_Aux._Long_LastYear)[3] <- "TotalLastYear"


DIA_Flows_Aux._Long_FirstYear <- data.frame(DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% 
                                              filter(p1<=12) %>% filter(flow == "1") %>% group_by(s1, s2) %>% 
                                              summarise(total = sum(as.numeric(weight))))

names(DIA_Flows_Aux._Long_FirstYear)[3] <- "TotalFirstYear"

DIA_Flows_Aux._Long_GROWTH <- DIA_Flows_Aux._Long_LastYear %>% left_join(DIA_Flows_Aux._Long_FirstYear, by = c("s1"="s1", "s2"="s2"))

DIA_Flows_Aux._Long_GROWTH <- DIA_Flows_Aux._Long_GROWTH %>% mutate(CAGR = ((TotalLastYear/TotalFirstYear)^(1/5))-1)


DIA_Flows_Aux._Long_GROWTH %>% filter(!is.na(CAGR)) %>% mutate(CAGR = CAGR*100) %>% mutate(comb=paste(s1,"->",s2)) %>%
  ggplot(aes(x=log10(TotalFirstYear), y=log10(TotalLastYear), size=CAGR, colour=comb, fill=comb, type=comb))+
  geom_point(alpha=0.6)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("\nTotal Flow Year 1 (log10)")+ ylab("Total Flow Year 15 (log10) \n")

DIA_Flows_Aux._Long_GROWTH2 <- DIA_Flows_Aux._Long_GROWTH %>% filter(!is.na(CAGR)) %>% mutate(CAGR = CAGR*100) %>% mutate(comb=paste0(s1,"->",s2))
write.csv(DIA_Flows_Aux._Long_GROWTH2, "DIA_Flows_Aux._Long_GROWTH.csv")
DIA_Flows_Aux._Long_GROWTH <- read.csv("DIA_Flows_Aux._Long_GROWTH.csv")

DIA_Flows_Aux._Long_GROWTH %>%
  ggplot(aes(x=reorder(comb, CAGR), y=CAGR, size=TotalLastYear))+
  geom_point(alpha=0.6, colour="midnightblue", fill="midnightblue", show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  xlab("\nClass Shift")+ ylab("Annual Growth Rate\n")








# ----
# Insulins periods duration by visibility -------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()


# count (how many months) in each of this lapsed periods!
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"

Insulin_Periods_DIA_VIZ <- Insulin_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)


write.csv(Insulin_Periods_DIA_VIZ, "Insulin_Periods_DIA_VIZ.csv")



# ----
# GLPs periods duration by visibility ----------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs to zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('38',.), ~replace(., grepl('38', .), "GLP"))%>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "GLP"))%>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "GLP"))%>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "GLP"))%>%
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "GLP"))%>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "GLP"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

# count (how many months) in each of this lapsed periods!
GPLP1_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(GPLP1_Periods_DIA)[3] <- "Duration"

GPLP1_Periods_DIA_VIZ <- GPLP1_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(GPLP1_Periods_DIA_VIZ, "GPLP1_Periods_DIA_VIZ.csv")



# ----
# SGLT2s periods duration by visibility ---------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2s too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "SGLT2"))%>% 
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "SGLT2"))%>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "SGLT2"))%>%
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "SGLT2"))%>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "SGLT2"))%>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "SGLT2"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SGLT2",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

# count (how many months) in each of this lapsed periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA_VIZ <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(SGLT2_Periods_DIA_VIZ, "SGLT2_Periods_DIA_VIZ.csv")



# ----
# DPP4s periods duration by visibility ------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "DPP4"))%>% 
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "DPP4"))%>% 
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "DPP4"))%>%
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "DPP4"))%>%
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "DPP4"))%>%
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "DPP4"))%>%
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "DPP4"))%>%
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "DPP4"))%>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "DPP4"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="DPP4",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)


DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()


# count (how many months) in each of this lapsed periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"


DPP4_Periods_DIA_VIZ <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                         select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(DPP4_Periods_DIA_VIZ, "DPP4_Periods_DIA_VIZ.csv")



# ----
# Biguanides periods duration by visibility --------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no biguanides too zero, and biguanides to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "Biguanide"))%>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "Biguanide"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Biguanide",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)


DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()


# count (how many months) in each of this lapsed periods!
Biguanide_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Biguanide_Periods_DIA)[3] <- "Duration"



Biguanide_Periods_DIA_VIZ <- Biguanide_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Biguanide_Periods_DIA_VIZ, "Biguanide_Periods_DIA_VIZ.csv")



# ----
# Antidiabetic periods duration by visibility --------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no biguanides too zero, and biguanides to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(17{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "Antidiabetic"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antidiabetic",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)


DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()


# count (how many months) in each of this lapsed periods!
Antidiabetic_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Antidiabetic_Periods_DIA)[3] <- "Duration"



Antidiabetic_Periods_DIA_VIZ <- Antidiabetic_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                                         select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Antidiabetic_Periods_DIA_VIZ, "Antidiabetic_Periods_DIA_VIZ.csv")














# ----
# Visibility/durations/persistency, only the very first exposure to drug -----------------------------

# ----
# Biguanides periods duration by visibility --------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no biguanides too zero, and biguanides to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(2{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(2{1})(\\D|$)', .), "Biguanide"))%>% 
  mutate_if(grepl('(^|\\D)(1{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(1{1})(\\D|$)', .), "Biguanide"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Biguanide",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)


DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()


# count (how many months) in each of this lapsed periods!
Biguanide_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Biguanide_Periods_DIA)[3] <- "Duration"

Biguanide_Periods_DIA <- Biguanide_Periods_DIA %>% filter(grp==min(grp))


Biguanide_Periods_DIA_VIZ <- Biguanide_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                                   select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Biguanide_Periods_DIA_VIZ, "Biguanide_Periods_DIA_VIZ_FIRST_ONLY.csv")













# ----
# Antidiabetic periods duration by visibility --------------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no biguanides too zero, and biguanides to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('(^|\\D)(8{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(8{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(9{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(9{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(10{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(10{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(11{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(11{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(12{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(12{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(13{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(13{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(14{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(14{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(15{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(15{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(16{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(16{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(17{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(17{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(18{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(18{1})(\\D|$)', .), "Antidiabetic"))%>% 
  mutate_if(grepl('(^|\\D)(19{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(19{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(20{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(20{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(21{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(21{1})(\\D|$)', .), "Antidiabetic"))%>%
  mutate_if(grepl('(^|\\D)(22{1})(\\D|$)',.), ~replace(., grepl('(^|\\D)(22{1})(\\D|$)', .), "Antidiabetic"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Antidiabetic",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)


DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()


# count (how many months) in each of this lapsed periods!
Antidiabetic_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Antidiabetic_Periods_DIA)[3] <- "Duration"

Antidiabetic_Periods_DIA <- Antidiabetic_Periods_DIA %>% filter(grp==min(grp))



Antidiabetic_Periods_DIA_VIZ <- Antidiabetic_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                                         select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(Antidiabetic_Periods_DIA_VIZ, "Antidiabetic_Periods_DIA_VIZ_FIRST_ONLY.csv")






# ----
# DPP4s periods duration by visibility ------------------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "DPP4"))%>% 
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "DPP4"))%>% 
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "DPP4"))%>%
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "DPP4"))%>%
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "DPP4"))%>%
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "DPP4"))%>%
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "DPP4"))%>%
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "DPP4"))%>%
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "DPP4"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="DPP4",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)


DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()


# count (how many months) in each of this lapsed periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% filter(grp==min(grp))


DPP4_Periods_DIA_VIZ <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                         select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(DPP4_Periods_DIA_VIZ, "DPP4_Periods_DIA_VIZ_FIRST_ONLY.csv")





# ----
# SGLT2s periods duration by visibility ---------------------------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2s too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "SGLT2"))%>% 
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "SGLT2"))%>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "SGLT2"))%>%
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "SGLT2"))%>%
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "SGLT2"))%>%
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "SGLT2"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="SGLT2",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

# count (how many months) in each of this lapsed periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% filter(grp==min(grp))


SGLT2_Periods_DIA_VIZ <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(SGLT2_Periods_DIA_VIZ, "SGLT2_Periods_DIA_VIZ_FIRST_ONLY.csv")







# ----
# GLPs periods duration by visibility ----------------------------------------
# Import files again
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs to zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('38',.), ~replace(., grepl('38', .), "GLP"))%>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "GLP"))%>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "GLP"))%>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "GLP"))%>%
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "GLP"))%>%
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "GLP"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="GLP",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()

# count (how many months) in each of this lapsed periods!
GPLP1_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(GPLP1_Periods_DIA)[3] <- "Duration"

GPLP1_Periods_DIA <- GPLP1_Periods_DIA %>% filter(grp==min(grp))


GPLP1_Periods_DIA_VIZ <- GPLP1_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                           select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)

write.csv(GPLP1_Periods_DIA_VIZ, "GPLP1_Periods_DIA_VIZ_FIRST_ONLY.csv")







# ----
# Insulins periods duration by visibility -------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", quote="", 
                                       colClasses = "character", stringsAsFactors = FALSE)

# select only columns with the months / drugs
DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLPs too zero, and GLPs to one
# convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('44',.), ~replace(., grepl('44', .), "Insulin"))%>% 
  mutate_if(grepl('45',.), ~replace(., grepl('45', .), "Insulin"))%>% 
  mutate_if(grepl('46',.), ~replace(., grepl('46', .), "Insulin"))%>% 
  mutate_if(grepl('47',.), ~replace(., grepl('47', .), "Insulin"))%>%
  mutate_if(grepl('48',.), ~replace(., grepl('48', .), "Insulin"))%>%
  mutate_if(grepl('49',.), ~replace(., grepl('49', .), "Insulin"))%>%
  mutate_if(grepl('50',.), ~replace(., grepl('50', .), "Insulin"))%>%
  mutate_if(grepl('51',.), ~replace(., grepl('51', .), "Insulin"))%>%
  mutate_if(grepl('52',.), ~replace(., grepl('52', .), "Insulin"))%>%
  mutate_if(grepl('53',.), ~replace(., grepl('53', .), "Insulin"))%>%
  mutate_if(grepl('54',.), ~replace(., grepl('54', .), "Insulin"))%>%
  mutate_if(grepl('55',.), ~replace(., grepl('55', .), "Insulin"))%>%
  mutate_if(grepl('56',.), ~replace(., grepl('56', .), "Insulin"))%>%
  mutate_if(grepl('57',.), ~replace(., grepl('57', .), "Insulin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Insulin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)


# for each patient, count how long it remains on the same line 
# of course, only 2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  filter(Treat == 1)

DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month1", "1")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month2", "2")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month3", "3")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month4", "4")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month5", "5")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month6", "6")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month7", "7")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month8", "8")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month9", "9")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month10", "10")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month11", "11")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month12", "12")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month13", "13")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month14", "14")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month15", "15")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month16", "16")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month17", "17")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month18", "18")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month19", "19")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month20", "20")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month21", "21")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month22", "22")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month23", "23")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month24", "24")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month25", "25")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month26", "26")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month27", "27")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month28", "28")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month29", "29")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month30", "30")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month31", "31")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month32", "32")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month33", "33")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month34", "34")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month35", "35")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month36", "36")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month37", "37")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month38", "38")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month39", "39")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month40", "40")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month41", "41")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month42", "42")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month43", "43")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month44", "44")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month45", "45")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month46", "46")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month47", "47")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month48", "48")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month49", "49")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month50", "50")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month51", "51")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month52", "52")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month53", "53")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month54", "54")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month55", "55")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month56", "56")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month57", "57")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month58", "58")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month59", "59")
DIA_Japan_Drug_Histories$Month <- str_replace(DIA_Japan_Drug_Histories$Month, "month60", "60")

DIA_Japan_Drug_Histories$Month <- as.numeric(DIA_Japan_Drug_Histories$Month)

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% 
  mutate(start = min(Month)) %>% mutate(visibility = 60-start) %>% ungroup()


# count (how many months) in each of this lapsed periods!
Insulin_Periods_DIA <- DIA_Japan_Drug_Histories %>%
  group_by(patient, grp) %>%
  summarise(n=n())

names(Insulin_Periods_DIA)[3] <- "Duration"

Insulin_Periods_DIA <- Insulin_Periods_DIA %>% filter(grp==min(grp))


Insulin_Periods_DIA_VIZ <- Insulin_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories %>% 
                                                               select(patient, weight, visibility), by=c("patient"="patient")) %>% 
  distinct() %>% mutate(weight = as.numeric(weight)) %>% mutate(Total_duration =sum(Duration)) %>% ungroup() %>%
  select(patient, weight, visibility, Total_duration) %>% distinct() %>% mutate(visibility = visibility+1) %>% 
  mutate(pats = sum(weight)) %>%
  group_by(visibility, Total_duration) %>% summarise(total= sum(weight)) %>%
  spread(key = visibility, value = total)


write.csv(Insulin_Periods_DIA_VIZ, "Insulin_Periods_DIA_VIZ_FIRST_ONLY.csv")





# ----
# check penetrance for each individual molecule over time ---------------------------
# repeat for each month, append to table
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)

DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% separate(drug_id, c('class', 'molecule'))

DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

length(unique(DIA_Japan_Drug_Histories$patient)) # 96910
sum(as.numeric(DIA_Japan_Drug_Histories$weight)) #7967485

DIA_Japan_Drug_Histories_month60 <- DIA_Japan_Drug_Histories %>% select(patient, weight, month60)
DIA_Japan_Drug_Histories_month60 <- separate_rows(DIA_Japan_Drug_Histories_month60, month60, sep = ",", convert=T )
names(DIA_Japan_Drug_Histories_month60)[3] <- "molecule"

DIA_Japan_Drug_Histories_month60 <- 
  DIA_Japan_Drug_Histories_month60 %>% 
  left_join(DANU_Japan_Ingredients %>% 
              select(molecule, generic_name)) %>% 
  filter(!is.na(generic_name))

DIA_Japan_Drug_Histories_month60 <- DIA_Japan_Drug_Histories_month60 %>% select(patient, weight, generic_name)
DIA_Japan_Drug_Histories_month60 <- DIA_Japan_Drug_Histories_month60 %>% distinct()
DIA_Japan_Drug_Histories_time_series_month60 <- data.frame(DIA_Japan_Drug_Histories_month60 %>% group_by(generic_name) %>% summarise(sum_weights_month60 = sum(as.numeric(weight))))
DIA_Japan_Drug_Histories_time_series <- DIA_Japan_Drug_Histories_time_series %>% full_join(DIA_Japan_Drug_Histories_time_series_month60)


DIA_Japan_Drug_Histories_time_series[is.na(DIA_Japan_Drug_Histories_time_series)] <- 0 

write.csv(DIA_Japan_Drug_Histories_time_series, "DIA_Japan_Drug_Histories_time_series.csv")

DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% select(generic_name, drug_group)


DIA_Japan_Drug_Histories_time_series <- DIA_Japan_Drug_Histories_time_series %>% left_join(DANU_Japan_Ingredients)

DIA_Japan_Drug_Histories_time_series <- DIA_Japan_Drug_Histories_time_series %>% arrange(drug_group, sum_weights_month60)

write.csv(DIA_Japan_Drug_Histories_time_series, "DIA_Japan_Drug_Histories_time_series.csv")



# ----
# Persistency curves per individual molecule ------------------------------------------------------
# ----
#DPP4 --------------------------------------------------------------------------------------------
# ----
#Alogliptin ----------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('23',.), ~replace(., grepl('23', .), "Alogliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Alogliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  29.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# ----
#Anagliptin ----------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('24',.), ~replace(., grepl('24', .), "Anagliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Anagliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  19.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))







# ----
#Linagliptin ----------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('25',.), ~replace(., grepl('25', .), "Linagliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Linagliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  20.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


# ----
#Omarigliptin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('26',.), ~replace(., grepl('26', .), "Omarigliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Omarigliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  16.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


# ----
#Saxagliptin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('27',.), ~replace(., grepl('27', .), "Saxagliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Saxagliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  22.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


# ----
#Sitagliptin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('28',.), ~replace(., grepl('28', .), "Sitagliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Sitagliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  31.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# ----
#Teneligliptin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('29',.), ~replace(., grepl('29', .), "Teneligliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Teneligliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  23.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


# ----
#Trelagliptin ----------------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('30',.), ~replace(., grepl('30', .), "Trelagliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Trelagliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  23.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# ----
#Vildagliptin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no DPP4 too zero, and DPP4 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('31',.), ~replace(., grepl('31', .), "Vildagliptin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Vildagliptin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these DPP4 periods!
DPP4_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(DPP4_Periods_DIA)[3] <- "Duration"

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% mutate(weight = as.numeric(weight))

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

DPP4_Periods_DIA <- DPP4_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(DPP4_Periods_DIA$Total_Duration, DPP4_Periods_DIA$weight) #  33.5

data.frame(DPP4_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))







# ----
# SGLT2 ------------------------------------------------------------------------------------------- 

# ----
#Canagliflozin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2 too zero, and SGLT2 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('32',.), ~replace(., grepl('32', .), "Canagliflozin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Canagliflozin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% mutate(weight = as.numeric(weight))

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(SGLT2_Periods_DIA$Total_Duration, SGLT2_Periods_DIA$weight) #  20.5

data.frame(SGLT2_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))
# --------------
#Dapagliflozin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2 too zero, and SGLT2 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('33',.), ~replace(., grepl('33', .), "Dapagliflozin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Dapagliflozin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% mutate(weight = as.numeric(weight))

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(SGLT2_Periods_DIA$Total_Duration, SGLT2_Periods_DIA$weight) #  17.5

data.frame(SGLT2_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


# ----
#Empagliflozin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2 too zero, and SGLT2 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('34',.), ~replace(., grepl('34', .), "Empagliflozin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Empagliflozin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% mutate(weight = as.numeric(weight))

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(SGLT2_Periods_DIA$Total_Duration, SGLT2_Periods_DIA$weight) #  17.5

data.frame(SGLT2_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))

# ----
#Ipragliflozin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2 too zero, and SGLT2 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('35',.), ~replace(., grepl('35', .), "Ipragliflozin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Ipragliflozin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% mutate(weight = as.numeric(weight))

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(SGLT2_Periods_DIA$Total_Duration, SGLT2_Periods_DIA$weight) #  18.5

data.frame(SGLT2_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))

# ----
#Luseogliflozin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2 too zero, and SGLT2 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('36',.), ~replace(., grepl('36', .), "Luseogliflozin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Luseogliflozin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% mutate(weight = as.numeric(weight))

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(SGLT2_Periods_DIA$Total_Duration, SGLT2_Periods_DIA$weight) #  18.5

data.frame(SGLT2_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# ----
#Tofogliflozin ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no SGLT2 too zero, and SGLT2 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('37',.), ~replace(., grepl('37', .), "Tofogliflozin"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Tofogliflozin",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
SGLT2_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(SGLT2_Periods_DIA)[3] <- "Duration"

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% mutate(weight = as.numeric(weight))

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

SGLT2_Periods_DIA <- SGLT2_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(SGLT2_Periods_DIA$Total_Duration, SGLT2_Periods_DIA$weight) #  21.5

data.frame(SGLT2_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))





# ----
# GLP1 -------------------------------------------------------------------------------------


# ----
#Dulaglutide ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLP1 too zero, and GLP1 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('39',.), ~replace(., grepl('39', .), "Dulaglutide"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Dulaglutide",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
GLP1_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(GLP1_Periods_DIA)[3] <- "Duration"

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% mutate(weight = as.numeric(weight))

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(GLP1_Periods_DIA$Total_Duration, GLP1_Periods_DIA$weight) #  14.5

data.frame(GLP1_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# ----
#Exenatide ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLP1 too zero, and GLP1 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('40',.), ~replace(., grepl('40', .), "Exenatide"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Exenatide",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
GLP1_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(GLP1_Periods_DIA)[3] <- "Duration"

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% mutate(weight = as.numeric(weight))

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(GLP1_Periods_DIA$Total_Duration, GLP1_Periods_DIA$weight) #  7.5

data.frame(GLP1_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))




# ----
#Liraglutide ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLP1 too zero, and GLP1 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('41',.), ~replace(., grepl('41', .), "Liraglutide"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Liraglutide",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
GLP1_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(GLP1_Periods_DIA)[3] <- "Duration"

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% mutate(weight = as.numeric(weight))

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(GLP1_Periods_DIA$Total_Duration, GLP1_Periods_DIA$weight) #  12.5

data.frame(GLP1_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))


# ----
#Lixisenatide ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLP1 too zero, and GLP1 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('42',.), ~replace(., grepl('42', .), "Lixisenatide"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Lixisenatide",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
GLP1_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(GLP1_Periods_DIA)[3] <- "Duration"

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% mutate(weight = as.numeric(weight))

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(GLP1_Periods_DIA$Total_Duration, GLP1_Periods_DIA$weight) #  6.5

data.frame(GLP1_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))




# ----
#Semaglutide Injectable ----------------------------------------------------------------------------------
DIA_Japan_Drug_Histories <- read.table("DIA Japan Drug Histories_v2.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>%  select(4:63)

# convert no GLP1 too zero, and GLP1 to one # convert to numeric everything
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% 
  mutate_if(grepl('43',.), ~replace(., grepl('43', .), "Semaglutide Injectable"))

DIA_Japan_Drug_Histories <-  DIA_Japan_Drug_Histories %>% mutate_all(function(x) ifelse(x=="Semaglutide Injectable",1,0))

DIA_Japan_Drug_Histories[] <-  lapply(DIA_Japan_Drug_Histories,as.numeric)

DIA_Japan_Drug_Histories_LONG <- read.table("DIA Japan Drug Histories_v2.txt", 
                                            header = T, sep="\t", quote="", 
                                            colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories_LONG <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories_LONG %>% bind_cols(DIA_Japan_Drug_Histories)
rm(DIA_Japan_Drug_Histories_LONG)

DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% arrange(patient)

# for each patient, count how long it remains on the same line,2 lines possible, treatment or no treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>%
  group_by(patient) %>%
  mutate(grp = rle(Treat)$lengths %>% {rep(seq(length(.)), .)})

# filter for the periods of treatment
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(Treat == 1)

# count (how many months) in each of these SGLT2 periods!
GLP1_Periods_DIA <- DIA_Japan_Drug_Histories %>% group_by(patient, grp) %>% summarise(n=n())

names(GLP1_Periods_DIA)[3] <- "Duration"

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% select(patient, Duration) 

DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% left_join(DIA_Japan_Drug_Histories) 

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% mutate(weight = as.numeric(weight))

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% group_by(patient) %>% mutate(Total_Duration = sum(Duration)) %>% select(patient, weight, Total_Duration)

GLP1_Periods_DIA <- GLP1_Periods_DIA %>% distinct()

library(spatstat)
weighted.median(GLP1_Periods_DIA$Total_Duration, GLP1_Periods_DIA$weight) #  4.5

data.frame(GLP1_Periods_DIA %>% distinct() %>% group_by(Total_Duration) %>% summarise(pats = sum(weight)))



# ----
# Semaglutide dosages over time ----------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
DIA_Japan_Medications <- read.table("DANU Japan Medications.txt", 
                                    header = T, sep="\t", quote="", 
                                    colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Medications <- DIA_Japan_Medications %>% filter(drug_group =="GLP1 Injectable") %>% select(drug_id, med_description, med_ingredient, med_strength)


Dia_Japan_Doses <- read.table("Dia Japan Doses_v2.txt", 
                              header = T, sep=",", quote="", 
                              colClasses = "character", stringsAsFactors = FALSE)

Dia_Japan_Doses <- Dia_Japan_Doses %>% filter(drug_group == "GLP1 Injectable") %>% left_join(DIA_Japan_Medications)  %>% mutate(from_dt = as.Date(from_dt))

Dia_Japan_Doses$doses <- parse_number(Dia_Japan_Doses$med_strength)



Dia_Japan_Doses %>% filter(generic_name =="Semaglutide Injectable") %>% 
  select(generic_name, dayssup, pat_id, from_dt, doses) %>% group_by(pat_id) %>% 
  summarise(n=n()) %>% arrange(-n)

Dia_Japan_Doses_semaglutide <- Dia_Japan_Doses %>% filter(generic_name =="Semaglutide Injectable") %>% 
  select(generic_name, dayssup, pat_id, from_dt, doses) 

Dia_Japan_Doses_semaglutide <- Dia_Japan_Doses_semaglutide %>% filter(!is.na(doses))

Dia_Japan_Doses_semaglutide_summary <- Dia_Japan_Doses_semaglutide %>% group_by(pat_id) %>% arrange(pat_id, from_dt) %>% 
  mutate(index = from_dt-lag(from_dt)) %>% ungroup() %>% mutate(index = as.numeric(index)) %>%
  mutate(index = ifelse(is.na(index), 0, index)) %>% group_by(pat_id) %>%
  mutate(time_progression = cumsum(index)) %>% select(-c(generic_name, dayssup, index))


Dia_Japan_Doses_semaglutide_summary %>% ungroup() %>% select(pat_id, doses, time_progression) %>%
  ggplot(aes(x=time_progression, y=doses, fill=doses, colour=-doses))+
  geom_jitter(height =0.1, width = 0.5, show.legend = F, alpha=0.5, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 30))+
  xlab("\nTime since therapy initiation (days)")+
  ylab("Dosage Prescribed\n")+
  scale_y_continuous(breaks = c(0.25, 0.5, 1))
xlim(0,30)

length(unique(Dia_Japan_Doses_semaglutide_summary$pat_id))

Dia_Japan_Doses_semaglutide_summary %>% ungroup() %>% 
  left_join(Dia_Japan_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% slice_head() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))


Dia_Japan_Doses_semaglutide_summary %>% ungroup() %>% 
  left_join(Dia_Japan_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=30) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_Japan_Doses_semaglutide_summary %>% ungroup() %>% 
  left_join(Dia_Japan_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=60) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))


Dia_Japan_Doses_semaglutide_summary %>% ungroup() %>% 
  left_join(Dia_Japan_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=90) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_Japan_Doses_semaglutide_summary %>% ungroup() %>% 
  left_join(Dia_Japan_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=120) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))


Dia_Japan_Doses_semaglutide_summary %>% ungroup() %>% 
  left_join(Dia_Japan_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=150) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))

Dia_Japan_Doses_semaglutide_summary %>% ungroup() %>% 
  left_join(Dia_Japan_Doses %>% select(pat_id, weight) %>% distinct()) %>%
  group_by(pat_id) %>% filter(time_progression <=180) %>% slice_tail() %>% ungroup() %>% group_by(doses) %>% summarise(n=sum(as.numeric(weight)))



# ----

library(tidyverse)
library(data.table)
library(spatstat)


# BMI distribution per stock -------------------------------------------------------------------
# BMI on each stock each time-point
DIA_Japan_Box_histories <- read.table("Dia Japan Box Histories_v2.1.txt", 
                                      header = T, sep=",", quote="", 
                                      colClasses = "character", stringsAsFactors = FALSE)

# File with BMI over time
BMIHistAll <- read.table("BMIHistAll.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(BMIHistAll)[1] <- "patient"
BMIHistAll <- BMIHistAll %>% select(patient, month1:month60)

# filter for pats in the DIABETES table only
BMIHistAll <- DIA_Japan_Box_histories %>% select(patient, weight) %>% inner_join(BMIHistAll)

BMIHistAll <- BMIHistAll %>% gather(Month, BMI, month1:month60, factor_key=TRUE)
BMIHistAll <- BMIHistAll %>% filter(BMI != "")
BMIHistAll <- separate_rows(BMIHistAll, BMI, sep = ",", convert=T )


DIA_Japan_Box_histories <- DIA_Japan_Box_histories %>% gather(Month, stock, month1:month60, factor_key=TRUE)

BMIHistAll <- BMIHistAll %>% left_join((DIA_Japan_Box_histories %>% select(patient, Month, stock)), by=c("patient"="patient", "Month"="Month"))

BMIHistAll %>% group_by(stock) %>% summarise(n=weighted.mean(as.numeric(BMI), as.numeric(weight)))


BMIHistAll %>% group_by(stock) %>% summarise(n=weighted.median(as.numeric(BMI), as.numeric(weight)))



BMIHistAll_each_record <- BMIHistAll
BMIHistAll_each_record %>% group_by(stock) %>% summarise(n=sum(as.numeric(weight)))
length(unique(BMIHistAll_each_record$patient))

BMIHistAll_each_record %>% select(stock, BMI, weight) %>% 
  mutate(stock = factor(stock, levels=c("D", "b", "d", "I", "x", "S", "G", "g"))) %>% group_by(stock) %>%
  ggplot(aes(BMI))+
  geom_density(aes(fill = stock, colour=stock), alpha =0.7, show.legend = F, size=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(~stock, ncol = 8)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  coord_flip()+
  xlim(20,40)+
  xlab("BMI (kg/m2)\n")+ ylab("\nProportion of patients")


BMIHistAll_each_record %>% select(stock, BMI, weight) %>% 
  mutate(stock = factor(stock, levels=c("D", "b", "d", "I", "x", "S", "G", "g"))) %>% group_by(stock) %>%
  ggplot(aes(BMI))+
  geom_boxplot(aes(fill = stock, colour=stock), width=0.1 , alpha =0.4, show.legend = F, size=1, outlier.shape = NA)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(~stock, ncol = 8)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  coord_flip()+
  xlim(20,40)+
  xlab("BMI (kg/m2)\n")+ ylab("\nProportion of patients")


data.frame(BMIHistAll_each_record %>% select(stock, BMI, weight) %>% 
  mutate(stock = factor(stock, levels=c("D", "b", "d", "I", "x", "S", "G", "g"))) %>% group_by(stock) %>% 
  summarise(x = quantile(BMI, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75)))

BMIHistAll_each_record$weight <- as.numeric(BMIHistAll_each_record$weight)

library(plyr)
library(dplyr)
library(Hmisc)

BMIHistAll_each_record %>% select(stock, BMI, weight) %>% 
  filter(stock=="D") %>% 
  plyr::summarize(ptile25 = wtd.quantile(BMI, weights = weight, 
                                   probs = .25, na.rm = TRUE),
            ptile50 = wtd.quantile(BMI, weights = weight, 
                                   probs = .50, na.rm = TRUE),
            ptile75 = wtd.quantile(BMI, weights = weight, 
                                   probs = .75, na.rm = TRUE))


weighted.median(BMIHistAll_each_record$BMI[BMIHistAll_each_record$stock=="G"], BMIHistAll_each_record$weight[BMIHistAll_each_record$stock=="G"])



BMIHistAll_each_record %>% group_by(stock) %>% summarise(n=sum(as.numeric(weight)))


data.frame(BMIHistAll_each_record %>% select(stock, BMI, weight) %>%
  mutate(BMI_bucket=ifelse(BMI>30,">30",
                           ifelse(BMI>25&BMI<=30,"25-30",
                                  ifelse(BMI>20&BMI<=25,"20-25",
                                         ifelse(BMI>18.5&BMI<=20,"18.5-20", "<18.5")))))%>%
  group_by(stock, BMI_bucket) %>% summarise(n=sum(as.numeric(weight))))



# ----
# Bypass gastric surgery -------------------
# penetrance across the entire 60-month period
DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))

OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

sum(as.numeric(OBE_Japan_Drug_Histories$weight)) #18564058      
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(patient, weight, month1:month60)
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% gather(Month, Treat, month1:month60, factor_key=TRUE)
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% filter(Treat != "-")
OBE_Japan_Drug_Histories <- separate_rows(OBE_Japan_Drug_Histories, Treat, sep = ",", convert=T )
names(OBE_Japan_Drug_Histories)[4] <- "molecule"

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% filter(molecule != "-")
OBE_Japan_Drug_Histories$molecule <- as.character(OBE_Japan_Drug_Histories$molecule)
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>%  left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_class)) 
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(patient, weight, drug_class)
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% distinct()

length(unique(OBE_Japan_Drug_Histories$patient)) #8620
#class penetrance ever
OBE_Japan_Drug_Histories %>% group_by(drug_class) %>%
  summarise(sum_weights = sum(as.numeric(weight))) %>%
  filter(!is.na(drug_class))


OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% filter(drug_class == "Surgery")
sum(as.numeric(OBE_Japan_Drug_Histories$weight)) #4227.57

gastric_Bypass_pats <- OBE_Japan_Drug_Histories %>% select(patient, weight)



OBE_Japan_Box_Histories <- read.table("OBE Japan Box Histories.txt", 
                                       header = T, sep="\t", 
                                       colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Box_Histories <- OBE_Japan_Box_Histories %>% select(patient, weight, month60)
OBE_Japan_Box_Histories <- OBE_Japan_Box_Histories %>% mutate(month60 = str_sub(month60, 2L, 2L))

OBE_Japan_Box_Histories %>% group_by(month60) %>% summarise(n=sum(as.numeric(weight)))

# ----
# class penetrance before, after ---------
OBE_Japan_Drug_Histories_long <- read.table("OBE Japan Drug Histories_v2.txt", 
                                            header = T, sep=",", 
                                            colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(patient, weight, month1:month60)
OBE_Japan_Drug_Histories <- gastric_Bypass_pats %>% left_join(OBE_Japan_Drug_Histories)
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% gather(Month, Treat, month1:month60, factor_key=TRUE)
OBE_Japan_Drug_Histories <- separate_rows(OBE_Japan_Drug_Histories, Treat, sep = ",", convert=T )
names(OBE_Japan_Drug_Histories)[4] <- "molecule"
OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% arrange(patient) %>% left_join(DANU_Japan_Ingredients %>% select(molecule, generic_name, drug_group)) 


OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month1", "1")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month2", "2")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month3", "3")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month4", "4")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month5", "5")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month6", "6")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month7", "7")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month8", "8")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month9", "9")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month10", "10")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month11", "11")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month12", "12")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month13", "13")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month14", "14")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month15", "15")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month16", "16")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month17", "17")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month18", "18")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month19", "19")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month20", "20")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month21", "21")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month22", "22")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month23", "23")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month24", "24")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month25", "25")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month26", "26")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month27", "27")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month28", "28")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month29", "29")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month30", "30")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month31", "31")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month32", "32")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month33", "33")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month34", "34")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month35", "35")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month36", "36")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month37", "37")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month38", "38")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month39", "39")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month40", "40")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month41", "41")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month42", "42")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month43", "43")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month44", "44")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month45", "45")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month46", "46")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month47", "47")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month48", "48")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month49", "49")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month50", "50")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month51", "51")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month52", "52")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month53", "53")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month54", "54")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month55", "55")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month56", "56")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month57", "57")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month58", "58")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month59", "59")
OBE_Japan_Drug_Histories$Month <- str_replace(OBE_Japan_Drug_Histories$Month, "month60", "60")

OBE_Japan_Drug_Histories$Month <- as.numeric(OBE_Japan_Drug_Histories$Month)

pats_surgery_19_42 <- OBE_Japan_Drug_Histories %>% filter(molecule=="58") %>%
  filter(Month>=19&Month<=42) %>% select(patient)

OBE_Japan_Drug_Histories <- pats_surgery_19_42 %>% left_join(OBE_Japan_Drug_Histories)

OBE_Japan_Drug_Histories <- data.frame(OBE_Japan_Drug_Histories %>% group_by(patient, weight) %>% 
                                         slice((which.max(drug_group == "Surgery")-18):(which.max(drug_group == "Surgery")+18))%>%
                                         mutate(MonthIndex=row_number()))

data.frame(OBE_Japan_Drug_Histories %>% group_by(MonthIndex, drug_group) %>%
             summarise(pats=sum(as.numeric(weight))))



BMIHistAll <- read.table("BMIHistAll.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(BMIHistAll)[1] <- "patient"
BMIHistAll <- BMIHistAll %>% select(patient, month1:month60)
BMIHistAll <- BMIHistAll %>% gather(Month, BMI, month1:month60, factor_key=TRUE)
BMIHistAll <- BMIHistAll %>% filter(BMI != "")
BMIHistAll <- separate_rows(BMIHistAll, BMI, sep = ",", convert=T )

BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month1", "1")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month2", "2")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month3", "3")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month4", "4")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month5", "5")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month6", "6")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month7", "7")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month8", "8")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month9", "9")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month10", "10")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month11", "11")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month12", "12")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month13", "13")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month14", "14")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month15", "15")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month16", "16")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month17", "17")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month18", "18")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month19", "19")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month20", "20")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month21", "21")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month22", "22")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month23", "23")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month24", "24")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month25", "25")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month26", "26")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month27", "27")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month28", "28")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month29", "29")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month30", "30")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month31", "31")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month32", "32")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month33", "33")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month34", "34")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month35", "35")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month36", "36")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month37", "37")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month38", "38")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month39", "39")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month40", "40")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month41", "41")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month42", "42")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month43", "43")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month44", "44")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month45", "45")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month46", "46")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month47", "47")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month48", "48")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month49", "49")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month50", "50")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month51", "51")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month52", "52")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month53", "53")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month54", "54")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month55", "55")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month56", "56")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month57", "57")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month58", "58")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month59", "59")
BMIHistAll$Month <- str_replace(BMIHistAll$Month, "month60", "60")

BMIHistAll$Month <- as.numeric(BMIHistAll$Month)
BMIHistAll

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% left_join(BMIHistAll, by=c("patient"="patient","Month"="Month"))


pats_BMI_before_after <- OBE_Japan_Drug_Histories %>% group_by(patient)%>% count(is.na(BMI)) %>% filter(`is.na(BMI)` == FALSE & n>=2) %>% select(patient)

OBE_Japan_Drug_Histories <- pats_BMI_before_after %>% left_join(OBE_Japan_Drug_Histories)

length(unique(OBE_Japan_Drug_Histories$patient))

data.frame(OBE_Japan_Drug_Histories %>% group_by(MonthIndex) %>% 
             summarise(n=weighted.mean(BMI, as.numeric(weight), na.rm = T))%>%
             mutate(MonthIndex=-18:18))%>%
  ggplot(aes(x=MonthIndex,y=n))+
  geom_smooth(method="loess",se = F, size=3, colour="firebrick")+
  geom_point(aes(colour=n), size=5, show.legend = F)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_colour_viridis_c()+
  xlab("\nMonths Leading to and Following Gastric Bypass Surgery")+ ylab("Weighted Average BMI (kg/m2)\n")



data.frame(OBE_Japan_Drug_Histories %>% ungroup()%>%
             filter(MonthIndex<19)%>%
             summarise(n=weighted.mean(BMI, as.numeric(weight), na.rm = T)))




# ----
# Physicians prescribing drugs classes -----------------------------------
OBE_Japan_Doses <- read.table("OBE Japan Doses.txt", 
                              header = T, sep="\t", quote="", 
                              colClasses = "character", stringsAsFactors = FALSE)

Medical_facility <- read.csv("Medical_facility.csv")

data.frame(OBE_Japan_Doses %>% left_join(Medical_facility %>% select(medical_facility_id, vanguard_class), by=c("facility"="medical_facility_id"))%>%
             group_by(drug_class, vanguard_class) %>% summarise(n=n()))




DIA_Japan_Doses <- read.table("DIA Japan Doses_v2.txt", 
                              header = T, sep=",", quote="", 
                              colClasses = "character", stringsAsFactors = FALSE)

Medical_facility <- read.csv("Medical_facility.csv")

data.frame(DIA_Japan_Doses %>% left_join(Medical_facility %>% select(medical_facility_id, vanguard_class), by=c("facility"="medical_facility_id"))%>%
             group_by(drug_group, vanguard_class) %>% summarise(n=n()))

# ----
# Diabetes vs Diabetes+Obesity pats ---------------------
DANU_Japan_Demographics <- read.table("DANU Japan Demographics.txt", 
                                      header = T, sep="\t", quote="", 
                                      colClasses = "character", stringsAsFactors = FALSE)

DANU_Japan_Demographics <- DANU_Japan_Demographics %>% filter(diagnosis == "Diabetes + Obesity" | diagnosis == "Diabetes")

DIA_OBE_pats <- DANU_Japan_Demographics %>% filter(diagnosis == "Diabetes + Obesity") %>% select(patid)
DIA_pats <- DANU_Japan_Demographics %>% filter(diagnosis == "Diabetes") %>% select(patid)


DIA_Japan_Box_histories <- read.table("Dia Japan Box Histories_v2.1.txt", 
                                      header = T, sep=",", quote="", 
                                      colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Box_histories <- DIA_Japan_Box_histories %>% select(patient, weight, month60)

DIA_Japan_Box_histories %>% inner_join(DIA_OBE_pats, by=c("patient"="patid")) %>% group_by(month60) %>% summarise(N=sum(as.numeric(weight)))
# 


DIA_Japan_Box_histories %>% inner_join(DIA_pats, by=c("patient"="patid")) %>% group_by(month60) %>% summarise(N=sum(as.numeric(weight)))



# File with HbA1c over time
HbA1cHist <- read.table("HbA1cHist.txt", 
                        header = T, sep="\t", 
                        colClasses = "character", stringsAsFactors = FALSE)

names(HbA1cHist)[1] <- "patient"
HbA1cHist <- HbA1cHist %>% select(-c(weight))


DIA_Japan_Box_histories <- read.table("Dia Japan Box Histories_v2.1.txt", 
                                      header = T, sep=",", quote="", 
                                      colClasses = "character", stringsAsFactors = FALSE)

DIA_Japan_Box_histories <- DIA_Japan_Box_histories %>% select(patient, weight)

HbA1cHist <- DIA_Japan_Box_histories %>% left_join(HbA1cHist)


HbA1cHist %>% inner_join(DIA_OBE_pats, by=c("patient"="patid")) %>% 
  gather(Month, HbA1c, X1:X60, factor_key=TRUE) %>% separate_rows(HbA1c, sep = ",", convert=T )%>%
  summarise(meanHbA1c = weighted.mean(HbA1c, as.numeric(weight), na.rm = T)) #7.02

HbA1cHist %>% inner_join(DIA_OBE_pats, by=c("patient"="patid")) %>% 
  gather(Month, HbA1c, X1:X60, factor_key=TRUE) %>% separate_rows(HbA1c, sep = ",", convert=T )%>%
  summarise(meanHbA1c = weighted.median(HbA1c, as.numeric(weight), na.rm = T)) #6.75

HbA1cHist %>% inner_join(DIA_pats, by=c("patient"="patid")) %>% 
  gather(Month, HbA1c, X1:X60, factor_key=TRUE) %>% separate_rows(HbA1c, sep = ",", convert=T )%>%
  summarise(meanHbA1c = weighted.mean(HbA1c, as.numeric(weight),na.rm = T)) #6.84

HbA1cHist %>% inner_join(DIA_pats, by=c("patient"="patid")) %>% 
  gather(Month, HbA1c, X1:X60, factor_key=TRUE) %>% separate_rows(HbA1c, sep = ",", convert=T )%>%
  summarise(meanHbA1c = weighted.median(HbA1c,as.numeric(weight), na.rm = T)) #6.65


# -----
# Penetrance comorbidites as a function of BMI buckets ------------------------------
#Obesity pats to track
OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt",  header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(patient)

#All BMIs and 
DANU_Japan_Events <- read.table("DANU Japan Events.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

DANU_Japan_Events <- OBE_Japan_Drug_Histories %>% left_join(DANU_Japan_Events, by=c("patient"="patid"))
# length(unique(DANU_Japan_Events$patient))  #446873

DANU_Japan_Events <- DANU_Japan_Events %>% filter(grepl('BMI', code))

DANU_Japan_Events$code2 <- parse_number(DANU_Japan_Events$code)
# length(unique(DANU_Japan_Events$patient))  #443339

DANU_Japan_Events <- DANU_Japan_Events %>% group_by(patient) %>% arrange(claimed)

DANU_Japan_Events <- DANU_Japan_Events %>% group_by(patient) %>% filter(row_number()==n())
DANU_Japan_Events <- DANU_Japan_Events %>% select(patient, weight, code2)
names(DANU_Japan_Events)[3] <- "Last_BMI"
DANU_Japan_Events <- DANU_Japan_Events %>% mutate(BMI_Bucket = ifelse(Last_BMI <25, "<25",
                                                                      ifelse(Last_BMI>=25 & Last_BMI <27, "25-27",
                                                                             ifelse(Last_BMI>=27 & Last_BMI <30, "27-30", ">30"))))


DANU_Japan_Events %>% group_by(BMI_Bucket) %>% summarise(n=sum(as.numeric(weight)))



OBE_Specific_comorbDxs_MarksDB_OBEPts <- fread("OBE_Specific_comorbDxs_MarksDB_OBEPts.txt", sep=",", colClasses = "character", quote = "\"", stringsAsFactors = FALSE)

OBE_Specific_comorbDxs_MarksDB_OBEPts <- OBE_Specific_comorbDxs_MarksDB_OBEPts %>% select(member_id, Category) %>% distinct()

summary_comorbidities_BMI_bucket <- data.frame(DANU_Japan_Events %>% left_join(OBE_Specific_comorbDxs_MarksDB_OBEPts, by=c("patient"="member_id")) %>% 
                                                 group_by(BMI_Bucket, Category) %>% summarise(n=sum(as.numeric(weight))) %>% 
                                                 mutate(penetrance = ifelse(BMI_Bucket==">30",n/2485028, 
                                                                            ifelse(BMI_Bucket=="27-30",n/5165254, n/10711552))))




# BMIs increase/decrease year over year -----------------------------------------
#Obesity pats to track
OBE_Japan_Drug_Histories <- read.table("OBE Japan Drug Histories.txt",  header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

OBE_Japan_Drug_Histories <- OBE_Japan_Drug_Histories %>% select(patient)

DANU_Japan_Events <- read.table("DANU Japan Events.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

DANU_Japan_Events <- OBE_Japan_Drug_Histories %>% left_join(DANU_Japan_Events, by=c("patient"="patid"))

DANU_Japan_Events <- DANU_Japan_Events %>% filter(grepl('BMI', code))

DANU_Japan_Events$code2 <- parse_number(DANU_Japan_Events$code)

DANU_Japan_Events <- DANU_Japan_Events %>% group_by(patient) %>% arrange(claimed)

DANU_Japan_Events <- DANU_Japan_Events %>% select(-c(code))

names(DANU_Japan_Events)[4] <- "BMI"

DANU_Japan_Events <- DANU_Japan_Events %>% mutate(claimed = as.Date(claimed))

DANU_Japan_Events <- DANU_Japan_Events %>% ungroup() %>% filter(claimed >= "2016-08-01")

DANU_Japan_Events <- DANU_Japan_Events %>% mutate(Year = ifelse(claimed <= "2017-07-31", "Year1",
                                                                ifelse(claimed <= "2018-07-31", "Year2",
                                                                       ifelse(claimed <= "2019-07-31", "Year3",
                                                                              ifelse(claimed <= "2020-07-31", "Year4",
                                                                                     ifelse(claimed <= "2021-07-31", "Year5",claimed))))))


DANU_Japan_Events <- DANU_Japan_Events %>% mutate(BMI_Bucket = ifelse(BMI>=25 & BMI <27, "25-27",
                                                                      ifelse(BMI>=27 & BMI <30, "27-30", ">30")))

# the first BMI per patient on each Year
DANU_Japan_Events_first <- DANU_Japan_Events %>% group_by(patient, Year) %>% slice(1)

DANU_Japan_Events_first <- DANU_Japan_Events_first %>% ungroup() %>% group_by(patient) %>% filter(n()==5)

DANU_Japan_Events_first %>% group_by(Year, BMI_Bucket) %>% summarise(n=(sum(as.numeric(weight))))



Wide_format <- DANU_Japan_Events_first %>% group_by(patient) %>%
  mutate(BMI_Bucket=ifelse(BMI_Bucket==">30",3, 
                           ifelse(BMI_Bucket=="27-30",2,1)))%>%
  spread(key = Year, value = BMI_Bucket) %>%
  select(-c(claimed, BMI))%>%
  replace(is.na(.), 0) %>%
  group_by(patient) %>% summarise(across(everything(),max))


Wide_format %>% filter(Year5==3 & Year4 ==2) %>% summarise(n=sum(as.numeric(weight))) #145007
Wide_format %>% filter(Year5==3 & Year4 ==1) %>% summarise(n=sum(as.numeric(weight))) #1511
Wide_format %>% filter(Year5==3 & Year4 ==3) %>% summarise(n=sum(as.numeric(weight))) #624320


Wide_format %>% filter(Year5==2 & Year4 ==3) %>% summarise(n=sum(as.numeric(weight))) #80134
Wide_format %>% filter(Year5==2 & Year4 ==1) %>% summarise(n=sum(as.numeric(weight))) #282629
Wide_format %>% filter(Year5==2 & Year4 ==2) %>% summarise(n=sum(as.numeric(weight))) #1133520


Wide_format %>% filter(Year5==1 & Year4 ==3) %>% summarise(n=sum(as.numeric(weight))) #4266
Wide_format %>% filter(Year5==1 & Year4 ==2) %>% summarise(n=sum(as.numeric(weight))) #203445
Wide_format %>% filter(Year5==1 & Year4 ==1) %>% summarise(n=sum(as.numeric(weight))) #904329



BMI_evolution_switch <- read.csv("BMI_evolution_switch.csv")
names(BMI_evolution_switch)[1] <- "From"


cols <- c("BMI 25-27" = "#CCCCCC", "BMI 27-30" = "#006699", "BMI 30+" = "#CC0033")

BMI_evolution_switch %>%
  ggplot(aes(x = as.factor(To), y = as.factor(From), colour = as.factor(To),
             size = n)) +
  geom_point() +
  scale_colour_manual(values = cols)+
  geom_text(aes(label = paste(round(n, digits = 0),"%")), 
            colour = "black", 
            size = 3) +
  scale_x_discrete(position = "top") + scale_y_discrete(limits=rev)+
  scale_size_continuous(range = c(10, 30)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())


# ----
# Patients ON Insulin m60, with flow last year, exclusively intrainsulin flow --------------------------------
# Select pats ON Insulin at m60 and with flows last year (496k)
DIA_Flows_Aux._Long <- read.table("DIA_Flows_Aux._Long.txt", 
                                  header = T, sep=",", 
                                  colClasses = "character", stringsAsFactors = FALSE)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% filter(p1>=48)

Insulin_pats <- DIA_Flows_Aux._Long %>% filter(p2>=60) %>% filter(s2=="I") %>% select(patient, weight)

pat_flows_last_year <- DIA_Flows_Aux._Long %>% filter(flow == "1") %>% select(patient)

Insulin_Pats_Flow_lastyear <- Insulin_pats %>% inner_join(pat_flows_last_year) %>% distinct() #496007.2


# Select pats with only Insulin to Insulin flows last year
Intraflows_HighGranularity_table <- fread("Intraflows_HighGranularity_table.csv")

Intraflows_HighGranularity_table <- Intraflows_HighGranularity_table %>% mutate(p1 = as.numeric(p1)) %>% mutate(p2 = as.numeric(p2))

Intraflows_HighGranularity_table <- Intraflows_HighGranularity_table %>% filter(p1>=48)


All_Intra_Flows_pats <- Intraflows_HighGranularity_table %>% select(patient) %>% distinct()

# All intraflows, excluding the exclusive intra insulin flows
Remove_flows_intrainsulin <- Intraflows_HighGranularity_table %>% 
  filter(!(s2=="I" & big_flow_type=="-" & antiD_flow_type=="-" & DPP4_flow_type=="-" & SGLT2_flow_type=="-" & GLP1O_flow_type=="-" & GLP1I_flow_type=="-" & Ins_flow_type!="-"))

Remove_flows_intrainsulin %>% select(patient) %>% distinct()

# of all intraflows, which ones did NOT remain? These are the patients with only Insulin intraflows
Pats_ONLY_intra_insulin <- All_Intra_Flows_pats %>% anti_join(Remove_flows_intrainsulin)

# Pats ON Insulin with flow last year
Insulin_Pats_Flow_lastyear %>% summarise(n=sum(as.numeric(weight))) #496007.2
# Remove those with only insulin intraflas
Insulin_Pats_Flow_lastyear %>% anti_join(Pats_ONLY_intra_insulin) %>% summarise(n=sum(as.numeric(weight))) #398149.7

# ----------
# Drug Experience ~ stock month60 --------

DIA_Japan_Drug_Histories <- fread("DIA Japan Drug Histories_v2.txt")
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(patient, month1:month60)
DIA_Japan_Drug_Histories <- gather(DIA_Japan_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-Month) %>% filter(Drugs != "-")
Treatment_exp_Vector <- DIA_Japan_Drug_Histories %>% select(patient) %>% distinct()

fwrite(Treatment_exp_Vector, "Treatment_exp_Vector.txt", sep="\t")


DANU_Ingredients <- read.table("DANU Japan Ingredients.txt", header = T, sep="\t", quote="", colClasses = "character", stringsAsFactors = FALSE)
DIA_Flows_Aux._Long     <- fread("DIA_Flows_Aux._Long_v2.1.txt", integer64 = "character", stringsAsFactors = F)
Treatment_exp_Vector   <-fread("Treatment_exp_Vector.txt")

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% select(-c(disease, starts, stops, re_starts))

DIA_Flows_Aux._Long <- Treatment_exp_Vector %>% left_join(DIA_Flows_Aux._Long)

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1_OralExp = ifelse(grepl("38",d1)|grepl("38",d2),1,0))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = cumsum(p1_OralExp))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_OralExp = ifelse(p1_OralExp==0,0,1))

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1_InsulinExp = ifelse(grepl("44",d1)|grepl("45",d1)|grepl("46",d1)|grepl("47",d1)|grepl("48",d1)|grepl("49",d1)|grepl("50",d1)|grepl("51",d1)|grepl("52",d1)|grepl("53",d1)|grepl("54",d1)|grepl("55",d1)|grepl("56",d1)|grepl("57",d1)|
                                                                           grepl("44",d2)|grepl("45",d2)|grepl("46",d2)|grepl("47",d2)|grepl("48",d2)|grepl("49",d2)|grepl("50",d2)|grepl("51",d2)|grepl("52",d2)|grepl("53",d2)|grepl("54",d2)|grepl("55",d2)|grepl("56",d2)|grepl("57",d2),1,0))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InsulinExp = cumsum(p1_InsulinExp))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InsulinExp = ifelse(p1_InsulinExp==0,0,1))

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1_InjExp = ifelse(grepl("39",d1)|grepl("40",d1)|grepl("41",d1)|grepl("42",d1)|grepl("43",d1)|grepl("39",d2)|grepl("40",d2)|grepl("41",d2)|grepl("42",d2)|grepl("43",d2),1,0))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = cumsum(p1_InjExp))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_InjExp = ifelse(p1_InjExp==0,0,1))

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1_SGLT2Exp = ifelse(grepl("32",d1)|grepl("33",d1)|grepl("34",d1)|grepl("35",d1)|grepl("36",d1)|grepl("37",d1)|grepl("32",d2)|grepl("33",d2)|grepl("34",d2)|grepl("35",d2)|grepl("36",d2)|grepl("37",d2),1,0))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SGLT2Exp = cumsum(p1_SGLT2Exp))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_SGLT2Exp = ifelse(p1_SGLT2Exp==0,0,1))

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1_DPP4Exp = ifelse(grepl("23",d1)|grepl("24",d1)|grepl("25",d1)|grepl("26",d1)|grepl("27",d1)|grepl("28",d1)|grepl("29",d1)|grepl("30",d1)|grepl("31",d1)|grepl("23",d2)|grepl("24",d2)|grepl("25",d2)|grepl("26",d2)|grepl("27",d2)|grepl("28",d2)|grepl("29",d2)|grepl("30",d2)|grepl("31",d2),1,0))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_DPP4Exp = cumsum(p1_DPP4Exp))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_DPP4Exp = ifelse(p1_DPP4Exp==0,0,1))

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1_AntiDiabeticExp = ifelse(grepl("(^|\\D)(8{1})(\\D|$)",d1)|grepl("(^|\\D)(9{1})(\\D|$)",d1)|grepl("10",d1)|grepl("11",d1)|grepl("12",d1)|grepl("13",d1)|grepl("14",d1)|grepl("15",d1)|grepl("16",d1)|grepl("17",d1)|grepl("18",d1)|grepl("19",d1)|grepl("20",d1)|grepl("21",d1)|grepl("22",d1)|
                                                                                                                            grepl("(^|\\D)(8{1})(\\D|$)",d2)|grepl("(^|\\D)(9{1})(\\D|$)",d2)|grepl("10",d2)|grepl("11",d2)|grepl("12",d2)|grepl("13",d2)|grepl("14",d2)|grepl("15",d2)|grepl("16",d2)|grepl("17",d2)|grepl("18",d2)|grepl("19",d2)|grepl("20",d2)|grepl("21",d2)|grepl("22",d2),1,0))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntiDiabeticExp = cumsum(p1_AntiDiabeticExp))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AntiDiabeticExp = ifelse(p1_AntiDiabeticExp==0,0,1))


DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1_BiguanideExp = ifelse(grepl("(^|\\D)(1{1})(\\D|$)",d1)|grepl("(^|\\D)(1{1})(\\D|$)",d2)|grepl("(^|\\D)(2{1})(\\D|$)",d1)|grepl("(^|\\D)(2{1})(\\D|$)",d2),1,0))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_BiguanideExp = cumsum(p1_BiguanideExp))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_BiguanideExp = ifelse(p1_BiguanideExp==0,0,1))

DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% mutate(p1_AdvancedExp = ifelse(grepl("38",d1)|grepl("38",d2)|grepl("44",d1)|grepl("45",d1)|grepl("46",d1)|grepl("47",d1)|grepl("48",d1)|grepl("49",d1)|grepl("50",d1)|grepl("51",d1)|grepl("52",d1)|grepl("53",d1)|grepl("54",d1)|grepl("55",d1)|grepl("56",d1)|grepl("57",d1)|
                                                                                grepl("44",d2)|grepl("45",d2)|grepl("46",d2)|grepl("47",d2)|grepl("48",d2)|grepl("49",d2)|grepl("50",d2)|grepl("51",d2)|grepl("52",d2)|grepl("53",d2)|grepl("54",d2)|grepl("55",d2)|grepl("56",d2)|grepl("57",d2)|
                                                                                grepl("39",d1)|grepl("40",d1)|grepl("41",d1)|grepl("42",d1)|grepl("43",d1)|grepl("39",d2)|grepl("40",d2)|grepl("41",d2)|grepl("42",d2)|grepl("43",d2)|grepl("32",d1)|grepl("33",d1)|grepl("34",d1)|grepl("35",d1)|grepl("36",d1)|grepl("37",d1)|grepl("32",d2)|grepl("33",d2)|grepl("34",d2)|grepl("35",d2)|grepl("36",d2)|grepl("37",d2),1,0))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AdvancedExp = cumsum(p1_AdvancedExp))
DIA_Flows_Aux._Long <- DIA_Flows_Aux._Long %>% group_by(patient) %>% mutate(p1_AdvancedExp = ifelse(p1_AdvancedExp==0,0,1))




Start_df <- DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2) %>% summarise(pats=sum(as.numeric(weight))) 



Start_df <- Start_df %>% left_join(DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_OralExp) %>% 
                                     summarise(pats_p1_OralExp=sum(as.numeric(weight))) %>% filter(p1_OralExp == 1))

Start_df <- Start_df %>% left_join(DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_InjExp) %>% 
                                     summarise(pats_p1_InjExp=sum(as.numeric(weight))) %>% filter(p1_InjExp == 1))

Start_df <- Start_df %>% left_join(DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_InsulinExp) %>% 
                                     summarise(pats_p1_InsulinExp=sum(as.numeric(weight))) %>% filter(p1_InsulinExp == 1))

Start_df <- Start_df %>% left_join(DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_SGLT2Exp) %>% 
                                     summarise(pats_p1_SGLT2Exp=sum(as.numeric(weight))) %>% filter(p1_SGLT2Exp == 1))

Start_df <- Start_df %>% left_join(DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_DPP4Exp) %>% 
                                     summarise(pats_p1_DPP4Exp=sum(as.numeric(weight))) %>% filter(p1_DPP4Exp == 1))

Start_df <- Start_df %>% left_join(DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_AntiDiabeticExp) %>% 
                                     summarise(pats_p1_AntiDiabeticExp=sum(as.numeric(weight))) %>% filter(p1_AntiDiabeticExp == 1))

Start_df <- Start_df %>% left_join(DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_BiguanideExp) %>% 
                                     summarise(pats_p1_BiguanideExp=sum(as.numeric(weight))) %>% filter(p1_BiguanideExp == 1))

Start_df <- Start_df %>% left_join(DIA_Flows_Aux._Long %>% filter(p2==60) %>% group_by(s2, p1_AdvancedExp) %>% 
                                     summarise(pats_p1_AdvancedExp=sum(as.numeric(weight))) %>% filter(p1_AdvancedExp == 1))


Start_df <- Start_df[,c(1,2,4,6,8,10,12,14,16,18)]


fwrite(Start_df, "Drug_exp_m60_raw_NoDPP4.txt", sep= "\t")


fwrite(Start_df, "Drug_exp_m60_raw.txt", sep= "\t")

fwrite(Start_df, "Drug_exp_m60.txt", sep= "\t")



# change the order and format and all that
Drug_exp_m60 <- fread("Drug_exp_m60_noDPP4.txt")


Drug_exp_m60 <- data.frame(lapply(Drug_exp_m60, function(x) if(is.numeric(x)) round(x, 0) else x))


row.names(Drug_exp_m60) <- Drug_exp_m60$Stock

Drug_exp_m60 <- Drug_exp_m60 %>% select(-c(Stock))


grid.bubble.plot <- function(df, 
                             axis_labels_size=8, 
                             aspect_ratio=1/1,
                             values_text_size=4,
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
  res_df <- data.frame(res_df %>% mutate(values_x=fct_relevel(values_x,c("Biguanide_Exp","Antidiabetic_Exp","DPP4_Exp","SGLT2_Exp","Oral_GLP1_Exp","Injectable_Exp","Insulin_Exp","Advanced_Exp"))) %>%
                         mutate(values_y=fct_relevel(values_y,c("Lapsed_Stock","Biguanide_Stock","Antidiabetic_Stock","DPP4_Stock","SGLT2_Stock","Oral_GLP1_Stock","Injectable_GLP1_Stock", "Insulin_Stock"))))
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



# -------
# Evolution of scripts over time ---------------

DIA_Doses_BIG <- fread("DIA Japan Doses_v2.txt")
DIA_Doses_BIG <- DIA_Doses_BIG %>% filter(status != "G")
DIA_Doses_BIG <- DIA_Doses_BIG %>% select(drug_id, generic_name, drug_group, pat_id, weight, from_dt)
setDT(DIA_Doses_BIG)[, Month_Yr := format(as.Date(from_dt), "%Y-%m") ]
DIA_Doses_BIG$Month_Yr <- format(as.Date(DIA_Doses_BIG$from_dt), "%Y-%m")
min(DIA_Doses_BIG$from_dt) #"2015-10-01"
max(DIA_Doses_BIG$from_dt) #"2021-06-30"

DIA_Doses_BIG <- DIA_Doses_BIG %>% filter(from_dt >= "2016-05-01") %>% filter(from_dt <= "2021-04-30")

length(unique(DIA_Doses_BIG$Month_Yr)) # 60

length(unique(DIA_Doses_BIG$pat_id)) #97592


DIA_Doses_BIG %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Scripts \n")


DIA_Doses_BIG %>% select(pat_id, weight, Month_Yr) %>% distinct() %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Patients \n")


data.frame(DIA_Doses_BIG %>% select(pat_id,weight, Month_Yr) %>% group_by(Month_Yr) %>% mutate(script_count = sum(as.numeric(weight))) %>% ungroup() %>%
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




OBE_Doses_BIG <- fread("OBE Japan Doses.txt")
OBE_Doses_BIG <- OBE_Doses_BIG %>% filter(status != "G")
OBE_Doses_BIG <- OBE_Doses_BIG %>% select(drug_id, generic_name, drug_group, pat_id, weight, from_dt)
setDT(OBE_Doses_BIG)[, Month_Yr := format(as.Date(from_dt), "%Y-%m") ]
OBE_Doses_BIG$Month_Yr <- format(as.Date(OBE_Doses_BIG$from_dt), "%Y-%m")
min(OBE_Doses_BIG$from_dt) #"2015-10-01"
max(OBE_Doses_BIG$from_dt) #"2021-06-30"

OBE_Doses_BIG <- OBE_Doses_BIG %>% filter(from_dt >= "2016-05-01") %>% filter(from_dt <= "2021-04-30")

length(unique(OBE_Doses_BIG$Month_Yr)) # 60

length(unique(OBE_Doses_BIG$pat_id)) #8629


OBE_Doses_BIG %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Scripts \n")


OBE_Doses_BIG %>% select(pat_id, weight, Month_Yr) %>% distinct() %>% group_by(Month_Yr) %>% summarise(n=sum(as.numeric(weight))) %>%
  ggplot(aes(x=Month_Yr, y=n))+
  geom_col(show.legend = F, fill="deepskyblue4", alpha=.6)+
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  xlab("\nMonth")+ylab("Number of Patients \n")


data.frame(OBE_Doses_BIG %>% select(pat_id,weight, Month_Yr) %>% group_by(Month_Yr) %>% mutate(script_count = sum(as.numeric(weight))) %>% ungroup() %>%
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


# -------
# MAX BMI  -------------------------------------------------------------------------------
DANU_Japan_Demographics <- fread("DANU Japan Demographics.txt")
DANU_Japan_Demographics <- DANU_Japan_Demographics %>% filter(diagnosis=="Obesity"|diagnosis=="Diabetes + Obesity")
sum(DANU_Japan_Demographics$weight) # 25489592
OBE_Pats <- DANU_Japan_Demographics %>% select(patid)
names(OBE_Pats)[1] <- "patient"


BMIHistAll <- read.table("BMIHistAll.txt", header = T, sep=",", colClasses = "character", stringsAsFactors = FALSE)
names(BMIHistAll)[1] <- "patient"
OBE_Pats <- BMIHistAll %>% inner_join(OBE_Pats)

OBE_Pats <- OBE_Pats %>% select(patient, weight, month1:month60)
OBE_Pats <- OBE_Pats %>% gather(Month, BMI, month1:month60, factor_key=TRUE)
OBE_Pats <- OBE_Pats %>% filter(BMI != "")
OBE_Pats <- separate_rows(OBE_Pats, BMI, sep = ",", convert=T )

OBE_Pats <- OBE_Pats %>% group_by(patient) %>% filter(BMI==max(BMI)) %>% slice(1) %>% select(-Month)

sum(as.numeric(OBE_Pats$weight)) #24033693

# 24033693/25489592 = 0.9428826

OBE_Pats$weight <- as.numeric(OBE_Pats$weight)

OBE_Pats$weight <- OBE_Pats$weight / 0.9428826

sum(as.numeric(OBE_Pats$weight)) #25489592

OBE_Pats %>% ungroup() %>% filter(BMI>30) %>% summarise(n=sum(weight)) # 4650114

# ------------------------------------
# Diabetes Experience Comorbidities GLP1 SGLT2 Combo ----------------------------
# ALL DIABETES TREAT-EXP
DIA_Japan_Drug_Histories <- fread("DIA Japan Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(4:63)
DIA_Japan_Drug_Histories[DIA_Japan_Drug_Histories != "-"] <- 1  # on drug 
DIA_Japan_Drug_Histories[DIA_Japan_Drug_Histories == "-"] <- 0  # no drug
DIA_Japan_Drug_Histories[] <- lapply(DIA_Japan_Drug_Histories,as.numeric)
DIA_Japan_Drug_Histories$SUM <- rowSums(DIA_Japan_Drug_Histories)

DIA_Japan_Drug_Histories_LONG <- fread("DIA Japan Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
Pats_vec <- DIA_Japan_Drug_Histories_LONG %>% select(patient, weight)
DIA_Japan_Drug_Histories <- Pats_vec %>% bind_cols(DIA_Japan_Drug_Histories)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% filter(SUM != 0)
sum(as.numeric(DIA_Japan_Drug_Histories$weight)) # 8197598
Treatment_exp_Vector <- DIA_Japan_Drug_Histories %>% select(patient, weight)
sum(as.numeric(Treatment_exp_Vector$weight)) # 8197598
Treatment_exp_Vector$weight <- as.numeric(Treatment_exp_Vector$weight)

# HbA1!C <= 10.5
DANU_Measures <- fread("HbA1cHist.txt",  integer64 = "character", stringsAsFactors = F)
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- DANU_Measures %>% inner_join(Treatment_exp_Vector, by=c("patient"="patient"))
DANU_Measures <- DANU_Measures %>% select(patient, weight.x, `1`:`60`)
DANU_Measures <- gather(DANU_Measures, Month, HbA1c,`1`:`60`, factor_key=TRUE)
DANU_Measures <- DANU_Measures %>%  select(patient, weight.x, HbA1c) %>% distinct() %>% filter(HbA1c != "")
DANU_Measures <- separate_rows(DANU_Measures, HbA1c, sep = ",", convert=T )
DANU_Measures <- DANU_Measures %>%  select(patient, weight.x, HbA1c) %>% distinct() %>% filter(HbA1c != "")
DANU_Measures <- DANU_Measures  %>% mutate(HbA1c=as.numeric(HbA1c)) %>% group_by(patient) %>%  filter(HbA1c==max(HbA1c)) %>% slice(1)
names(DANU_Measures)[2] <- "weight"
DANU_Measures %>% ungroup() %>% summarise(n=sum(weight)) # 5457419 factor 0.6657339  or 1.502102 x
DANU_Measures %>% filter(HbA1c<=10.5) %>% ungroup() %>% summarise(n=sum(weight)) # 5206572  (0.9540356 prop.)
DANU_Measures <- DANU_Measures %>% filter(HbA1c<=10.5) %>% ungroup()  %>% select(patient, weight)
PatsToTrack <- DANU_Measures

# ICD10 Dx CVD vs CKD
DANU_Japan_Disorder_Codes <- fread("DANU Japan Disorder Codes.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Disorder_Codes <- DANU_Japan_Disorder_Codes %>% select(code, description, icd10_code)
unique(DANU_Japan_Disorder_Codes$icd10_code)

DANU_Japan_Disorder_Dossiers <- fread("DANU Japan Disorder Dossiers.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Disorder_Dossiers <- DANU_Japan_Disorder_Dossiers %>% select(patid, code)
names(DANU_Japan_Disorder_Dossiers)[1] <- "patient"
DANU_Japan_Disorder_Dossiers <- DANU_Japan_Disorder_Dossiers %>% inner_join(PatsToTrack)
DANU_Japan_Disorder_Dossiers %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 4107397 

DANU_Japan_Disorder_Codes %>% filter(grepl("I21", icd10_code)|grepl("I22", icd10_code)|grepl("I25", icd10_code)|grepl("I63", icd10_code)|
                                         grepl("I75", icd10_code)|grepl("I74", icd10_code)|grepl("I70", icd10_code)) %>% select(code) %>%
  inner_join(DANU_Japan_Disorder_Dossiers) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 854590.8


CVD <- DANU_Japan_Disorder_Codes %>% filter(grepl("I21", icd10_code)|grepl("I22", icd10_code)|grepl("I25", icd10_code)|grepl("I63", icd10_code)|
                                         grepl("I75", icd10_code)|grepl("I74", icd10_code)|grepl("I70", icd10_code)) %>% select(code) %>%
  inner_join(DANU_Japan_Disorder_Dossiers) %>% select(patient, weight) 

DANU_Japan_Disorder_Codes %>% filter(grepl("N18", icd10_code)) %>% select(code, icd10_code) %>%
  inner_join(DANU_Japan_Disorder_Dossiers) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 217300.5


CKD <- DANU_Japan_Disorder_Codes %>% filter(grepl("N18", icd10_code)) %>% select(code, icd10_code) %>%
  inner_join(DANU_Japan_Disorder_Dossiers) %>% select(patient, weight) 

# DANU_Japan_Disorder_Codes %>% filter(grepl("N184", icd10_code)) %>% select(code, icd10_code) %>%
#   inner_join(DANU_Japan_Disorder_Dossiers) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 6259.28
# 
# DANU_Japan_Disorder_Codes %>% filter(grepl("N183", icd10_code)) %>% select(code, icd10_code) %>%
#   inner_join(DANU_Japan_Disorder_Dossiers) %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 21878.02

CVD <- CVD[,1]
CKD <- CKD[,1]

# Drugs for everyone 

DANU_Japan_Ingredients <- read.table("DANU Japan Ingredients.txt", 
                                     header = T, sep="\t", quote="", 
                                     colClasses = "character", stringsAsFactors = FALSE)

DANU_Japan_Ingredients       <- DANU_Japan_Ingredients %>%  separate(drug_id, c('class', 'molecule'))
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% select(molecule, generic_name)
DANU_Japan_Ingredients <- DANU_Japan_Ingredients %>% filter(generic_name=="Empagliflozin"|generic_name=="Canagliflozin"|generic_name=="Dapagliflozin"|generic_name=="Ertugliflozin"|
                              generic_name=="Exenatide"|generic_name=="Liraglutide"|generic_name=="Albiglutide"|generic_name=="Dulaglutide"|generic_name=="Lixisenatide"|generic_name=="Semaglutide Injectable")
DANU_Japan_Ingredients$molecule <- as.numeric(DANU_Japan_Ingredients$molecule)

DIA_Japan_Drug_Histories <- fread("DIA Japan Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DIA_Japan_Drug_Histories       <- gather(DIA_Japan_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
DIA_Japan_Drug_Histories$Month <- as.character(DIA_Japan_Drug_Histories$Month)
DIA_Japan_Drug_Histories$Month <- parse_number(DIA_Japan_Drug_Histories$Month)
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% group_by(patient) %>% filter(Month==60)
DIA_Japan_Drug_Histories <- separate_rows(DIA_Japan_Drug_Histories, Drugs, sep = ",", convert=T )
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% select(-c(disease, Month))
names(DIA_Japan_Drug_Histories)[3] <- "molecule"
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% left_join(DANU_Japan_Ingredients %>% mutate(molecule = as.character(molecule)))
DIA_Japan_Drug_Histories <- DIA_Japan_Drug_Histories %>% mutate(Class=ifelse(grepl("flozin",generic_name),"SGLT2",
                                                                 ifelse(is.na(generic_name),"none", "GLP1")))
DIA_Japan_Drug_Histories$weight <- as.numeric(DIA_Japan_Drug_Histories$weight)


# CVD Drugs
CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct()) %>%
  summarise(n=sum(weight)) # 13630.58 combo


CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct())) %>%
  summarise(n=sum(weight)) # 20210.24 GLP1 only

CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct())) %>%
  summarise(n=sum(weight)) # 160870.2 sglt2 only

CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="none"&molecule!="-") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CVD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1"|Class=="SGLT2") %>% select(patient, weight) %>% distinct()) %>%
  summarise(n=sum(weight)) # 547440.2




# CKD Drugs
CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct()) %>%
  summarise(n=sum(weight)) # 4369.69 combo   6563.72


CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct())) %>%
  summarise(n=sum(weight)) # 10394.35 GLP1 only  15613.37

CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct())) %>%
  summarise(n=sum(weight)) # 37323.76 sglt2 only  56064.09

CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="none"&molecule!="-") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CKD %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1"|Class=="SGLT2") %>% select(patient, weight) %>% distinct()) %>%
  summarise(n=sum(weight)) # 120042.3   180315.8





CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories %>% select(patient, weight) %>% distinct()) %>% 
  summarise(n=sum(weight)) * 1.502102 *0.28
 

CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct()) %>%
  summarise(n=sum(weight)) * 1.502102 * 0.28 # 263.1707

CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct())) %>%
  summarise(n=sum(weight)) * 1.502102 * 0.28 # 1812.552

CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="SGLT2") %>% select(patient, weight) %>% distinct() %>%
  inner_join(CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1") %>% select(patient, weight) %>% distinct())) %>%
  summarise(n=sum(weight)) * 1.502102 * 0.28 # 4994.203

CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="none"&molecule!="-") %>% select(patient, weight) %>% distinct() %>%
  anti_join(CVD %>% inner_join(CKD) %>% left_join(DIA_Japan_Drug_Histories) %>% filter(Class=="GLP1"|Class=="SGLT2") %>% select(patient, weight) %>% distinct()) %>%
  summarise(n=sum(weight))  * 1.502102 * 0.28 # 14418.88



# ------------------------------------------------------------------------

# # Estimate NASH /NAFLD pop base don lab tests ------------------------------------------
  
Annual_health_checkup <- fread("Annual_health_checkup.csv")
names(Annual_health_checkup)
Annual_health_checkup

# Annual_health_checkup <- Annual_health_checkup %>% select(member_id, bmi, abdominal_circumference, 
#                                                           systolic_bp, diastolic_bp, triglyceride, hdl_cholesterol, ldl_cholesterol, 
#                                                           ast, alt, g_gt, fasting_blood_sugar,
#                                                           casual_blood_sugar, hba1c, urinary_sugar, hct, hemoglobin_content, rbc_count,
#                                                           serum_uric_acid, serum_creatinine)
names(Annual_health_checkup)[1] <- "patient"
length(unique(Annual_health_checkup$patient)) # 2816028

DIA_Japan_Drug_Histories <- fread("DIA Japan Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
length(unique(DIA_Japan_Drug_Histories$patient)) # 242002

Annual_health_checkup <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% left_join(Annual_health_checkup) 
length(unique(Annual_health_checkup$patient)) # 242002
sum(as.numeric(DIA_Japan_Drug_Histories$weight)) # 17895480

Annual_health_checkup %>% select(ast) %>% drop_na() %>% summarise(n=mean(ast)) # 26.42692
Annual_health_checkup %>% select(alt) %>% drop_na() %>% summarise(n=mean(alt)) # 32.82255
Annual_health_checkup %>% select(g_gt) %>% drop_na() %>% summarise(n=mean(g_gt)) # 53.02265
Annual_health_checkup %>% select(serum_creatinine) %>% drop_na() %>% summarise(n=mean(serum_creatinine)) # 0.8322618
Annual_health_checkup %>% select(hdl_cholesterol) %>% drop_na() %>% summarise(n=mean(hdl_cholesterol)) # 57.64742
Annual_health_checkup %>% select(triglyceride) %>% drop_na() %>% summarise(n=mean(triglyceride)) # 143.065

Annual_health_checkup %>% select(patient, ast, alt, g_gt, bmi) %>% drop_na() %>% select(patient) %>% distinct()

# Patients with Fatty Liver
DANU_Japan_Disorder_Codes <- fread("DANU Japan Disorder Codes.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Disorder_Codes <- DANU_Japan_Disorder_Codes %>% select(code, description, icd10_code, icd10_description)
unique(DANU_Japan_Disorder_Codes$icd10_code)
DANU_Japan_Disorder_Codes <- DANU_Japan_Disorder_Codes %>% filter(icd10_code=="K760") %>% select(code)
DANU_Japan_Disorder_Dossiers <- fread("DANU Japan Disorder Dossiers.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
DANU_Japan_Disorder_Dossiers <- DANU_Japan_Disorder_Dossiers %>% select(patid, code)
names(DANU_Japan_Disorder_Dossiers)[1] <- "patient"
DANU_Japan_Disorder_Dossiers <- DANU_Japan_Disorder_Dossiers %>% inner_join(Annual_health_checkup %>% select(patient) %>% distinct())
FattyLiver_Pats <- DANU_Japan_Disorder_Dossiers %>% inner_join(DANU_Japan_Disorder_Codes) %>% select(patient) %>% distinct()

FattyLiver_Pats %>% left_join(DIA_Japan_Drug_Histories) %>% summarise(n=sum(as.numeric(weight))) # 1950427
# 30200/242002 12%

FattyLiver_Pats %>% left_join(Annual_health_checkup) %>% select(ast) %>% drop_na() %>% summarise(n=mean(ast)) # 30.63532
FattyLiver_Pats %>% left_join(Annual_health_checkup) %>% select(alt) %>% drop_na() %>% summarise(n=mean(alt)) # 42.94562
FattyLiver_Pats %>% left_join(Annual_health_checkup) %>% select(g_gt) %>% drop_na() %>% summarise(n=mean(g_gt)) # 64.25675
FattyLiver_Pats %>% left_join(Annual_health_checkup) %>% select(serum_creatinine) %>% drop_na() %>% summarise(n=mean(serum_creatinine)) # 0.822888
FattyLiver_Pats %>% left_join(Annual_health_checkup) %>% select(hdl_cholesterol) %>% drop_na() %>% summarise(n=mean(hdl_cholesterol)) # 54.22455
FattyLiver_Pats %>% left_join(Annual_health_checkup) %>% select(triglyceride) %>% drop_na() %>% summarise(n=mean(triglyceride)) # 161.0592

FattyLiver_Pats %>% left_join(Annual_health_checkup) %>% filter(ast>100&alt>100&g_gt>100) %>%
  select(bmi  ) %>% drop_na() %>% summarise(n=mean(bmi  )) # 161.0592



Annual_health_checkup %>% left_join(FattyLiver_Pats %>% mutate(Group="NAFLD")) %>% select(Group, ast) %>%
   filter(!is.na(ast)) %>% group_by(Group) %>% summarise(n=mean(ast)) 

  Group     n
  <chr> <dbl>
1 NAFLD  30.6
2 NA     25.8

Annual_health_checkup %>% left_join(FattyLiver_Pats %>% mutate(Group="NAFLD")) %>% select(Group, alt) %>%
   filter(!is.na(alt)) %>% group_by(Group) %>% summarise(n=mean(alt)) 

  Group     n
  <chr> <dbl>
1 NAFLD  42.9
2 NA     31.4

Annual_health_checkup %>% left_join(FattyLiver_Pats %>% mutate(Group="NAFLD")) %>% select(Group, g_gt) %>%
   filter(!is.na(g_gt)) %>% group_by(Group) %>% summarise(n=mean(g_gt)) 

  Group     n
  <chr> <dbl>
1 NAFLD  64.3
2 NA     51.4

temp <- Annual_health_checkup %>% select(patient, bmi, abdominal_circumference, triglyceride, ldl_cholesterol, hdl_cholesterol, hba1c,
                                 ast, alt, g_gt) %>% drop_na() %>%
  left_join(FattyLiver_Pats %>% mutate(Group="FattyLiver")) %>% mutate(Group=ifelse(is.na(Group), "No", Group))

for (i in names(temp[,2:10])){
  print(i)
  print(temp  %>% group_by(Group) %>% summarise(n=mean(get(i))))
}


temp <- Annual_health_checkup %>% select(patient, ast, alt, g_gt) %>% drop_na() %>%
  left_join(FattyLiver_Pats %>% mutate(Group="FattyLiver")) %>% mutate(Group=ifelse(is.na(Group), "No", Group))

temp %>% mutate(Group=as.factor(Group)) %>%
  mutate(Group=ifelse(Group=="FattyLiver","NAFLD Dx", "No Dx")) %>%
  filter(g_gt<=300) %>%
  ggplot((aes(g_gt, colour=Group, fill=Group))) +
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("brown3", "azure4")) +
    scale_colour_manual(values = c("brown3", "azure4")) +
  theme_minimal() +
  xlab("\n GGT (IU/L)") +
  ylab("Patient (kernel) density \n")


#temp$Group <- as.factor(temp$Group)
temp <- temp[,2:5]
temp <- temp %>% mutate(Group = ifelse(Group=="FattyLiver", 1, 0))
#temp$Group <- as.factor(temp$Group)
# temp$ast <- scale(temp$ast)
# temp$alt <- scale(temp$alt)
# temp$g_gt <- scale(temp$g_gt)
temp <- temp %>% sample_n(1540315)
temp %>% group_by(Group) %>% count()
temp <- temp %>% group_by(Group) %>% sample_n(20000) %>% ungroup()

library(randomForest)



summary(randomForest(Group ~ ., data = temp))
modelAll_1_rf <- randomForest(Group ~ ., data = temp)


predict <- predict(modelAll_1_rf, temp, type = 'response')

ignore <- temp %>% bind_cols(data.frame(predict))

ignore %>% mutate(Group=as.factor(Group)) %>%
  mutate(Group=ifelse(Group==1,"NAFLD Dx", "No Dx")) %>%
  ggplot((aes(predict, colour=Group, fill=Group))) +
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("brown3", "azure4")) +
    scale_colour_manual(values = c("brown3", "azure4")) +
  theme_minimal() +
  xlab("\n Predicted Probability / Propensity Score") +
  ylab("Patient (kernel) density \n")



ignore %>% mutate(Group2=ifelse(predict>0.625,"Yes","No")) %>%
  group_by(Group, Group2) %>% count()

# table_mat <- table(temp$Group, predict > 0.50)
# table_mat
# 
# accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
# accuracy_Test

library(DALEX)
explainer_ranger <- explain(modelAll_1_rf,
                            data = ignore,
                            y =  ignore$Group,
                            label = "model_rf")


print(explainer_ranger)
plot(explainer_ranger)
describe(explainer_ranger)


new_observation <- temp[65,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
# ------------------------
# Apply NASH model from MDV -------------------------
  
load(file = "MDV_NASH_RF_model.rda")

Annual_health_checkup <- fread("Annual_health_checkup.csv")
names(Annual_health_checkup)
Annual_health_checkup

Annual_health_checkup <- Annual_health_checkup %>% select(member_id, date_of_health_checkup, ast, alt, g_gt)
names(Annual_health_checkup)[1] <- "patient"
length(unique(Annual_health_checkup$patient)) # 2816028

DIA_Japan_Drug_Histories <- fread("DIA Japan Drug Histories.txt", header = T, sep="\t", colClasses = "character", stringsAsFactors = FALSE)
length(unique(DIA_Japan_Drug_Histories$patient)) # 242002

Annual_health_checkup <- DIA_Japan_Drug_Histories %>% select(patient, weight) %>% left_join(Annual_health_checkup) 
length(unique(Annual_health_checkup$patient)) # 242002
sum(as.numeric(DIA_Japan_Drug_Histories$weight)) # 17,895,480

Annual_health_checkup %>% select(ast) %>% drop_na() %>% summarise(n=mean(ast)) # 26.42692
Annual_health_checkup %>% select(alt) %>% drop_na() %>% summarise(n=mean(alt)) # 32.82255
Annual_health_checkup %>% select(g_gt) %>% drop_na() %>% summarise(n=mean(g_gt)) # 53.02265

Annual_health_checkup %>% select(patient, ast, alt, g_gt) %>% drop_na() %>% select(patient) %>% distinct() # 213896

Annual_health_checkup <- Annual_health_checkup %>% select(patient, date_of_health_checkup , ast, alt, g_gt) %>% drop_na()
Annual_health_checkup <- Annual_health_checkup %>% group_by(patient) %>% filter(date_of_health_checkup==max(date_of_health_checkup)) 
length(unique(Annual_health_checkup$patient))  # 213896
Annual_health_checkup <- Annual_health_checkup %>% select(patient , ast, alt, g_gt) 

names(Annual_health_checkup)[2] <- "AST"
names(Annual_health_checkup)[3] <- "ALT"
names(Annual_health_checkup)[4] <- "GGT"

temp <- Annual_health_checkup %>% ungroup() %>% select(-patient)
temp <- temp %>% select(2,1,3)

predict <- predict(modelAll_1_rf, temp, type = 'response')

ignore <- Annual_health_checkup %>% bind_cols(data.frame(predict))

ignore %>% ungroup() %>%
  ggplot(aes( predict)) +
  geom_density(alpha=0.7, colour="brown3", fill="brown3") +
  theme_minimal() +
  xlab("\n Predicted Probability / Propensity Score") +
  ylab("Patient (kernel) density \n")

# ---------------------------------

