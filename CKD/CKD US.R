# CKD Libraries ----------

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

# ------------------

# Initial sizing ----------------------------------------
CKD_Drug_Histories <- fread("CKD Drug Histories.txt")
sum(CKD_Drug_Histories$weight) # 24160347

CKD_Box_Histories <- fread("CKD Box Histories.txt")
sum(CKD_Box_Histories$weight) # 24160347


# - only with N18
# - only drug-experienced
# - only alive up to m48

DANU_Dossiers_Full <- fread("DANU Dossiers Full.txt")
unique(DANU_Dossiers_Full$condition)
unique(DANU_Dossiers_Full$diagnosis)

DANU_Dossiers_Full %>% filter(diagnosis=="Kidney Disease") %>% 
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 24160347

DANU_Dossiers_Full %>% filter(grepl("N18",code)) %>%
  select(patid, weight) %>% distinct() %>% summarise(n=sum(weight)) # 18021492





CKD_Demographics <- fread("CKD Demographics.txt")
sum(CKD_Demographics$weight) # 24160347

CKD_Demographics %>% filter(died=="N" | death_date>"2020-05-01" ) %>% summarise(n=sum(weight)) # 21241904

CKD_Demographics %>% filter(died=="N" | death_date>"2020-05-01" ) %>%
  inner_join(DANU_Dossiers_Full %>% filter(grepl("N18",code)) %>%
  select(patid, weight) %>% distinct()) %>% summarise(n=sum(weight))  # 15717693






CKD_Ingredients <- fread("CKD Ingredients.txt",  colClasses = "character", stringsAsFactors = F)

CKD_Drug_Histories <- fread("CKD Drug Histories.txt")
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-" & Drugs != "133")
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient) %>% distinct()
names(CKD_Drug_Histories)[1] <- "patid"




CKD_Demographics %>% filter(died=="N" | death_date>"2020-05-01" ) %>%
  inner_join(DANU_Dossiers_Full %>% filter(grepl("N18",code)) %>%
  select(patid, weight) %>% distinct()) %>% 
  inner_join(CKD_Drug_Histories) %>%  summarise(n=sum(weight))  # 13384925

Pats_to_track <- 
CKD_Demographics %>% filter(died=="N" | death_date>"2020-05-01" ) %>%
  inner_join(DANU_Dossiers_Full %>% filter(grepl("N18",code)) %>%
  select(patid, weight) %>% distinct()) %>% 
  inner_join(CKD_Drug_Histories) %>% select(patid, weight)

fwrite(Pats_to_track, "Pats_to_track.txt", sep="\t")



# ---------------

# Stages ------------------------------

Pats_to_track <- fread("Pats_to_track.txt", sep="\t")
DANU_Dossiers_Full <- fread("DANU Dossiers Full.txt")
DANU_Dossiers_Full <- Pats_to_track %>% inner_join(DANU_Dossiers_Full)
DANU_Dossiers_Full <- DANU_Dossiers_Full %>% filter(grepl("N18", code))
DANU_Dossiers_Full$code <- parse_number(DANU_Dossiers_Full$code)

DANU_Dossiers_Full %>% filter(code==18|code==189) %>%
  select(patid, weight, code,frequency) %>% distinct() %>%
  mutate(frequency=ifelse(frequency>3,2,1)) %>% group_by(frequency) %>% summarise(n=sum(weight))

DANU_Dossiers_Full <- DANU_Dossiers_Full %>% select(patid, weight, code) %>% distinct()

unique(DANU_Dossiers_Full$code)

DANU_Dossiers_Full %>% 
  mutate(rank = ifelse(code==186,1,
                       ifelse(code==185,2,
                              ifelse(code==184,3,
                                     ifelse(code==1832,4,
                                            ifelse(code==1831,5,
                                                   ifelse(code==1830,6,
                                                          ifelse(code==183,7,
                                                                 ifelse(code==182,8,
                                                                        ifelse(code==181,9,
                                                                               ifelse(code==18,10,10))))))))))) %>%
  group_by(patid, weight) %>% filter(rank==min(rank)) %>%
  ungroup() %>% group_by(rank) %>% summarise(n=sum(weight))





Pats_to_track <- fread("Pats_to_track.txt", sep="\t")

DANU_Dossiers_Full <- fread("DANU Dossiers Full.txt")
DANU_Dossiers_Full <- Pats_to_track %>% inner_join(DANU_Dossiers_Full)
DANU_Dossiers_Full <- DANU_Dossiers_Full %>% filter(grepl("N18", code))
DANU_Dossiers_Full$code <- parse_number(DANU_Dossiers_Full$code)
DANU_Dossiers_Full <- DANU_Dossiers_Full %>% group_by(patid) %>%
  mutate(latest==as.Date(latest)) %>% filter(latest==max(latest)) %>% select(patid, weight, code) %>% distinct() %>% ungroup()

DANU_Dossiers_Full %>% 
  mutate(rank = ifelse(code==186,1,
                       ifelse(code==185,2,
                              ifelse(code==184,3,
                                     ifelse(code==1832,4,
                                            ifelse(code==1831,5,
                                                   ifelse(code==1830,6,
                                                          ifelse(code==183,7,
                                                                 ifelse(code==182,8,
                                                                        ifelse(code==181,9,
                                                                               ifelse(code==18,10,10))))))))))) %>%
  group_by(patid, weight) %>% filter(rank==min(rank)) %>%
  ungroup() %>% group_by(rank) %>% summarise(n=sum(weight))


# ---------
# Drug Usage All - Ever & Last 12 months & last month -----------------
Pats_to_track <- fread("Pats_to_track.txt", sep="\t")
names(Pats_to_track)[1] <- "patient"

CKD_Drug_Histories <- fread("CKD Drug Histories.txt")
names(CKD_Drug_Histories)[2] <- "patient"

CKD_Drug_Histories <- Pats_to_track %>% left_join(CKD_Drug_Histories)

sum(as.numeric(CKD_Drug_Histories$weight))  # 13384925

CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month60, factor_key=TRUE)

CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-")
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="133")
CKD_Drug_Histories <- separate_rows(CKD_Drug_Histories, Drugs, sep = ",", convert=T)

CKD_Ingredients <- fread("CKD Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, drug_class, drug_group)
unique(CKD_Ingredients$drug_class)
unique(CKD_Ingredients$drug_group)
CKD_Ingredients$molecule <- as.numeric(CKD_Ingredients$molecule)
names(CKD_Ingredients)[1] <- "Drugs"

CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(CKD_Ingredients) 

data.frame(CKD_Drug_Histories %>% 
             mutate(drug_class=ifelse(drug_group=="Injectable Antihypertensive", "Injectables", drug_class)) %>%
  select(patient, weight, drug_class) %>% distinct() %>%
  group_by(drug_class) %>% summarise(n=sum(as.numeric(weight)))) %>%
                 mutate(drug_class=str_replace(drug_class, " ", "_"))

# ------------------------------------------

# Predict stages for missing pats unk --------------------------

Pats_to_track <- fread("Pats_to_track.txt", sep="\t")
DANU_Dossiers_Full <- fread("DANU Dossiers Full.txt")
DANU_Dossiers_Full <- Pats_to_track %>% inner_join(DANU_Dossiers_Full)
DANU_Dossiers_Full <- DANU_Dossiers_Full %>% filter(grepl("N18", code))
DANU_Dossiers_Full$code <- parse_number(DANU_Dossiers_Full$code)


DANU_Dossiers_Full <- DANU_Dossiers_Full %>% select(patid, weight, code) %>% distinct()

unique(DANU_Dossiers_Full$code)

DANU_Dossiers_Full %>% 
  mutate(rank = ifelse(code==186,1,
                       ifelse(code==185,2,
                              ifelse(code==184,3,
                                     ifelse(code==1832,4,
                                            ifelse(code==1831,5,
                                                   ifelse(code==1830,6,
                                                          ifelse(code==183,7,
                                                                 ifelse(code==182,8,
                                                                        ifelse(code==181,9,
                                                                               ifelse(code==18,10,10))))))))))) %>%
  group_by(patid, weight) %>% filter(rank==min(rank)) %>%
  ungroup() %>% group_by(rank) %>% summarise(n=sum(weight))


#     rank        n
#    <dbl>    <dbl>
#  1     1 1309809.
#  2     2  264334.
#  3     3  399065.
#  4     4   99188.
#  5     5  316351.
#  6     6  462256.
#  7     7 5716123.
#  8     8 1657740.
#  9     9  432582.
#  
#  
# 10    10 2727477.



# EndStage	1309809	12%
# 5	         264334	2%
# 4        	 399065	4%
# 3b        	99188	1%
# 3a	       316351	3%
# 3	         462256	4%
# 3	        5716123	54%
# 2	        1657740	16%
# 1	         432582	4%

#          10657448	
# 		
# Unk	      2727477	

Stages <- 
DANU_Dossiers_Full %>% 
  mutate(rank = ifelse(code==186,1,
                       ifelse(code==185,2,
                              ifelse(code==184,3,
                                     ifelse(code==1832,4,
                                            ifelse(code==1831,5,
                                                   ifelse(code==1830,6,
                                                          ifelse(code==183,7,
                                                                 ifelse(code==182,8,
                                                                        ifelse(code==181,9,
                                                                               ifelse(code==18,10,10))))))))))) %>%
  group_by(patid, weight) %>% filter(rank==min(rank)) %>% ungroup()

rm(DANU_Dossiers_Full, Pats_to_track)
sum(Stages$weight)
names(Stages)[1] <- "patient"




CKD_Drug_Histories <- fread("CKD Drug Histories.txt", colClasses = "character")
CKD_Drug_Histories <- Stages %>% select(patient) %>% inner_join(CKD_Drug_Histories)
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-"&Drugs!="133")
CKD_Drug_Histories <- separate_rows(CKD_Drug_Histories, Drugs, sep = ",", convert=T)

CKD_Ingredients <- fread("CKD Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, generic_name)
CKD_Ingredients$molecule <- as.numeric(CKD_Ingredients$molecule)
names(CKD_Ingredients)[1] <- "Drugs"

CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(CKD_Ingredients)
CKD_Drug_Histories$generic_name <- str_replace_all(CKD_Drug_Histories$generic_name, " ", "_")

CKD_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 13384925

CKD_Drug_Histories <- CKD_Drug_Histories %>%  select(patient, weight, generic_name) %>% distinct() %>%
    mutate(Treat=1) %>% select(-weight) %>% spread(key=generic_name, value=Treat)

CKD_Drug_Histories[is.na(CKD_Drug_Histories)] <- 0



# CKD_Comorbidity_Inventories <- fread("CKD Comorbidity Inventories.txt", colClasses = "character")
# names(CKD_Comorbidity_Inventories)[1] <- "patient"
# 
# CKD_Comorbidity_Inventories <- Stages %>% select(patient) %>% left_join(CKD_Comorbidity_Inventories) %>% select(patient, diagnosis)
# CKD_Comorbidity_Inventories <- CKD_Comorbidity_Inventories %>% filter(grepl("C", diagnosis)|grepl("D", diagnosis)|grepl("E", diagnosis)|grepl("F", diagnosis)|grepl("G", diagnosis)|
#                                         grepl("H", diagnosis)|grepl("I", diagnosis)|grepl("J", diagnosis)|grepl("K", diagnosis)|grepl("L", diagnosis)|
#                                         grepl("M", diagnosis)|grepl("N", diagnosis)|grepl("R", diagnosis))
# 
# CKD_Comorbidity_Inventories <- CKD_Comorbidity_Inventories %>% mutate(Treat=1) %>% spread(key=diagnosis, value=Treat)
# 
# CKD_Comorbidity_Inventories[is.na(CKD_Comorbidity_Inventories)] <- 0

temp_short <- CKD_Drug_Histories # %>% left_join(CKD_Comorbidity_Inventories)

temp_short <- Stages %>% select(-c(code, weight)) %>% left_join(temp_short)

temp_short_to_fit <- temp_short %>% filter(rank!=10)
temp_short_to_fit_noPat <- temp_short_to_fit[,2:106]


# modelAll_1_gbm <- gbm(rank ~ . , data = temp_short_to_fit_noPat)
# 
# toPlot <- data.frame(predict(modelAll_1_gbm, temp_short_to_fit_noPat))
# names(toPlot) <- "toPlot"
# 
# toPlot %>% ggplot(aes(toPlot)) + geom_density()
# 
# temp_short_to_predict <- temp_short %>% filter(rank==10)
# temp_short_to_predict <- temp_short_to_predict[,2:1112]
# temp_short_to_predict <- temp_short_to_predict %>% select(-c(G28, G49, M64))
# 
# ignore <- data.frame(predict(modelAll_1_gbm, temp_short_to_predict))  %>%
#   bind_cols(temp_short %>% filter(rank==10)) %>% rename("prediction"="predict.modelAll_1_gbm..temp_short_to_predict.")
# 
# Stages %>% filter(rank==10) %>% summarise(n=sum(weight))



#temp_short_to_fit_noPat$rank <- as.factor(temp_short_to_fit_noPat$rank)
temp_short_to_fit_noPat <- temp_short_to_fit_noPat %>% group_by(rank) %>% sample_n(690)
temp_short_to_fit_noPat <-  temp_short_to_fit_noPat %>% rename("ARNI"="Sacubitril/Valsartan")

modelAll_1_randomForest <- randomForest(rank ~ . , data = temp_short_to_fit_noPat)
summary(modelAll_1_randomForest)
data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)


temp_short_to_predict <- temp_short %>% filter(rank==10)
temp_short_to_predict <-  temp_short_to_predict %>% rename("ARNI"="Sacubitril/Valsartan")


newstagespreds <- data.frame(predict(modelAll_1_randomForest, temp_short_to_predict)) %>%
  rename("prediction"="predict.modelAll_1_randomForest..temp_short_to_predict.") %>%
  bind_cols(temp_short %>% filter(rank==10)) %>%
  arrange(prediction) %>%  select(patient, prediction)

fwrite(newstagespreds, "newstagespreds.txt", sep="\t")

# -----------------------
# Put a rank to all patients ----------------
Stages %>% group_by(rank) %>% summarise(n=sum(weight))

# EndStage	1309809	12%
# 5	         264334	2%
# 4        	 399065	4%
# 3b        	99188	1%
# 3a	       316351	3%
# 3	         462256	4%
# 3	        5716123	54%
# 2	        1657740	16%
# 1	         432582	4%


newstagespreds <- fread("newstagespreds.txt")
nrow(newstagespreds) # 19210

0.12*19210
0.14*19210
0.18*19210
0.19*19210
0.22*19210
0.26*19210
0.40*19210
0.56*19210

newstagespreds <- newstagespreds %>%
arrange(prediction) %>%
  mutate(RowN = row_number()) %>%
  mutate(rank=ifelse(RowN<=2305, 1, 
                                ifelse(RowN<=2689, 2, 
                                       ifelse(RowN<=3458,3, 
                                              ifelse(RowN<=3650,4,
                                                     ifelse(RowN<=4226,5,
                                                            ifelse(RowN<=4995,6,
                                                                   ifelse(RowN<=7684,7,
                                                                          ifelse(RowN<=10758,8,9)))))))))  %>%
  select(patient, rank)

Stages_Complete <- Stages %>% filter(rank!=10) %>% select(patient, rank) %>%
  full_join(newstagespreds)

CKD_Drug_Histories <- fread("CKD Drug Histories.txt", colClasses = "character")
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, weight)

Stages_Complete %>% left_join(CKD_Drug_Histories) %>% summarise(n=sum(as.numeric(weight))) # 13384925

Stages_Complete %>% left_join(CKD_Drug_Histories) %>% group_by(rank) %>% summarise(n=100*sum(as.numeric(weight))/13384925)



CKD_Drug_Histories <- fread("CKD Drug Histories.txt", colClasses = "character")
CKD_Drug_Histories <- Stages_Complete %>% select(patient) %>% inner_join(CKD_Drug_Histories)
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-"&Drugs!="133")
CKD_Drug_Histories <- separate_rows(CKD_Drug_Histories, Drugs, sep = ",", convert=T)

CKD_Ingredients <- fread("CKD Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, generic_name)
CKD_Ingredients$molecule <- as.numeric(CKD_Ingredients$molecule)
names(CKD_Ingredients)[1] <- "Drugs"

CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, weight, Drugs) %>% distinct() %>% left_join(CKD_Ingredients) 
CKD_Drug_Histories$generic_name <- str_replace_all(CKD_Drug_Histories$generic_name, " ", "_")

CKD_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(as.numeric(weight))) # 13384925

CKD_Drug_Histories <- CKD_Drug_Histories %>%  select(patient, weight, generic_name) %>% distinct() %>%
    mutate(Treat=1) %>%  spread(key=generic_name, value=Treat)

CKD_Drug_Histories[is.na(CKD_Drug_Histories)] <- 0

CKD_Drug_Histories <- Stages_Complete %>% left_join(CKD_Drug_Histories)
sum(is.na(CKD_Drug_Histories))

names(CKD_Drug_Histories)

for(i in names(CKD_Drug_Histories)[3:106]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  print(CKD_Drug_Histories %>% group_by(rank, get(i)) %>% summarise(n=sum(as.numeric(weight))))
}

CKD_Drug_Histories %>% group_by(rank, Kidney_Transplant) %>% summarise(n=sum(as.numeric(weight)))

Stages_Complete %>% mutate(Stage=ifelse(rank==1, "ESRD", 
                                        ifelse(rank==2, "Stage5",
                                               ifelse(rank==3, "Stage4" ,
                                                      ifelse(rank==4|rank==5|rank==6|rank==7,"Stage3",
                                                             ifelse(rank==8,"Stage2", "Stage1")))))) %>%
  group_by(Stage) %>% count() %>% mutate(perc=n/sum(11465+11512+14807 +49875+3566+2229))

Stages_Complete <- Stages_Complete %>% mutate(Stage=ifelse(rank==1, "ESRD", 
                                        ifelse(rank==2, "Stage5",
                                               ifelse(rank==3, "Stage4" ,
                                                      ifelse(rank==4|rank==5|rank==6|rank==7,"Stage3",
                                                             ifelse(rank==8,"Stage2", "Stage1"))))))

fwrite(Stages_Complete, "CKD_Stages_Complete_FilledIn.txt", sep="\t")

# ------------------------
# Physician Specialty Class Initiation --------------------

CKD_Doses <- fread("CKD Doses.txt", colClasses = "character")
CKD_Doses <- CKD_Doses %>% filter(paid=="P") %>% select(drug_class, pat_id, from_dt, specialty) %>% mutate(from_dt=as.Date(from_dt))
names(CKD_Doses)[2] <- "patid"

CKD_Pts_Cmbdt_flags <- fread("CKD_Pts_Cmbdt_flags_&_Cmbdt_Hierchy_grps.csv", colClasses = "character")
CKD_Pts_Cmbdt_flags <- CKD_Pts_Cmbdt_flags %>% select(patid, weight, pts_cmb_grps)
CKD_Pts_Cmbdt_flags %>% group_by(pts_cmb_grps) %>% summarise(n=sum(as.numeric(weight)))


CKD_Demographics <- fread("CKD Demographics.txt", colClasses = "character")
names(CKD_Demographics)[1] <- "patid"
CKD_Demographics <- CKD_Demographics %>% mutate(ckd_onset=as.Date(ckd_onset)) %>% filter(ckd_onset >= "2016-05-01")
CKD_Demographics <- CKD_Demographics %>% select(patid, weight, ckd_onset)

CKD_Doses <- CKD_Demographics %>% inner_join(CKD_Pts_Cmbdt_flags) %>% inner_join(CKD_Doses)  %>% filter(from_dt>=ckd_onset)

unique(CKD_Doses$drug_class)
First_RAAS <- CKD_Doses %>% filter(drug_class=="ARB"|drug_class=="ACE") %>% group_by(patid) %>% filter(from_dt==min(from_dt))
First_MRA <- CKD_Doses %>% filter(drug_class=="MRA") %>% group_by(patid) %>% filter(from_dt==min(from_dt))
First_BB <- CKD_Doses %>% filter(drug_class=="Beta Blocker") %>% group_by(patid) %>% filter(from_dt==min(from_dt))
First_GLP1 <- CKD_Doses %>% filter(drug_class=="GLP1") %>% group_by(patid) %>% filter(from_dt==min(from_dt))
First_SGLT2 <- CKD_Doses %>% filter(drug_class=="SGLT2") %>% group_by(patid) %>% filter(from_dt==min(from_dt))

names(First_RAAS)[7] <- "code"
names(First_MRA)[7] <- "code"
names(First_BB)[7] <- "code"
names(First_GLP1)[7] <- "code"
names(First_SGLT2)[7] <- "code"

DANU_Specialty_Codes <- fread("DANU Specialty Codes.txt", colClasses = "character")
DANU_Specialty_Codes <- DANU_Specialty_Codes %>% select(code, specialty)

data.frame(First_RAAS %>% ungroup()  %>% left_join(DANU_Specialty_Codes)) %>%
  group_by(pts_cmb_grps, specialty) %>% summarise(n=sum(as.numeric(weight))) %>%
    drop_na() %>% filter(specialty!="Unknown"&specialty!="Facility"&specialty!="Other Physician"&specialty!="Other Provider"&specialty!="Nutrition Specialist") %>%
  ungroup() %>% spread(key=specialty, value=n)

data.frame(First_MRA %>% ungroup()  %>% left_join(DANU_Specialty_Codes)) %>%
  group_by(pts_cmb_grps, specialty) %>% summarise(n=sum(as.numeric(weight)))  %>% 
    drop_na() %>% filter(specialty!="Unknown"&specialty!="Facility"&specialty!="Other Physician"&specialty!="Other Provider"&specialty!="Nutrition Specialist") %>%
  ungroup() %>% spread(key=specialty, value=n)

data.frame(First_BB %>% ungroup()  %>% left_join(DANU_Specialty_Codes)) %>%
  group_by(pts_cmb_grps, specialty) %>% summarise(n=sum(as.numeric(weight)))  %>%
    drop_na() %>% filter(specialty!="Unknown"&specialty!="Facility"&specialty!="Other Physician"&specialty!="Other Provider"&specialty!="Nutrition Specialist") %>%
  ungroup() %>% spread(key=specialty, value=n)

data.frame(First_GLP1 %>% ungroup()  %>% left_join(DANU_Specialty_Codes)) %>%
  group_by(pts_cmb_grps, specialty) %>% summarise(n=sum(as.numeric(weight)))  %>%
    drop_na() %>% filter(specialty!="Unknown"&specialty!="Facility"&specialty!="Other Physician"&specialty!="Other Provider"&specialty!="Nutrition Specialist") %>%
  ungroup() %>% spread(key=specialty, value=n)


data.frame(First_SGLT2 %>% ungroup()  %>% left_join(DANU_Specialty_Codes)) %>%
  group_by(pts_cmb_grps, specialty) %>% summarise(n=sum(as.numeric(weight)))  %>%
    drop_na() %>% filter(specialty!="Unknown"&specialty!="Facility"&specialty!="Other Physician"&specialty!="Other Provider"&specialty!="Nutrition Specialist") %>%
  ungroup() %>% spread(key=specialty, value=n)


# ----------------------
# Physician all scripts  --------------------

CKD_Doses <- fread("CKD Doses.txt", colClasses = "character")
CKD_Doses <- CKD_Doses %>% filter(paid=="P") %>% select(drug_class, specialty) 
names(CKD_Doses)[2] <- "code"

DANU_Specialty_Codes <- fread("DANU Specialty Codes.txt", colClasses = "character")
DANU_Specialty_Codes <- DANU_Specialty_Codes %>% select(code, specialty)

CKD_Doses <- CKD_Doses %>% inner_join(DANU_Specialty_Codes)

CKD_Doses <- CKD_Doses %>% filter(specialty!="Unknown"&specialty!="Facility"&specialty!="Other Physician"&specialty!="Other Provider"&specialty!="Nutrition Specialist") 

unique(CKD_Doses$specialty)
                         
CKD_Doses %>% group_by(specialty, drug_class) %>% count() %>% ungroup() %>%
  spread(key=drug_class, value=n)

# ---------------------------
# Lab tests --------------------

CKD_Pts_Cmbdt_flags <- fread("CKD_Pts_Cmbdt_flags_&_Cmbdt_Hierchy_grps.csv", colClasses = "character")
CKD_Pts_Cmbdt_flags <- CKD_Pts_Cmbdt_flags %>% select(patid, weight, pts_cmb_grps)
CKD_Pts_Cmbdt_flags %>% group_by(pts_cmb_grps) %>% summarise(n=sum(as.numeric(weight)))

# 1 ASCVD        5238099.
# 2 Non-ASCVD    1481330.
# 3 T2D ASCVD    1795567.
# 4 T2D No ASCVD  539086.

CKD_Stages_Complete_FilledIn <- fread("CKD_Stages_Complete_FilledIn.txt", colClasses = "character")
names(CKD_Stages_Complete_FilledIn)[1] <- "patid"

DANU_Measures <- fread("DANU Measures.txt", colClasses = "character")
DANU_Measures <- DANU_Measures %>% select(patid, weight, test, claimed, value)
unique(DANU_Measures$test)
DANU_Measures <- DANU_Measures %>% filter(test=="ACR Ratio")


CKD_Demographics <- fread("CKD Demographics.txt")
CKD_Demographics <- CKD_Demographics %>% select(patid, ckd_onset)


temp <- DANU_Measures %>% inner_join(CKD_Demographics %>% mutate(ckd_onset=as.character(ckd_onset))) %>% 
  mutate(claimed=as.Date(claimed)) %>% mutate(ckd_onset=as.Date(ckd_onset)) %>%
   mutate(Elapsed= as.numeric(as.numeric(claimed)-as.numeric(ckd_onset))/30.5 ) %>%
   ungroup() %>% inner_join(CKD_Pts_Cmbdt_flags) %>% select(Elapsed, value, pts_cmb_grps) 

temp %>% # filter(Elapsed>=0) %>%
  mutate(value=as.numeric(value)) %>%
  # mutate(Elapsed=round(Elapsed,1)) %>%
  ggplot(aes(Elapsed, value)) +
  geom_smooth(fill="firebrick", colour="black") +
  xlab("\n No. Elapsed Months Since 1st Observed CKD Diagnosis") +
  ylab("Urinary ACR ratio \n [Normal <30 mg/g] \n") +
  theme_minimal() 


# Everyone, over time

DANU_Measures %>%
  mutate(claimed=as.Date(claimed)) %>% 
    mutate(value=as.numeric(value)) %>%
  group_by(patid) %>% mutate(MIN=min(claimed)) %>%
  mutate(Elapsed= as.numeric(as.numeric(claimed)-as.numeric(MIN))/30.5 ) %>% 
  ungroup() %>% select(Elapsed, value) %>%
  ggplot(aes(Elapsed, value)) +
  geom_smooth(fill="firebrick", colour="black") +
  xlab("\n No. Elapsed Months Since 1st ACR ratio sampling") +
  ylab("Urinary ACR ratio \n [Normal <30 mg/g] \n") +
  theme_minimal() 


# Per stage, patient normalized

DANU_Measures %>%
  inner_join(CKD_Stages_Complete_FilledIn %>% select(-rank)) %>%
  mutate(Stage=as.factor(Stage)) %>%
  mutate(Stage=fct_relevel(Stage,c("Stage1","Stage2","Stage3","Stage4","Stage5","ESRD"))) %>%
  mutate(claimed=as.Date(claimed)) %>% 
  mutate(value=as.numeric(value)) %>%
  group_by(patid) %>% mutate(MIN_date=min(claimed)) %>%
  mutate(Elapsed= as.numeric(as.numeric(claimed)-as.numeric(MIN_date))/30.5 ) %>% 
  group_by(patid) %>% mutate(MIN_value=min(value)) %>%
  mutate(value_perc= as.numeric(as.numeric(value)-as.numeric(MIN_value))/MIN_value ) %>% 
  ungroup() %>% select(Elapsed, value_perc, Stage) %>%
  ggplot(aes(Elapsed, value_perc, colour=Stage, fill=Stage)) +
  geom_smooth(se = F) +
  xlab("\n No. Elapsed Months Since 1st ACR ratio sampling") +
  ylab("% Change Over baseline \n Urinary ACR ratio (mg/g) \n") +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Stage, ncol=3,  scales = "free_y")

DANU_Measures %>% mutate(value=as.numeric(value)) %>%
 group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% ungroup() %>%
    inner_join(CKD_Stages_Complete_FilledIn) %>% group_by(Stage) %>%
  summarise(n=median(value))


DANU_Measures %>%
  inner_join(CKD_Stages_Complete_FilledIn %>% select(-rank)) %>%
  mutate(Stage=as.factor(Stage)) %>%
  mutate(Stage=fct_relevel(Stage,c("Stage1","Stage2","Stage3","Stage4","Stage5","ESRD"))) %>%
  mutate(claimed=as.Date(claimed)) %>% 
  mutate(value=as.numeric(value)) %>%
  group_by(patid) %>% mutate(MIN_date=min(claimed)) %>%
  mutate(Elapsed= as.numeric(as.numeric(claimed)-as.numeric(MIN_date))/30.5 ) %>%
  group_by(patid) %>% mutate(MIN_value=min(value)) %>%
  mutate(value_perc= as.numeric(as.numeric(value)-as.numeric(MIN_value))/MIN_value ) %>%
  ungroup() %>% select(claimed, value, Stage) %>%
  ggplot(aes(claimed, value, colour=Stage, fill=Stage)) +
  geom_smooth(se = F, size=2, alpha=0.7) +
  xlab("\n Exact Date") +
  ylab("Urinary ACR ratio \n [Normal <30 mg/g] \n") +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()


# Per comorbidty group, patient normalized

DANU_Measures %>%
  inner_join(CKD_Pts_Cmbdt_flags %>% select(-weight)) %>%
  mutate(pts_cmb_grps=as.factor(pts_cmb_grps)) %>%
  mutate(pts_cmb_grps=fct_relevel(pts_cmb_grps,c("Non-ASCVD","ASCVD","T2D No ASCVD","T2D ASCVD"))) %>%
  mutate(claimed=as.Date(claimed)) %>% 
  mutate(value=as.numeric(value)) %>%
  group_by(patid) %>% mutate(MIN_date=min(claimed)) %>%
  mutate(Elapsed= as.numeric(as.numeric(claimed)-as.numeric(MIN_date))/30.5 ) %>% 
  group_by(patid) %>% mutate(MIN_value=min(value)) %>%
  mutate(value_perc= as.numeric(as.numeric(value)-as.numeric(MIN_value))/MIN_value ) %>% 
  ungroup() %>% select(Elapsed, value_perc, pts_cmb_grps) %>%
  ggplot(aes(Elapsed, value_perc, colour=pts_cmb_grps, fill=pts_cmb_grps)) +
  geom_smooth(se = F) +
  xlab("\n No. Elapsed Months Since 1st Urinary ACR ratio sampling") +
  ylab("% Change Over baseline \n Urinary ACR ratio (mg/g) \n") +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~pts_cmb_grps, ncol=2,  scales = "free_y")

DANU_Measures %>% mutate(value=as.numeric(value)) %>%
 group_by(patid) %>% filter(value==max(value)) %>% slice(1) %>% ungroup() %>%
    inner_join(CKD_Pts_Cmbdt_flags) %>% group_by(pts_cmb_grps) %>%
  summarise(n=weighted.mean(value, as.numeric(weight)))


DANU_Measures %>%
  inner_join(CKD_Pts_Cmbdt_flags %>% select(-weight)) %>%
  mutate(pts_cmb_grps=as.factor(pts_cmb_grps)) %>%
  mutate(pts_cmb_grps=fct_relevel(pts_cmb_grps,c("Non-ASCVD","ASCVD","T2D No ASCVD","T2D ASCVD"))) %>%
  mutate(claimed=as.Date(claimed)) %>% 
  mutate(value=as.numeric(value)) %>%
  group_by(patid) %>% mutate(MIN_date=min(claimed)) %>%
  mutate(Elapsed= as.numeric(as.numeric(claimed)-as.numeric(MIN_date))/30.5 ) %>%
  group_by(patid) %>% mutate(MIN_value=min(value)) %>%
  mutate(value_perc= as.numeric(as.numeric(value)-as.numeric(MIN_value))/MIN_value ) %>%
  ungroup() %>% select(claimed, value, pts_cmb_grps) %>%
  ggplot(aes(claimed, value, colour=pts_cmb_grps, fill=pts_cmb_grps)) +
  geom_smooth(se = F, size=2, alpha=0.7) +
  xlab("\n Exact Date") +
  ylab("Urinary ACR ratio (mg/g) \n [Normal <30 mg/g] \n") +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()



# ---------------------------
# Lab results before/after class initiation --------------------

Months_lookup <- fread("CKD Analysis Results 8.0/Months_lookup.txt")

CKD_Stages_Complete_FilledIn <- fread("CKD Analysis Results 8.0/CKD_Stages_Complete_FilledIn.txt", colClasses = "character")
CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% select(-rank)

CKD_Drug_Histories <- fread("CKD Analysis Results 8.0/CKD Drug Histories.txt")
CKD_Drug_Histories <- CKD_Stages_Complete_FilledIn %>% left_join(CKD_Drug_Histories)
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)

CKD_Ingredients <- fread("CKD Analysis Results 8.0/CKD Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, drug_class, drug_group)
unique(CKD_Ingredients$drug_class)
CKD_Ingredients$molecule <- as.numeric(CKD_Ingredients$molecule)
names(CKD_Ingredients)[1] <- "Drugs"
unique(CKD_Ingredients$drug_class)

string_Dialysis <- paste0("\\b(",paste0(CKD_Ingredients$Drugs[CKD_Ingredients$drug_class=="Dialysis"], collapse = "|"),")\\b")

Dialysis_Pats <- CKD_Drug_Histories %>% filter(grepl(string_Dialysis, Drugs)) %>% select(patient) %>% distinct()
 
CKD_Drug_Histories <- Dialysis_Pats %>% left_join(CKD_Drug_Histories) %>% select(-c(disease)) %>%
  group_by(patient)  %>% filter(grepl(string_Dialysis, Drugs)) %>% 
  filter(Month==min(Month)) %>% select(patient, Stage, weight, Month) %>%
  rename("FirstDialysis"="Month")


DANU_Measures <- fread("DANU Measures 5.0/DANU Measures.txt")
unique(DANU_Measures$test)
DANU_Measures <- DANU_Measures %>% filter(test=="ACR Ratio") %>% select(patid, weight, claimed, value) 
names(DANU_Measures)[1] <- "patient"

DANU_Measures <- Dialysis_Pats %>% inner_join(DANU_Measures)

DANU_Measures <- DANU_Measures %>% mutate(claimed=as.character(claimed))
DANU_Measures <- DANU_Measures %>% mutate(claimed=str_sub(claimed, 1L, 7L))
DANU_Measures <- DANU_Measures %>% left_join(Months_lookup, by=c("claimed"="Month")) %>% select(-claimed)

DANU_Measures <- CKD_Drug_Histories %>% full_join(DANU_Measures) %>%
  arrange(patient, Stage, weight, FirstDialysis, Exact_Month, value)

DANU_Measures <- DANU_Measures %>% drop_na() %>% mutate(Elapsed=Exact_Month-FirstDialysis) 


DANU_Measures <- DANU_Measures %>% ungroup() %>% filter(Elapsed<0 & Elapsed > (-6) ) %>% select(patient) %>% distinct() %>%
  inner_join(DANU_Measures %>% ungroup() %>% filter(Elapsed>0 & Elapsed < 6) %>% select(patient) %>% distinct()) %>%
  inner_join(DANU_Measures)



DANU_Measures <- DANU_Measures %>% left_join(DANU_Measures %>% 
                              group_by(patient) %>% summarise(value=mean(value)) %>%  
                              select(patient, value) %>% distinct() %>% rename("MEAN"="value")) %>%
  mutate(value= (value-MEAN)/MEAN) %>% ungroup()




DANU_Measures %>%
 filter(Elapsed>(-7) & Elapsed<7) %>%
  ggplot(aes(Elapsed, value)) +
  geom_smooth(method="loess", fill="firebrick", colour="firebrick", se =F, size=3, alpha=0.6) +
  xlab("\n No. Elapsed Months Since 1st Dialysis") +
  ylab("Urinary ACR ratio \n [mg/g] \n") +
  theme_minimal() 




# ---------------
# Lab results average per class exp --------------------

CKD_Stages_Complete_FilledIn <- fread("CKD_Stages_Complete_FilledIn.txt", colClasses = "character")
CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% select(-rank)

CKD_Drug_Histories <- fread("CKD Drug Histories.txt")
CKD_Drug_Histories <- CKD_Stages_Complete_FilledIn %>% left_join(CKD_Drug_Histories)
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, Stage, weight, Drugs) %>% distinct()

CKD_Ingredients <- fread("CKD Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, drug_class)
unique(CKD_Ingredients$drug_class)
CKD_Ingredients$molecule <- as.numeric(CKD_Ingredients$molecule)
names(CKD_Ingredients)[1] <- "Drugs"
unique(CKD_Ingredients$drug_class)

CKD_Drug_Histories <- CKD_Drug_Histories %>% filter(Drugs!="-")
CKD_Drug_Histories <- separate_rows(CKD_Drug_Histories, Drugs, sep = ",", convert=T)
CKD_Drug_Histories <- CKD_Drug_Histories %>% left_join(CKD_Ingredients)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(-Drugs) %>% distinct()

CKD_Drug_Histories <- CKD_Drug_Histories %>% mutate(Exp=1) %>% ungroup() %>% spread(key=drug_class, value=Exp)
CKD_Drug_Histories[is.na(CKD_Drug_Histories)] <- 0


DANU_Measures <- fread("DANU Measures.txt")
unique(DANU_Measures$test)
DANU_Measures <- DANU_Measures %>% filter(test=="ACR Ratio") %>% select(patid, weight, value) %>% distinct()
names(DANU_Measures)[1] <- "patient"
DANU_Measures <- CKD_Drug_Histories %>% select(patient) %>% inner_join(DANU_Measures)
DANU_Measures <- DANU_Measures %>% group_by(patient) %>% filter(value==max(value)) %>% slice(1)

CKD_Drug_Histories <-DANU_Measures %>% inner_join(CKD_Drug_Histories)

CKD_Drug_Histories %>% group_by(ARNI) %>% summarise(n=weighted.mean(value, weight))

for (i in names(CKD_Drug_Histories[,5:21])){
    print(i)
    print(CKD_Drug_Histories %>% group_by(get(i)) %>% summarise(n=weighted.mean(value, weight)))
}


# ------------------------
# Class penetrance before 1st ARNI/MRA start ---------------------------------------

CKD_Drug_Histories <- fread("CKD Drug Histories.txt")
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)

CKD_Ingredients <- fread("CKD Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, drug_class, drug_group)
unique(CKD_Ingredients$drug_class)
CKD_Ingredients$molecule <- as.numeric(CKD_Ingredients$molecule)
names(CKD_Ingredients)[1] <- "Drugs"
unique(CKD_Ingredients$drug_class)
string_ARNI <- paste0("\\b(",paste0(CKD_Ingredients$Drugs[CKD_Ingredients$drug_class=="ARNI"], collapse = "|"),")\\b")
string_MRA <- paste0("\\b(",paste0(CKD_Ingredients$Drugs[CKD_Ingredients$drug_class=="MRA"], collapse = "|"),")\\b")

MRA_Pats <- CKD_Drug_Histories %>% filter(Drugs != "-") %>% filter(grepl(string_MRA, Drugs)) %>% select(patient) %>% distinct()
 
FirstMRA <- MRA_Pats %>% left_join(CKD_Drug_Histories) %>% select(-c(disease)) %>%
  group_by(patient)  %>% filter(grepl(string_MRA, Drugs)) %>% 
  filter(Month==min(Month)) %>% select(patient, weight, Month) %>%
  rename("FirstMRA"="Month")

FirstMRA <- FirstMRA %>% left_join(CKD_Drug_Histories)
FirstMRA <- separate_rows(FirstMRA, Drugs, sep = ",", convert=T)


FirstMRA <- FirstMRA %>% filter(Drugs!="-") %>% mutate(Drugs=as.numeric(Drugs)) %>% left_join(CKD_Ingredients %>% select(-drug_group)) %>% select(-disease)

FirstMRA %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight)))  # 367867

FirstMRA <- FirstMRA %>% mutate(Lapsed=Month-FirstMRA) %>% filter((Lapsed>=(-12)) & (Lapsed<=(12)))

FirstMRA <- FirstMRA %>% select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(FirstMRA)

FirstMRA <- FirstMRA %>% filter(drug_class!="Death") %>%  select(patient, Month) %>% distinct() %>% group_by(patient) %>% count() %>% filter(n>=25) %>%
  select(patient) %>% left_join(FirstMRA)


FirstMRA %>% select(patient, weight) %>% distinct() %>% ungroup() %>% summarise(n=sum(as.numeric(weight))) # 515899

FirstMRA %>%  select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight)/515899)) %>% ungroup() %>%
  spread(key=Lapsed, value=n)

unique(FirstARNI$drug_class)



"ACE", "ARB" , "ARNI", "MRA", "Alpha Blocker",  "CCB", "Beta Blocker", "Diuretic",
"Biguanide" ,  "SGLT2", "GLP1", 
"Antihypertensive", "Hospital Inpatient", "Dialysis", "Kidney Transplant" , "Surgery Inpatient"

"#B3F1FF",  "#69E2FF", "#FFBFBF", "#AE1641", "#09D0FF", "#0099BD", "#001E60", "#5F69B1", "#FFE4A7",  "#FFEFCA", "#F1AC02", 
"#D1F0D1" , "#E8F8E8" , "#96DE96", "#6BCF6B", "#B482DA"



FirstMRA %>% filter(drug_class!="Death") %>% select(patient, weight, drug_class, Lapsed) %>% distinct() %>%
  group_by(Lapsed, drug_class) %>% summarise(n=sum(as.numeric(weight))) %>% ungroup() %>%
  mutate(drug_class=ifelse(is.na(drug_class),"Lapsed",drug_class)) %>%
  mutate(n=n/515899) %>%
   mutate(drug_class=str_replace(drug_class, " ", "_"))  %>%
  mutate(drug_class=factor(drug_class, levels=c("ACE", "ARB" , "ARNI", "MRA", "Alpha_Blocker",  "CCB", "Beta_Blocker", "Diuretic",
                                                "Biguanide" ,  "SGLT2", "GLP1", "Antihypertensive", "Hospital_Inpatient", "Dialysis", "Kidney_Transplant" , "Surgery_Inpatient"))) %>%
   ggplot(aes(Lapsed,n*100, colour=drug_class)) +
  geom_line(size=2, alpha=.8) +
  ylim(0,110) +
  xlab("\n No. Elapsed Months \n(Before/After 1st MRA)") +
  ylab("Population % \n") +
  theme_classic() +
  scale_colour_manual(values=c("#B3F1FF",  "#69E2FF", "#FFBFBF", "#AE1641", "#09D0FF", "#0099BD", "#001E60", "#5F69B1", "#FFE4A7",  
                                        "#FFEFCA", "#F1AC02",  "#D1F0D1" , "#E8F8E8" , "#96DE96", "#6BCF6B", "#B482DA"))
                                        

# ---------------------------------------
# Stratification comorbs per ACR cat -----------------

CKD_Pts_Cmbdt_flags <- fread("CKD Analysis Results 8.0/CKD_Pts_Cmbdt_flags_&_Cmbdt_Hierchy_grps.csv", colClasses = "character")
CKD_Pts_Cmbdt_flags <- CKD_Pts_Cmbdt_flags %>% select(patid, weight, pts_cmb_grps)
CKD_Pts_Cmbdt_flags %>% group_by(pts_cmb_grps) %>% summarise(n=sum(as.numeric(weight)))

#   pts_cmb_grps        n
#   <chr>           <dbl>
# 1 ASCVD        5238099.
# 2 Non-ASCVD    1481330.
# 3 T2D ASCVD    1795567.
# 4 T2D No ASCVD  539086.


DANU_Measures <- fread("DANU Measures 5.0/DANU Measures.txt", colClasses = "character")
DANU_Measures <- DANU_Measures %>% select(patid, weight, test,  claimed, value)
DANU_Measures <- DANU_Measures %>% filter(test=="ACR Ratio") %>% mutate(value=as.numeric(value))
DANU_Measures <- CKD_Pts_Cmbdt_flags %>% inner_join(DANU_Measures)
DANU_Measures <- DANU_Measures %>% select(-c(claimed,test)) %>% distinct() %>% group_by(patid) %>% filter(value==max(value)) %>% slice(1) 

DANU_Measures %>% ungroup() %>% group_by(pts_cmb_grps) %>% summarise(n=median(value))

# 1 ASCVD         402.
# 2 Non-ASCVD     273.
# 3 T2D ASCVD     342.
# 4 T2D No ASCVD  259.

# 1 ASCVD         34  
# 2 Non-ASCVD     21  
# 3 T2D ASCVD     46.3
# 4 T2D No ASCVD  31.9



DANU_Measures %>% ungroup() %>%
  mutate(ACR_group=ifelse(value>=300, ">300", ifelse(value>=150, ">150", ifelse(value>=60, ">60", ifelse(value>=30, ">30", ifelse(value>=20, ">20", ifelse(value>=10, ">10", ifelse(value>=5, ">5", "<5")))))))) %>%
  group_by(pts_cmb_grps, ACR_group) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=ACR_group, value=n)

DANU_Measures %>% ungroup() %>%
  rename("Group"="pts_cmb_grps") %>%
  ggplot(aes(value, colour=Group, fill=Group)) +
  geom_density() +
  xlim(0,1000) +
  facet_wrap(~Group) +
  ggsci::scale_colour_jco() +
  ggsci::scale_fill_jco() +
  theme_minimal() +
  xlab("\n Max ACR observed per patient") + ylab("Cohort density \n")

CKD_Stages_Complete_FilledIn <- fread("CKD Analysis Results 8.0/CKD_Stages_Complete_FilledIn.txt", colClasses = "character")
CKD_Stages_Complete_FilledIn <- CKD_Stages_Complete_FilledIn %>% select(-rank)

data.frame(
  CKD_Stages_Complete_FilledIn %>% inner_join(DANU_Measures, by=c("patient"="patid")) %>%
  group_by(pts_cmb_grps, Stage) %>% mutate(value=ifelse(value>=30, ">30", "<30")) %>%
  group_by(pts_cmb_grps, Stage, value) %>% summarise(n=sum(as.numeric(weight)))
  ) %>% filter(Stage=="Stage1"|Stage=="Stage2") %>%
  ungroup() %>% spread(key=value, value=n) %>%
  mutate(Perc=`>30`/(`>30`+`<30`))


  
# ----------------

# CKD GLP1 SGLT2   Combos ---------------------------
CKD_Pts_Cmbdt_flags <- fread("CKD_Pts_Cmbdt_flags_&_Cmbdt_Hierchy_grps.csv", integer64 = "character", stringsAsFactors = F)
sum(CKD_Pts_Cmbdt_flags$weight)
CKD_Pts_Cmbdt_flags <- CKD_Pts_Cmbdt_flags %>% select(patid, weight) %>% distinct() %>% rename("patient"="patid")

CKD_Ingredients       <- fread("CKD Ingredients.txt", integer64 = "character", stringsAsFactors = F)
CKD_Ingredients$drug_id <- unlist(lapply(CKD_Ingredients$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))
string_SGLT2 <- paste0("\\b(",paste0(CKD_Ingredients$drug_id[CKD_Ingredients$drug_class == "SGLT2"], collapse = "|"),")\\b")
string_GLP1 <- paste0("\\b(",paste0(CKD_Ingredients$drug_id[CKD_Ingredients$drug_class == "GLP1"], collapse = "|"),")\\b")


CKD_Drug_Histories <- fread("CKD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(-c(disease)) %>% inner_join(CKD_Pts_Cmbdt_flags) 
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, Treat, weight) %>% distinct() %>% filter(Treat!="-")
CKD_Drug_Histories <- CKD_Drug_Histories %>% mutate(GLP1_SGLT2_Status = ifelse(grepl(string_GLP1, Treat)&grepl(string_SGLT2, Treat), "Combo", 
                                                                               ifelse(grepl(string_GLP1, Treat), "GLP1",
                                                                                            ifelse(grepl(string_SGLT2, Treat), "SGLT2", "none"))))

CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, GLP1_SGLT2_Status, weight) %>% distinct() 

Combos <- CKD_Drug_Histories %>% filter(GLP1_SGLT2_Status=="Combo") %>% select(patient, weight) %>% distinct()
GLP1 <- CKD_Drug_Histories %>% filter(GLP1_SGLT2_Status=="GLP1") %>% select(patient, weight) %>% distinct()
SGLT2 <- CKD_Drug_Histories %>% filter(GLP1_SGLT2_Status=="SGLT2") %>% select(patient, weight) %>% distinct()


GLP1_Pats <- GLP1 %>% anti_join(Combos) %>% anti_join(SGLT2)
SGLT2_Pats <- SGLT2 %>% anti_join(Combos) %>% anti_join(GLP1)
Combo_Pats <- Combos

sum(GLP1_Pats$weight) #  418624
sum(SGLT2_Pats$weight) # 290115
sum(Combo_Pats$weight) # 171531


CKD_Drug_Histories <- fread("CKD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(-c(disease)) %>% inner_join(CKD_Pts_Cmbdt_flags) 
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, Treat, weight) %>% distinct() %>% filter(Treat!="-")
CKD_Drug_Histories %>% filter(grepl(string_GLP1, Treat)) %>% select(patient, weight) %>% distinct() %>%
  inner_join(CKD_Drug_Histories %>% filter(grepl(string_SGLT2, Treat)) %>% select(patient, weight) %>% distinct()) %>% summarise(n=sum(weight)) # 236566



CKD_Drug_Histories <- fread("CKD Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(-c(disease)) %>% inner_join(CKD_Pts_Cmbdt_flags) 
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CKD_Drug_Histories <- CKD_Drug_Histories %>% select(patient, Treat, weight) %>% distinct() %>% filter(Treat!="-")

GLP1_Pats %>% left_join(CKD_Drug_Histories) %>% filter(grepl(string_GLP1, Treat)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(n2=mean(n))
SGLT2_Pats %>% left_join(CKD_Drug_Histories) %>% filter(grepl(string_SGLT2, Treat)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(n2=mean(n))
Combo_Pats %>% left_join(CKD_Drug_Histories) %>% filter(grepl(string_GLP1, Treat)&grepl(string_SGLT2, Treat)) %>% group_by(patient, weight) %>% count() %>% ungroup() %>% summarise(n2=mean(n))

# -----------------
# CKD and Diabetes Size - labs - costs --------
# OLD CKD Data

DANU_Demographics_Full <- fread("DANU Demographics 8.0/DANU Demographics Full.txt")
DANU_Demographics_Full <- DANU_Demographics_Full %>% select(patid, weight, diagnosis) %>% distinct()

CKD_Stages_Complete_FilledIn <- fread("CKD Analysis Results 8.0/CKD_Stages_Complete_FilledIn.txt", colClasses = "character")
names(CKD_Stages_Complete_FilledIn)[1] <- "patid"

CKD_Stages_Complete_FilledIn %>% left_join(DANU_Demographics_Full) %>% summarise(pop=sum(weight)) # 13384925


CKD_Stages_Complete_FilledIn %>%  left_join(DANU_Demographics_Full) %>%
  left_join(DANU_Demographics_Full %>% filter(grepl("Diab", diagnosis)) %>% select(patid) %>% mutate(T2D="T2D")) %>%
  summarise(pop=sum(weight)) # 7666429  # 0.5727659


groups <- CKD_Stages_Complete_FilledIn %>%  left_join(DANU_Demographics_Full) %>%
  left_join(DANU_Demographics_Full %>% filter(grepl("Diab", diagnosis)) %>% select(patid) %>% mutate(T2D="T2D")) %>%
  select(patid, Stage, weight, T2D)


groups %>% group_by(Stage, T2D) %>% summarise(n=sum(weight)) %>%
  spread(key=T2D, value=n) %>% mutate(perc=(T2D/(T2D+`<NA>`)))



DANU_Measures <- fread("DANU Measures 5.0/DANU Measures.txt", colClasses = "character")
DANU_Measures <- DANU_Measures %>% select(patid, weight, test, claimed, value)
unique(DANU_Measures$test)

MAX <- DANU_Measures %>% inner_join(groups %>% select(patid)) %>%
  group_by(patid, test) %>% summarise(value=max(as.numeric(value))) %>% ungroup()


groups %>% mutate(T2D=ifelse(is.na(T2D),"no", T2D)) %>%
  inner_join(MAX) %>% group_by(test, T2D) %>% summarise(mean=weighted.mean(as.numeric(value), weight, na.rm=T)) %>%
  spread(key=T2D, value=mean)


#  1 ACR Ratio          187.    390.   
#  2 Albumin Level        4.33    4.31 
#  3 ALT Level           48.8    52.9  
#  4 AST Level           52.9    56.4  
#  5 BMI                 31.1    34.3  
#  6 Creatinine Urine   139.    152.   
#  7 Fasting Glucose    110.    155.   
#  8 Fibrosis Activity    0.790   0.890
#  9 Fibrosis Score       0.454   0.469
# 10 Fibrosis Stage       1.69    1.98 
# 11 Gamma Globulin       1.16    1.18 
# 12 HbA1c Level          5.84    7.74 
# 13 Microalbumin Urine   9.28   16.2  
# 14 NASH Indicator       1       0.848
# 15 NASH Score           0.372   0.331
# 16 Platelet Count     287.    295. 


groups %>% mutate(T2D=ifelse(is.na(T2D),"no", T2D)) %>%
  inner_join(DANU_Measures %>% filter(test=="ACR Ratio") %>% select(-weight)) %>%
  ggplot(aes(as.Date(claimed), as.numeric(value))) +
  geom_smooth()



CKD_Drug_Histories <- fread("CKD Analysis Results 8.0/CKD Drug Histories.txt")
CKD_Drug_Histories <- groups %>% select(patid) %>% left_join(CKD_Drug_Histories, by=c("patid"="patient"))
CKD_Drug_Histories <- gather(CKD_Drug_Histories, Month, Drugs, month1:month60, factor_key=TRUE)
CKD_Drug_Histories$Month <- as.character(CKD_Drug_Histories$Month)
CKD_Drug_Histories$Month <- parse_number(CKD_Drug_Histories$Month)

CKD_Ingredients <- fread("CKD Analysis Results 8.0/CKD Ingredients.txt",  colClasses = "character", stringsAsFactors = F)
CKD_Ingredients <- CKD_Ingredients %>%  separate(drug_id, c('group', 'molecule'))
CKD_Ingredients <- CKD_Ingredients %>% select(molecule, drug_class, drug_group)
unique(CKD_Ingredients$drug_class)
CKD_Ingredients$molecule <- as.numeric(CKD_Ingredients$molecule)
names(CKD_Ingredients)[1] <- "Drugs"
unique(CKD_Ingredients$drug_group)


string_GLP1 <- paste0("\\b(",paste0(CKD_Ingredients$Drugs[CKD_Ingredients$drug_class=="GLP1"], collapse = "|"),")\\b")

GLP1_pats <- CKD_Drug_Histories %>% filter(Drugs!="-") %>% select(-Month) %>% distinct() %>%
  filter(grepl(string_GLP1, Drugs)) %>% select(patid) %>% distinct() %>% mutate(GLP1="GLP1")


groups %>% mutate(T2D=ifelse(is.na(T2D),"no", T2D)) %>%
  inner_join(DANU_Measures %>% filter(test=="ACR Ratio") %>% select(-weight)) %>%
  left_join(GLP1_pats) %>% mutate(GLP1=ifelse(is.na(GLP1),"no", GLP1)) %>%
  ggplot(aes(as.Date(claimed), as.numeric(value), colour=GLP1, fill=GLP1)) +
  geom_smooth()



CKD_Costs <- fread("CKD_Costs.txt")

# -------