
library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(openxlsx)
# library(survminer)
# library(survival)
# library(ggsurvfit)
# library(ggrepel)
#library(lessR)

options(scipen = 999)

# Biomarkers ----------------------------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")

setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[
  Primary_Cancer %in% c("Breast Cancer", "Lung Cancer", "Intestinal Cancer", "Prostate Cancer"),
  .(patid, Primary_Cancer)
]

biomarkers <- fread("Source/biomarkers.txt", sep=",")

setDT(biomarkers)

biomarkers[, c("ENCID", "SOURCEID", "chunck_id") := NULL]

biomarkers <- biomarkers[grepl("^[A-Za-z]", BIOMARKER)]

names(biomarkers)[1] <- "patid"

lookup <- unique(biomarkers[, .(BIOMARKER, VARIATION_DETAIL, BIOMARKER_STATUS)])[order(BIOMARKER)]

biomarkers <- biomarkers[BIOMARKER_STATUS != "equivocal"]

biomarkers[, BIOMARKER_STATUS := ifelse(BIOMARKER_STATUS == "amplified", "positive",
                                        ifelse(BIOMARKER_STATUS == "non-amplified", "negative", BIOMARKER_STATUS))]                                                            ifelse(BIOMARKER_STATUS=="non-amplified", "negative", BIOMARKER_STATUS)))


New_Primary_Cancer_Box[, .N, by = Primary_Cancer]
   
temp <- New_Primary_Cancer_Box %>% inner_join(biomarkers %>% select(patid, BIOMARKER) %>% distinct()) %>%
  group_by(Primary_Cancer, BIOMARKER) %>% count() %>% spread(key=BIOMARKER, value=n) 

fwrite(temp, "temp.csv")

temp_pos <- New_Primary_Cancer_Box %>% inner_join(biomarkers %>% filter(BIOMARKER_STATUS=="positive") %>% select(patid, BIOMARKER) %>% distinct()) %>%
  group_by(Primary_Cancer, BIOMARKER) %>% count() %>% spread(key=BIOMARKER, value=n) 

fwrite(temp_pos, "temp_pos.csv")



New_Primary_Cancer_Box %>% inner_join(biomarkers %>% select(patid, BIOMARKER,BIOMARKER_STATUS) %>% distinct()) 

target <- New_Primary_Cancer_Box %>% inner_join(biomarkers %>% select(patid, BIOMARKER,BIOMARKER_STATUS) %>% distinct()) %>%
  filter(Primary_Cancer=="Lung Cancer" & BIOMARKER=="EGFR") %>% select(patid, BIOMARKER_STATUS) %>% distinct()

length(unique(target$patid))

target <- target %>% mutate(exp=1) %>% spread(key=BIOMARKER_STATUS, value=exp)
target[is.na(target)] <-0
target <- target %>% mutate(EGFR=ifelse(positive==1,1,0)) %>% select(patid, EGFR)


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics%>% select(patid, died) 

target %>% left_join(PONS_Demographics) %>% group_by(EGFR, died) %>% count()


# -------------
# Drug usage by Breast Cancer Metastasis and genetic Background status ------------------

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")

length(unique(New_Primary_Cancer_Box$patid)) # 153529

setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer %in% c("Breast Cancer"), .(patid, Primary_Cancer)]

biomarkers <- fread("Source/biomarkers.txt", sep=",")

setDT(biomarkers)

biomarkers[, c("ENCID", "SOURCEID", "chunck_id") := NULL]

biomarkers <- biomarkers[grepl("^[A-Za-z]", BIOMARKER)]

names(biomarkers)[1] <- "patid"

lookup <- unique(biomarkers[, .(BIOMARKER, VARIATION_DETAIL, BIOMARKER_STATUS)])[order(BIOMARKER)]

unique(lookup$BIOMARKER)

biomarkers <- biomarkers[BIOMARKER_STATUS != "equivocal"]

biomarkers[, BIOMARKER_STATUS := ifelse(BIOMARKER_STATUS == "amplified", "positive",
                                        ifelse(BIOMARKER_STATUS == "non-amplified", "negative", BIOMARKER_STATUS))]

biomarkers <- biomarkers[BIOMARKER %in% c("PR", "ER", "ER/PR", "ER/PR/HER2/NEU", "HER2/NEU"), ]

Breast <- New_Primary_Cancer_Box[biomarkers, on = "patid", nomatch = 0]

distinct(
  Breast[, .(BIOMARKER, BIOMARKER_STATUS)]
  )[order(BIOMARKER)]

# "ER",  POSITIVE GOOD
# "PR",  POSITIVE GOOD 
# "HER2/NEU",   POSITIVE BAD !
# "ER/PR",  POSITIVE GOOD
# "ER/PR/HER2/NEU"   POSITIVE GOOD/BAD, requires combination therapy

# MAX ?
# Breast <- Breast[, .SD[which.max(NOTE_DATE)], by = .(patid, Primary_Cancer, BIOMARKER)]

Breast <- unique(
  Breast[, .(patid, Primary_Cancer, BIOMARKER, BIOMARKER_STATUS)]
  )[, exp := 1][ , dcast(.SD, patid + Primary_Cancer + BIOMARKER ~ BIOMARKER_STATUS, value.var = "exp")]

Breast[is.na(Breast)] <-0

length(unique(Breast$patid)) # 29315

Breast <- Breast[, Positive := ifelse(positive == 1, 1, 0)][, .(patid, Primary_Cancer, BIOMARKER, Positive)]

PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, diagnosis)]

setDT(PONS_Demographics)

PONS_Demographics <- unique(PONS_Demographics[Breast[, .(patid)], on = "patid", nomatch = 0])

PONS_Demographics <- Breast[PONS_Demographics, on = "patid", nomatch = 0]

PONS_Demographics[, mets := ifelse(grepl(",", diagnosis), 1, 0)]

unique(PONS_Demographics$BIOMARKER)

PONS_Demographics %>% select(patid) %>% distinct() # 29315
PONS_Demographics %>% select(patid, BIOMARKER) %>% distinct() %>% group_by(BIOMARKER) %>% count() # 29315


length(unique(PONS_Demographics$patid))


ignore <- PONS_Demographics %>% select(patid, BIOMARKER, Positive) %>% distinct() %>% 
  mutate(BIOMARKER=ifelse(BIOMARKER %in% c("ER", "PR", "ER/PR"), "HR", BIOMARKER)) %>% distinct()

length(unique(ignore$patid))
length(unique(ignore$BIOMARKER))

ignore <- ignore %>% select(patid, BIOMARKER) %>% distinct() %>% filter(BIOMARKER=="HR") %>% select(patid) %>%
  inner_join(ignore %>% select(patid, BIOMARKER) %>% distinct() %>% filter(BIOMARKER=="HER2/NEU") %>% select(patid)) %>%
    full_join(ignore %>% select(patid, BIOMARKER) %>% distinct() %>% filter(BIOMARKER=="ER/PR/HER2/NEU") %>% select(patid)) %>% distinct() %>%
  left_join(ignore)

ignore %>% select(patid, BIOMARKER) %>% distinct() %>%
  left_join(ignore %>% filter(Positive==1) %>% select(patid, BIOMARKER) %>% distinct() %>% mutate(Pos=1)) %>%
  spread(key=BIOMARKER, value=Pos) %>%
  group_by(`ER/PR/HER2/NEU`, `HER2/NEU` , `HR`) %>% count() %>% mutate(n=n/21044)



ignore %>% select(patid, BIOMARKER) %>% distinct() %>%
  left_join(ignore %>% filter(Positive==1) %>% select(patid, BIOMARKER) %>% distinct() %>% mutate(Pos=1)) %>%
  spread(key=BIOMARKER, value=Pos) %>% 
  mutate(HR=ifelse(is.na(HR),0,HR)) %>% 
  mutate(`HER2/NEU`=ifelse(is.na(`HER2/NEU`),0,`HER2/NEU`)) %>% 
  mutate(`ER/PR/HER2/NEU`=ifelse(is.na(`ER/PR/HER2/NEU`),0,`ER/PR/HER2/NEU`)) %>%
  mutate(group=ifelse(`ER/PR/HER2/NEU`==1 & `HER2/NEU`==1 & HR==1, "HRPosHER2Pos",
                      ifelse( `ER/PR/HER2/NEU`==1 & `HER2/NEU`==1, "HRPosHER2Pos",
                              ifelse( `ER/PR/HER2/NEU`==1 & HR==1, "HRPosHER2Pos",
                                      ifelse( `ER/PR/HER2/NEU`==1 , "HRPosHER2Pos",
                                              ifelse( `HER2/NEU`==1 & HR==1, "HRPosHER2Pos",
                                                      ifelse(`HER2/NEU`==1, "HRNegHER2Pos",
                                                             ifelse(HR==1, "HRPosHER2Neg", "HRNegHER2Neg" )))))))) %>%
  group_by(group) %>% count() %>% mutate(n=n/21044)


groups <- ignore %>% select(patid, BIOMARKER) %>% distinct() %>%
  left_join(ignore %>% filter(Positive==1) %>% select(patid, BIOMARKER) %>% distinct() %>% mutate(Pos=1)) %>%
  spread(key=BIOMARKER, value=Pos) %>% 
  mutate(HR=ifelse(is.na(HR),0,HR)) %>% 
  mutate(`HER2/NEU`=ifelse(is.na(`HER2/NEU`),0,`HER2/NEU`)) %>% 
  mutate(`ER/PR/HER2/NEU`=ifelse(is.na(`ER/PR/HER2/NEU`),0,`ER/PR/HER2/NEU`)) %>%
  mutate(group=ifelse(`ER/PR/HER2/NEU`==1 & `HER2/NEU`==1 & HR==1, "HRPosHER2Pos",
                      ifelse( `ER/PR/HER2/NEU`==1 & `HER2/NEU`==1, "HRPosHER2Pos",
                              ifelse( `ER/PR/HER2/NEU`==1 & HR==1, "HRPosHER2Pos",
                                      ifelse( `ER/PR/HER2/NEU`==1 , "HRPosHER2Pos",
                                              ifelse( `HER2/NEU`==1 & HR==1, "HRPosHER2Pos",
                                                      ifelse(`HER2/NEU`==1, "HRNegHER2Pos",
                                                             ifelse(HR==1, "HRPosHER2Neg", "HRNegHER2Neg" )))))))) 
   
  
fwrite(groups, "Source/Groups_HR_HER2_status.txt")


# CLASSES

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat)])
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
setDT(unique(CAN_Drug_Histories))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer", .(molecule, chemo_class)]
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, chemo_class) %>% distinct() %>% mutate(exp=1) %>% spread(key=chemo_class, value=exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-`<NA>`) 
names(CAN_Drug_Histories)[1] <- "patid" 
names(CAN_Drug_Histories)



All <- PONS_Demographics[, .(tot = .N), by = .(mets, BIOMARKER, Positive)]
All[order(BIOMARKER, mets, Positive)]

Hormonal <- PONS_Demographics[CAN_Drug_Histories, on = "patid", nomatch = 0]

Hormonal <- Hormonal[Hormonal_Therapy == 1, .(count = .N), 
                     by = .(mets, BIOMARKER, Positive, Hormonal_Therapy)]

merge(All, Hormonal, 
      by = c("mets", "BIOMARKER", "Positive"), all.x = TRUE
      )[, perc := round(100 * count / tot, 2)][order(BIOMARKER, mets, Positive)]


Biologic <- PONS_Demographics[CAN_Drug_Histories, on = "patid", nomatch = 0]

Biologic <- Biologic[Biologic == 1, .(count = .N),  by = .(mets, BIOMARKER, Positive, Biologic)]

merge(All, Biologic, 
      by = c("mets", "BIOMARKER", "Positive"), all.x = TRUE
      )[, perc := round(100 * count / tot, 2)][order(BIOMARKER, mets, Positive)]


Classes <- PONS_Demographics[CAN_Drug_Histories, on = "patid", nomatch = 0]

target_columns <- colnames(Classes)[7:21]
  
result <- data.table(mets = character(), BIOMARKER = character(), Positive = numeric())

for (col in target_columns) {
  
  temp_result <- Classes[, .(count = sum(get(col))), by = .(mets, BIOMARKER, Positive)]

  temp_result[, col := col]
  
  result <- rbindlist(list(result, temp_result), use.names = TRUE, fill = TRUE)
}

result_wide <- dcast(result, mets + BIOMARKER + Positive ~ col, value.var = "count", fill = 0)

result_wide[, mets := as.numeric(mets)]

result_final <- merge(All, result_wide, by = c("mets", "BIOMARKER", "Positive"), all.x = TRUE)

fwrite(result_final , "Results/result_final.csv")




# MOLECULES

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat)])
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
setDT(unique(CAN_Drug_Histories))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer", .(molecule, generic_name, chemo_class)]
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% 
  group_by(patient, Treat, generic_name, chemo_class) %>% 
  mutate(chemo_class = paste(chemo_class, generic_name, sep = "_")) %>% 
  ungroup() %>% select(-generic_name)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, chemo_class) %>% distinct() %>% mutate(exp=1) %>% spread(key=chemo_class, value=exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")
names(CAN_Drug_Histories)[1] <- "patid" 
names(CAN_Drug_Histories)



All <- PONS_Demographics[, .(tot = .N), by = .(mets, BIOMARKER, Positive)]
All[order(BIOMARKER, mets, Positive)]

setDT(CAN_Drug_Histories)

Biologic_Trastuzumab <- PONS_Demographics[CAN_Drug_Histories, on = "patid", nomatch = 0]

Biologic_Trastuzumab <- Biologic_Trastuzumab[Biologic_Trastuzumab == 1, .(count = .N),  by = .(mets, BIOMARKER, Positive, Biologic_Trastuzumab)]

merge(All, Biologic_Trastuzumab, 
      by = c("mets", "BIOMARKER", "Positive"), all.x = TRUE
      )[, perc := round(100 * count / tot, 2)][order(BIOMARKER, mets, Positive)]


Classes <- PONS_Demographics[CAN_Drug_Histories, on = "patid", nomatch = 0]

target_columns <- colnames(Classes)[7:192]
  
result <- data.table(mets = character(), BIOMARKER = character(), Positive = numeric())

for (col in target_columns) {
  
  temp_result <- Classes[, .(count = sum(get(col))), by = .(mets, BIOMARKER, Positive)]

  temp_result[, col := col]
  
  result <- rbindlist(list(result, temp_result), use.names = TRUE, fill = TRUE)
}

result_wide <- dcast(result, mets + BIOMARKER + Positive ~ col, value.var = "count", fill = 0)

result_wide[, mets := as.numeric(mets)]

result_final <- merge(All, result_wide, by = c("mets", "BIOMARKER", "Positive"), all.x = TRUE)

fwrite(result_final , "Results/result_final2.csv")






# ---------
# Repeat drug class / molecules penetrance using 4 groups allocation ---------------

groups <- fread("Source/Groups_HR_HER2_status.txt")
setDT(groups)

groups %>% group_by(group) %>% count()

PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, diagnosis)]

setDT(PONS_Demographics)

PONS_Demographics <- unique(PONS_Demographics[groups[, .(patid)], on = "patid", nomatch = 0])

PONS_Demographics <- groups[PONS_Demographics, on = "patid", nomatch = 0]

PONS_Demographics[, mets := ifelse(grepl(",", diagnosis), 1, 0)]

PONS_Demographics <-PONS_Demographics %>% select(patid, mets)

groups <- groups %>% left_join(PONS_Demographics) %>% select(patid, group, mets) %>% distinct()

unique(groups$mets)

# CLASSES

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[groups[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat)])
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
setDT(unique(CAN_Drug_Histories))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer", .(molecule, chemo_class)]
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, chemo_class) %>% distinct() %>% mutate(exp=1) %>% spread(key=chemo_class, value=exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-`<NA>`) 
names(CAN_Drug_Histories)[1] <- "patid" 
names(CAN_Drug_Histories)
setDT(CAN_Drug_Histories)

All <- groups[, .(tot = .N), by = .(mets, group)]
All[order(group, mets)]

setDT(groups)

Hormonal <- groups[CAN_Drug_Histories, on = "patid", nomatch = 0]

Hormonal <- Hormonal[Hormonal_Therapy == 1, .(count = .N),  by = .(mets, group, Hormonal_Therapy)]

merge(All, Hormonal, 
      by = c("mets", "group"), all.x = TRUE
      )[, perc := round(100 * count / tot, 2)][order(group, mets)]


Biologic <- groups[CAN_Drug_Histories, on = "patid", nomatch = 0]

Biologic <- Biologic[Biologic == 1, .(count = .N),  by = .(mets, group, Biologic)]

merge(All, Biologic, 
      by = c("mets", "group"), all.x = TRUE
      )[, perc := round(100 * count / tot, 2)][order(group, mets)]


Classes <- groups[CAN_Drug_Histories, on = "patid", nomatch = 0]

target_columns <- colnames(Classes)[4:18]
  
result <- data.table(mets = character(), group = character())

for (col in target_columns) {
  
  temp_result <- Classes[, .(count = sum(get(col))), by = .(mets, group)]

  temp_result[, col := col]
  
  result <- rbindlist(list(result, temp_result), use.names = TRUE, fill = TRUE)
}

result_wide <- dcast(result, mets + group ~ col, value.var = "count", fill = 0)

result_wide[, mets := as.numeric(mets)]

result_final <- merge(All, result_wide, by = c("mets", "group"), all.x = TRUE)

fwrite(result_final , "Results/result_final_4groups.csv")





# MOLECULES

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[groups[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat)])
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
setDT(unique(CAN_Drug_Histories))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer", .(molecule, generic_name, chemo_class)]
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% 
  group_by(patient, Treat, generic_name, chemo_class) %>% 
  mutate(chemo_class = paste(chemo_class, generic_name, sep = "_")) %>% 
  ungroup() %>% select(-generic_name)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, chemo_class) %>% distinct() %>% mutate(exp=1) %>% spread(key=chemo_class, value=exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")
names(CAN_Drug_Histories)[1] <- "patid" 
names(CAN_Drug_Histories)
setDT(CAN_Drug_Histories)



All <- groups[, .(tot = .N), by = .(mets, group)]
All[order(group, mets)]

setDT(groups)

Biologic_Trastuzumab <- groups[CAN_Drug_Histories, on = "patid", nomatch = 0]

Biologic_Trastuzumab <- Biologic_Trastuzumab[Biologic_Trastuzumab == 1, .(count = .N),  by = .(mets, group, Biologic_Trastuzumab)]

merge(All, Biologic_Trastuzumab, 
      by = c("mets", "group"), all.x = TRUE
      )[, perc := round(100 * count / tot, 2)][order(group, mets)]


Classes <- groups[CAN_Drug_Histories, on = "patid", nomatch = 0]

target_columns <- colnames(Classes)[4:181]
  
result <- data.table(mets = character(), group = character())

for (col in target_columns) {
  
  temp_result <- Classes[, .(count = sum(get(col))), by = .(mets, group)]

  temp_result[, col := col]
  
  result <- rbindlist(list(result, temp_result), use.names = TRUE, fill = TRUE)
}

result_wide <- dcast(result, mets + group  ~ col, value.var = "count", fill = 0)

result_wide[, mets := as.numeric(mets)]

result_final <- merge(All, result_wide, by = c("mets", "group"), all.x = TRUE)

fwrite(result_final , "Results/result_final2_4groups.csv")

# ---------------


# How many months treated BEFORE metastasis VS how many months survived thereafter ? --------------- 

groups <- fread("Source/Groups_HR_HER2_status.txt")

setDT(groups)

PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, diagnosis)]

setDT(PONS_Demographics)

PONS_Demographics <- unique(PONS_Demographics[groups[, .(patid)], on = "patid", nomatch = 0])

PONS_Demographics <- groups[PONS_Demographics, on = "patid", nomatch = 0]

PONS_Demographics[, mets := ifelse(grepl(",", diagnosis), 1, 0)]

PONS_Demographics <- PONS_Demographics %>% select(patid, mets)

groups <- groups[PONS_Demographics, on = "patid", nomatch = 0L][, .(patid, group, mets)]

PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, cancer_onset , cancer_metastasis, death_date )]

PONS_Demographics <- PONS_Demographics[!is.na(cancer_metastasis) & !is.na(cancer_onset)]

PONS_Demographics <- groups[PONS_Demographics, on = "patid", nomatch = 0][,.(patid, cancer_onset, cancer_metastasis, death_date)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics[, death_date := as.character(death_date)][, death_date := substr(death_date, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_onset" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_onset")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_onset")}]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]

PONS_Demographics <- merge(PONS_Demographics, Months_lookup, by.x = "death_date", by.y = "Month", all.x = TRUE)

PONS_Demographics <- PONS_Demographics[,.SD, .SDcols = !c("death_date")][, {setnames(.SD, old = "Exact_Month", new = "death_date")}]

PONS_Demographics <- PONS_Demographics[order(PONS_Demographics, patid)]





CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")

CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

setDT(CAN_Drug_Histories)

CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat, Month)])

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer" , .(molecule, chemo_class)]

PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_cancer <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule, collapse = "|"),")\\b")

setDT(unique(CAN_Drug_Histories))

CAN_Drug_Histories <- CAN_Drug_Histories[grepl(string_cancer, Treat), ]

CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Demographics <- groups[PONS_Demographics, on = "patid"][mets == 1]

names(CAN_Drug_Histories)[1] <- "patid"

CAN_Drug_Histories <- CAN_Drug_Histories[PONS_Demographics, on = "patid"][Month < cancer_metastasis]

CAN_Drug_Histories <- CAN_Drug_Histories[, .(count = .N), by = patid]

setnames(CAN_Drug_Histories, old = "count", new = "MonthsTreatBefMets")

PONS_Demographics <- merge(PONS_Demographics, CAN_Drug_Histories, by = "patid", all.x = TRUE)

PONS_Demographics[, MonthsTreatBefMets := ifelse(is.na(MonthsTreatBefMets), 0, MonthsTreatBefMets)]

PONS_Demographics[, death_date := ifelse(is.na(death_date), 100, death_date)]

PONS_Demographics[, ElapsMetsDead := death_date - cancer_metastasis]

cor(PONS_Demographics$MonthsTreatBefMets[PONS_Demographics$death_date<100], 
    PONS_Demographics$ElapsMetsDead[PONS_Demographics$death_date<100])



for (grp in unique(PONS_Demographics$group)) {
  
  subset_data <- PONS_Demographics[group == grp & death_date < 100]
  
  correlation_result <- cor(subset_data$MonthsTreatBefMets, subset_data$ElapsMetsDead)
  
  cat(
    paste(
      paste(grp, correlation_result), 
      "\n"
      )
    )
}


cor(subset_data$MonthsTreatBefMets, as.numeric(subset_data$death_date))
cor(subset_data$MonthsTreatBefMets, as.numeric(subset_data$ElapsMetsDead))

PONS_Demographics[, .(count = .N), by = MonthsTreatBefMets]

PONS_Demographics %>%  mutate(death_date=ifelse(death_date==100,0,1)) %>% group_by(death_date) %>%
  summarise(mean=mean(MonthsTreatBefMets))

ignore <- PONS_Demographics %>%  mutate(death_date=ifelse(death_date==100,0,1)) 

ignore$CancerToMets <- ignore$cancer_metastasis - ignore$cancer_onset

ignore$TreatPerc <- ignore$MonthsTreatBefMets / (ignore$CancerToMets+1)

logistic_model <- glm(death_date ~ log(MonthsTreatBefMets), data=ignore[ignore$MonthsTreatBefMets>0], family=binomial)

lm_model <- lm(ElapsMetsDead ~ MonthsTreatBefMets + CancerToMets + TreatPerc, data=ignore)

summary(lm_model)


ignore  %>% group_by(death_date) %>% summarise(mean=mean(MonthsTreatBefMets))

model <- glm(death_date ~ MonthsTreatBefMets, data=ignore, family=binomial)
newdata <- data.frame(MonthsTreatBefMets=seq(min(ignore$MonthsTreatBefMets), max(ignore$MonthsTreatBefMets),len=10000))
newdata$vs = predict(model, newdata, type="response")
plot(death_date ~ MonthsTreatBefMets, data=ignore, col="steelblue")
lines(vs ~ MonthsTreatBefMets, newdata, lwd=2)

ignore %>%
  filter(MonthsTreatBefMets>0) %>%
  ggplot(aes(x=MonthsTreatBefMets, y=death_date)) + 
  #geom_jitter(alpha=.5, height=0.01, size=0.1, colour="#66A4B9") +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="#66A4B9") +
  ylab("Probability of having died \n during the follow-up") +
  xlab("\n Number of Months ON TREATMENT \n Between Cancer Onset and Metastasis") +
  theme_minimal() 

PONS_Demographics %>%  filter(death_date!=100 & MonthsTreatBefMets>0) %>%
  ggplot(aes(MonthsTreatBefMets, ElapsMetsDead, colour=group, fill=group)) +
  geom_jitter(size=1, alpha=0.4) +
  geom_smooth(se=FALSE, method = "gam") +
  coord_cartesian(xlim = c(0, 40), ylim = c(0,50)) +
  theme_minimal() +
  xlab("\n Number of Months ON TREATMENT \n Between Cancer Onset and Metastasis") +
  ylab("Number of Months \n Between Metastasis and Death \n") +
  scale_colour_manual(values=c("#D3D3D3","#FFEFCA", "#66A4B9", "#C86060")) +
  scale_fill_manual(values=c("#D3D3D3","#FFEFCA", "#66A4B9", "#C86060")) 



PONS_Demographics %>%  filter(death_date!=100 ) %>%
  ggplot(aes(MonthsTreatBefMets, ElapsMetsDead, colour=group, fill=group)) +
  geom_jitter(size=1, alpha=0.4) +
  geom_smooth(se=FALSE, method = "gam") +
  coord_cartesian(xlim = c(0, 40), ylim = c(0,50)) +
  theme_minimal() +
  xlab("\n Number of Months ON TREATMENT \n Between Cancer Onset and Metastasis") +
  ylab("Number of Months \n Between Metastasis and Death \n") +
  scale_colour_manual(values=c("#D3D3D3","#FFEFCA", "#66A4B9", "#C86060")) +
  scale_fill_manual(values=c("#D3D3D3","#FFEFCA", "#66A4B9", "#C86060")) 


ignore

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age)

ignore %>% left_join(PONS_Demographics) %>%
  ggplot(aes(age, CancerToMets)) +
  #geom_point() +
  geom_smooth()

# ----------------------
# Does metastasis site change drug choice ? ----------
groups <- fread("Source/Groups_HR_HER2_status.txt")

setDT(groups)

PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, diagnosis)]

setDT(PONS_Demographics)

PONS_Demographics <- unique(PONS_Demographics[groups[, .(patid)], on = "patid", nomatch = 0])

PONS_Demographics <- groups[PONS_Demographics, on = "patid", nomatch = 0]

PONS_Demographics[, mets := ifelse(grepl(",", diagnosis), 1, 0)]

PONS_Demographics <- PONS_Demographics %>% select(patid, mets)

groups <- groups[PONS_Demographics, on = "patid", nomatch = 0L][, .(patid, group, mets)]

groups <- groups[mets==1, ]

PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, diagnosis)]

PONS_Demographics <- groups[PONS_Demographics, on = "patid", nomatch = 0]

PONS_Demographics <- unique(PONS_Demographics[, .(patid, diagnosis)])

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)

PONS_Demographics <- setDT(PONS_Demographics)[diagnosis!="Breast Cancer", ] 

#PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% count() %>% filter(n==1) %>% select(patid) %>%
#  left_join(PONS_Demographics) %>% ungroup() 

PONS_Demographics <- PONS_Demographics[, .SD[1,], by = patid]

PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")

setDT(PONS_Demographics)

PONS_Demographics[, .N, by = diagnosis][order(-N)]

groups <- groups[PONS_Demographics, on = "patid", nomatch = 0]


PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, cancer_metastasis )]

PONS_Demographics <- PONS_Demographics[!is.na(cancer_metastasis)]

PONS_Demographics <- groups[PONS_Demographics, on = "patid", nomatch = 0][,.(patid, cancer_metastasis)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]


CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")

CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

setDT(CAN_Drug_Histories)

CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat, Month)])

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer" , .(molecule, chemo_class)]

PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

string_cancer <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule, collapse = "|"),")\\b")

setDT(unique(CAN_Drug_Histories))

CAN_Drug_Histories <- CAN_Drug_Histories[grepl(string_cancer, Treat), ]

CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

names(CAN_Drug_Histories)[1] <- "patid"

CAN_Drug_Histories <- CAN_Drug_Histories[PONS_Demographics, on = "patid"][Month > cancer_metastasis]

CAN_Drug_Histories <- distinct(CAN_Drug_Histories[,.(patid, Treat)])

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)

setDT(unique(CAN_Drug_Histories))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer", .(molecule, chemo_class)]
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patid, chemo_class) %>% distinct() %>% drop_na() %>% mutate(exp=1) %>% spread(key=chemo_class, value=exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")

groups <- groups %>% mutate(diagnosis=as.factor(as.character(diagnosis)))

groups <- groups %>% left_join(CAN_Drug_Histories)

groups[is.na(groups)] <- 0

names(groups)

ignore <- groups %>% group_by(diagnosis) %>% count() %>% rename("total"="n") %>%
  left_join(
    gather(groups, Drug, Exp, Alkylating_Agent:Topoisomerase_Inhibitor) %>%
  filter(Exp==1) %>% arrange(patid) %>% group_by(diagnosis, Drug) %>% count()
  ) %>% mutate(n=100*n/total) %>% spread(key=Drug, value=n)


fwrite(ignore, "ignore2.csv")


# -------------------------------------
# Breast Cancer, Metastatic, All, Hormonal Therapy exo, drugs month over month ---------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")

setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[
  Primary_Cancer %in% c("Breast Cancer"),
  .(patid, Primary_Cancer)
]


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, cancer_metastasis) %>% drop_na() %>% select(patid, weight) 

PONS_Demographics <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box) %>% select(patid, weight)

sum(as.numeric(PONS_Demographics$weight)) # 1424476


CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Month, Treat)])

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

Hormone_mets_pats <- CAN_Drug_Histories %>% filter(grepl(string_Hormonal, Treat)) %>% select(patient) %>% distinct()

Hormone_mets_pats %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>% summarise(n=sum(as.numeric(weight))) # 777760.6

CAN_Drug_Histories <- Hormone_mets_pats %>% left_join(CAN_Drug_Histories)

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)

PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer", .(molecule, generic_name)]
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)


temp <- CAN_Drug_Histories %>% mutate(Month=parse_number(as.character(Month))) %>%
  left_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule")) %>% drop_na() %>%
  select(patient, Month, generic_name) %>% distinct() %>%
  left_join(PONS_Demographics %>% rename("patient"="patid")) %>%
  group_by(Month, generic_name) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=Month, value=n)

temp[is.na(temp)] <- 0

fwrite(temp, "Breast_HormonalExp_Mets_drugMonthoverMonth.csv")

# ---------

# Drug usages Palbociclib after metastasis Breast Cancer -------------

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")

setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[
  Primary_Cancer %in% c("Breast Cancer"),
  .(patid, Primary_Cancer)
]


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, cancer_metastasis) %>% drop_na() %>% select(patid, weight, cancer_metastasis) 


Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]



PONS_Demographics <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box) %>% select(patid, weight, cancer_metastasis)

sum(as.numeric(PONS_Demographics$weight)) # 1424476

PONS_Demographics <- PONS_Demographics %>% filter(cancer_metastasis<=36)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

Hormone_mets_pats <- CAN_Drug_Histories %>% filter(grepl(string_Hormonal, Treat)) %>% select(patient) %>% distinct()

Hormone_mets_pats %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>% summarise(n=sum(as.numeric(weight))) 

PONS_Demographics <- Hormone_mets_pats %>% inner_join(PONS_Demographics, by=c("patient"="patid"))

CAN_Drug_Histories <- Hormone_mets_pats %>% left_join(CAN_Drug_Histories)

CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))


CAN_Drug_Histories <- PONS_Demographics %>% left_join(CAN_Drug_Histories) %>% 
  mutate(Month=Month-cancer_metastasis) %>%
  filter(Month>=0)

CAN_Drug_Histories %>% group_by(patient) %>% count() %>% arrange(n)

PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer", .(molecule, generic_name, chemo_class)]
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

CAN_Drug_Histories %>% group_by(patient) %>% count()

CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)

CAN_Drug_Histories
PONS_Demographics
length(unique(CAN_Drug_Histories$patient))



PONS_Demographics_d <- fread("Source/PONS Demographics.txt")
PONS_Demographics_d <- PONS_Demographics_d %>% select(patid, death_date) 

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics_d[, death_date := as.character(death_date)][, death_date := substr(death_date, 1L, 7L)]

PONS_Demographics_d <- PONS_Demographics_d %>% left_join(Months_lookup, by=c("death_date"="Month"))
PONS_Demographics_d <- PONS_Demographics_d %>% select(patid, Exact_Month) %>% rename("death_date"="Exact_Month")
PONS_Demographics_d <- PONS_Demographics_d %>% mutate(death_date=ifelse(is.na(death_date), 61, death_date))


Viz <- PONS_Demographics %>% left_join(PONS_Demographics_d, by=c("patient"="patid"))


Viz <- data.frame(Viz %>%
  mutate(month_number = map2(cancer_metastasis , death_date, seq)) %>%
  unnest(month_number))

data.frame(Viz %>% mutate(month_number=month_number-cancer_metastasis) %>%
  group_by(month_number) %>% summarise(n=sum(weight)))

length(unique(Viz$patient))


unique(CAN_Drug_Histories$cancer_metastasis)


length(unique(CAN_Drug_Histories$patient))

head(Viz)

sort(unique(CAN_Drug_Histories$Treat))

data.frame(
  CAN_Drug_Histories %>%
  left_join(PONS_Ingredients_JN_ChemoClass %>% mutate(molecule=as.character(molecule)), by=c("Treat"="molecule")) %>% 
    filter(Treat=="-" | !is.na(generic_name)) %>% filter(Treat!=355) %>%
  mutate(generic_name=ifelse(is.na(generic_name), "none",
                             ifelse(generic_name=="Palbociclib", "Palbociclib",
                                    ifelse(chemo_class %in% c("Biologic", "Immuno/Targeted", "PD1/PDL1"), "OtherTarget", 
                                           ifelse(chemo_class %in% c("Radio"), "Radio",
                                                  ifelse(chemo_class %in% c("Hormonal Therapy"), "Hormonal", "Other")))))) %>%
  distinct() %>%
  select(patient, weight, Month, generic_name) %>% distinct() %>%
    mutate(rank=ifelse(generic_name=="Palbociclib", 1,
                       ifelse(generic_name=="OtherTarget", 2,
                              ifelse(generic_name=="Radio", 3,
                                     ifelse(generic_name=="Other", 4,
                                            ifelse(generic_name=="Hormonal", 5, 
                                                   ifelse(generic_name=="none",6,7))))))) %>%
  group_by(patient, weight, Month) %>% filter(rank==min(rank)) %>% ungroup() %>%
  group_by(Month, generic_name) %>% summarise(n=sum(as.numeric(weight))) %>%
  spread(key=generic_name, value=n)
)






CAN_Drug_Histories %>%
  left_join(PONS_Ingredients_JN_ChemoClass %>% mutate(molecule=as.character(molecule)), by=c("Treat"="molecule")) %>% 
    filter(Treat=="-" | !is.na(generic_name)) %>% filter(Treat!=355) %>%
  mutate(generic_name=ifelse(is.na(generic_name), "none",
                             ifelse(generic_name=="Palbociclib", "Palbociclib",
                                    ifelse(chemo_class %in% c("Biologic", "Immuno/Targeted", "PD1/PDL1"), "OtherTarget", 
                                           ifelse(chemo_class %in% c("Radio"), "Radio",
                                                  ifelse(chemo_class %in% c("Hormonal Therapy"), "Hormonal", "Other")))))) %>%
  distinct() %>%
  select(patient, weight, Month, generic_name) %>% distinct() %>%
    mutate(rank=ifelse(generic_name=="Palbociclib", 1,
                       ifelse(generic_name=="OtherTarget", 2,
                              ifelse(generic_name=="Radio", 3,
                                     ifelse(generic_name=="Other", 4,
                                            ifelse(generic_name=="Hormonal", 5, 
                                                   ifelse(generic_name=="none",6,7))))))) %>%
  group_by(patient, weight, Month) %>% filter(rank==min(rank)) %>% ungroup() %>% mutate(exp=1) %>%
  spread(key=generic_name, value=exp) %>% select(-rank) %>% group_by(patient) %>%
  filter( is.na(Palbociclib) & !is.na(lead(Palbociclib))) %>% ungroup() %>%
  group_by(Hormonal, none, Other, OtherTarget, Palbociclib, Radio) %>% summarise(n=sum(weight))


# Month-over-month -> Source of Drug Palbociclib
# Month-over-month -> Destination from Drug Palbociclib
# Profiles of Long term / Short term 
# Drugs prior to metastasis , age, bmi, sex





data.frame(CAN_Drug_Histories %>% filter(Treat==179) %>%  mutate(Month=Month-cancer_metastasis) %>%
  select(patient, weight, Month) %>% distinct()  %>%
  group_by(Month) %>% summarise(total=sum(weight)))


ignore <- data.frame(CAN_Drug_Histories %>% filter(Treat==179) %>%  mutate(Month=Month-cancer_metastasis) %>%
             select(patient, weight, Month) %>% distinct() %>%
  left_join(CAN_Drug_Histories  %>%  mutate(Month=Month-cancer_metastasis)) %>%
    filter(Treat!=179) %>%
  left_join(PONS_Ingredients_JN_ChemoClass %>% mutate(molecule=as.character(molecule)), by=c("Treat"="molecule")) %>%
  select(patient, weight, Month, chemo_class) %>% distinct() %>%
  group_by(Month, chemo_class) %>% summarise(n=sum(weight)) %>% 
  spread(key=chemo_class, value=n))

fwrite(ignore, "ignore.csv")




temp_df <- CAN_Drug_Histories %>%
  left_join(PONS_Ingredients_JN_ChemoClass %>% mutate(molecule=as.character(molecule)), by=c("Treat"="molecule")) %>% 
    filter(Treat=="-" | !is.na(generic_name)) %>% filter(Treat!=355) %>%
  mutate(generic_name=ifelse(is.na(generic_name), "none",
                             ifelse(generic_name=="Palbociclib", "Palbociclib",
                                    ifelse(chemo_class %in% c("Biologic", "Immuno/Targeted", "PD1/PDL1"), "OtherTarget", 
                                           ifelse(chemo_class %in% c("Radio"), "Radio",
                                                  ifelse(chemo_class %in% c("Hormonal Therapy"), "Hormonal", "Other")))))) %>%
  distinct() %>%
  select(patient, weight, Month, generic_name) %>% distinct() %>%
    mutate(rank=ifelse(generic_name=="Palbociclib", 1,
                       ifelse(generic_name=="OtherTarget", 2,
                              ifelse(generic_name=="Radio", 3,
                                     ifelse(generic_name=="Other", 4,
                                            ifelse(generic_name=="Hormonal", 5, 
                                                   ifelse(generic_name=="none",6,7))))))) %>%
  group_by(patient, weight, Month) %>% filter(rank==min(rank)) %>% ungroup() %>% mutate(exp=1) %>%
  spread(key=generic_name, value=exp) %>% select(-rank) %>% group_by(patient)


temp_df[is.na(temp_df)] <- 0


data.frame(
  temp_df %>% ungroup() %>% filter(Month<25) %>% group_by(patient) %>%
  filter( Palbociclib==0 & lead(Palbociclib)==1 ) %>% ungroup() %>%
  group_by(Month, Hormonal, none, Other, OtherTarget, Palbociclib, Radio) %>% summarise(n=sum(weight)) %>%
  gather(class, exp, Hormonal:Radio) %>% filter(exp==1) %>%
  arrange(Month, class) %>% select(-exp) %>%
  group_by(Month) %>% mutate(total=sum(n)) %>% mutate(n2=round(100*n/total, 1)) %>%
  select(Month, class, n2) %>% spread(key=class, value=n2)
)  

#    Month Hormonal none Other OtherTarget Radio
# 1      0     30.9 51.3   5.7         9.5   2.6
# 2      1     30.7 24.8  15.8        11.5  17.2
# 3      2     28.8 14.7  12.1        27.2  17.3
# 4      3     38.9 13.4  11.5        27.8   8.4
# 5      4     35.7 13.9  14.7        28.8   6.8
# 6      5     35.5  7.4  16.7        37.0   3.4
# 7      6     29.7  6.0  20.8        34.6   8.9
# 8      7     34.7 12.3   6.0        39.8   7.2
# 9      8     27.1 19.0  10.4        39.6   3.8
# 10     9     35.3 13.0   2.5        39.5   9.7
# 11    10     23.1 17.5  18.5        32.8   8.1
# 12    11     28.7 20.0  17.0        28.5   5.9
# 13    12     58.6  7.6   7.3        26.6    NA
# 14    13     37.1 16.7   6.5        35.8   3.9
# 15    14     29.0 14.7  11.4        37.9   6.9
# 16    15     38.6 24.2   7.1        26.4   3.7
# 17    16     44.1 10.6   8.6        35.3   1.5
# 18    17     37.2 11.4   9.1        34.2   8.2
# 19    18     30.6 14.3  12.7        31.1  11.3
# 20    19     37.1 21.4   2.5        36.2   2.8
# 21    20     57.1  6.6   1.4        26.3   8.7
# 22    21     37.8 13.0   9.0        37.5   2.7
# 23    22     34.2 19.3   2.5        40.3   3.8
# 24    23     30.1 22.0  18.4        27.6   1.9


data.frame(
  temp_df %>% ungroup() %>% filter(Month<25) %>% group_by(patient) %>%
  filter( Palbociclib==0 & lead(Palbociclib)==1 ) %>% ungroup() %>%
  group_by(Month, Hormonal, none, Other, OtherTarget, Palbociclib, Radio) %>% summarise(n=sum(weight)) %>%
  gather(class, exp, Hormonal:Radio) %>% filter(exp==1) %>%
  arrange(Month, class) %>% select(-exp) %>%
  group_by(Month) %>% mutate(total=sum(n)) %>% mutate(n2=round(100*n/total, 1)) %>%
  select(Month, class, n2) %>% spread(key=class, value=n2)
)     %>%
  gather(Class, Pan, Hormonal:Radio) %>%
  ggplot(aes(Month, Pan, colour=Class, fill=Class)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Number of Months After Metastasis \n ") +
  ylab("Patient Proportion (%) \n") +
  ggsci::scale_color_jco() +
  ggsci::scale_fill_jco() 


data.frame(
  temp_df %>% ungroup() %>% filter(Month<25) %>% group_by(patient) %>%
  filter( Palbociclib==0 & lag(Palbociclib)==1 ) %>% ungroup() %>%
  group_by(Month, Hormonal, none, Other, OtherTarget, Palbociclib, Radio) %>% summarise(n=sum(weight)) %>%
  gather(class, exp, Hormonal:Radio) %>% filter(exp==1) %>%
  arrange(Month, class) %>% select(-exp) %>%
  group_by(Month) %>% mutate(total=sum(n)) %>% mutate(n2=round(100*n/total, 1)) %>%
  select(Month, class, n2) %>% spread(key=class, value=n2)
)   


#    Month Hormonal none Other OtherTarget Radio
# 1      1     27.1 15.3   9.6        42.1   5.9
# 2      2     22.1 15.7  21.9        33.0   7.2
# 3      3     41.1  8.5  18.8        25.6   5.9
# 4      4     26.0 19.2  15.4        35.1   4.3
# 5      5     32.9 16.0  12.3        34.9   3.8
# 6      6     26.8 13.5  15.3        41.8   2.6
# 7      7     22.5 10.9  14.4        48.2   4.0
# 8      8     22.0 14.2  15.4        43.6   4.8
# 9      9     17.2  7.2  19.1        51.2   5.3
# 10    10     22.2 14.2  14.5        44.0   5.1
# 11    11     26.8 12.8  15.2        43.7   1.6
# 12    12     25.1  8.6  16.3        47.4   2.6
# 13    13     15.5 10.2  24.8        45.7   3.8
# 14    14     38.4  9.4   8.5        43.7    NA
# 15    15     29.1 13.5  16.6        34.4   6.4
# 16    16     24.1  9.3  12.4        52.0   2.1
# 17    17     25.6 15.9  13.5        40.3   4.7
# 18    18     23.9 16.7  13.5        42.2   3.7
# 19    19     34.4 11.9   4.8        41.2   7.8
# 20    20     26.9  8.1  14.2        48.4   2.4
# 21    21     21.6 17.6  15.6        40.6   4.6
# 22    22     22.8 20.9  14.2        42.2    NA
# 23    23     31.3 14.6  15.7        36.5   1.8
# 24    24     22.6 17.0  20.8        35.5   4.1


data.frame(
  temp_df %>% ungroup() %>% filter(Month<25) %>% group_by(patient) %>%
  filter( Palbociclib==0 & lag(Palbociclib)==1 ) %>% ungroup() %>%
  group_by(Month, Hormonal, none, Other, OtherTarget, Palbociclib, Radio) %>% summarise(n=sum(weight)) %>%
  gather(class, exp, Hormonal:Radio) %>% filter(exp==1) %>%
  arrange(Month, class) %>% select(-exp) %>%
  group_by(Month) %>% mutate(total=sum(n)) %>% mutate(n2=round(100*n/total, 1)) %>%
  select(Month, class, n2) %>% spread(key=class, value=n2)
)   %>%
  gather(Class, Pan, Hormonal:Radio) %>%
  ggplot(aes(Month, Pan, colour=Class, fill=Class)) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Number of Months After Metastasis \n ") +
  ylab("Patient Proportion (%) \n") +
  ggsci::scale_color_jco() +
  ggsci::scale_fill_jco() 

Palbociclib_months_N <-  temp_df %>% filter(Palbociclib==1) %>% group_by(patient, weight) %>% count()

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age, gender, died) 
names(PONS_Demographics)[1] <- "patient"

Palbociclib_months_N %>% inner_join(PONS_Demographics) %>%
  ggplot(aes(age,n)) +
  geom_jitter(alpha=0.5,size=0.2) +
  geom_smooth() +
  theme_minimal() +
  xlab("\n Age") + ylab("Number of Months ON Palbociclib \n")

Palbociclib_months_N %>% inner_join(PONS_Demographics) %>%
  group_by(died) %>% summarise(mean=mean(n))


Palbociclib_months_N %>% inner_join(PONS_Demographics) %>%
  ggplot(aes(x=n)) + 
 geom_density(fill="deepskyblue4", alpha=0.6) +
 theme_minimal() +
 ylab("Patient density \n") +
 xlab("\n Number of Months ON Palbociclib \n ") 

Palbociclib_months_N %>% inner_join(PONS_Demographics) %>%
  mutate(died=ifelse(died=="Y",1,0)) %>%   
  ggplot(aes(x=n, y=died)) + 
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="#66A4B9") +
 theme_minimal() +
 ylab("Probability of having died \n during the follow-up") +
 xlab("\n Number of Months ON Palbociclib \n ") 






PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, diagnosis)]

PONS_Demographics <- Palbociclib_months_N %>% select(patient) %>% inner_join(PONS_Demographics, by=c("patient"="patid"))

setDT(PONS_Demographics)

PONS_Demographics <- unique(PONS_Demographics[, .(patient, diagnosis)])

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)

PONS_Demographics <- setDT(PONS_Demographics)[diagnosis!="Breast Cancer", ] 

PONS_Demographics <- PONS_Demographics %>% group_by(patient) %>% count() %>% filter(n==1) %>% 
  select(patient) %>% left_join(PONS_Demographics) %>% ungroup() 

PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")

PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")

PONS_Demographics %>% inner_join(Palbociclib_months_N) %>% group_by(diagnosis) %>% summarise(mean=mean(n))


PONS_Demographics <- PONS_Demographics %>% inner_join(Palbociclib_months_N) %>% filter(diagnosis!="Prostate")

PONS_Demographics %>% ggplot(aes(diagnosis, n, colour=diagnosis, fill=diagnosis)) +
  geom_jitter() +
  geom_violin(alpha=.5) +
  theme_minimal()



# -----------------
# Percentage who used any biologic/target among all cancer types ----------------------------------

# Any Primary Cancer, Cancer Rx Exp
# % Patients used advanced Rx (biologic/immuno/target)  ~  per primary cancer + Mets vs no-mets

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)

PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis), 0,1))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(PONS_Demographics)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")

CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

setDT(CAN_Drug_Histories)

CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Treat)])

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Treat)])

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

unique(PONS_Ingredients_JN_ChemoClass$chemo_class)

#string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
#  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")

string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


TargetRx_pats <- CAN_Drug_Histories %>% filter(grepl(string_target, Treat)) %>% select(patient) %>% distinct()

names(TargetRx_pats)[1] <- "patid"

TargetRx_pats$Target <- 1

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(TargetRx_pats) %>% mutate(Target=ifelse(is.na(Target), 0 , 1))

New_Primary_Cancer_Box$Primary_Cancer <- str_replace_all(New_Primary_Cancer_Box$Primary_Cancer, " Cancer", "")

temp <- New_Primary_Cancer_Box %>% group_by(Primary_Cancer, cancer_metastasis, Target) %>% summarise(n=sum(weight))

temp <- temp %>% spread(key=Target, value=n) %>% mutate(perc=`1`/(`1`+`0`))

fwrite(temp, "TargetExp_AllCancers.txt")

# -----------------
# Compare patients Palbociclib vs Other target ------------------------

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")

setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[
  Primary_Cancer %in% c("Breast Cancer"),
  .(patid, Primary_Cancer)
]


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, cancer_metastasis) %>% drop_na() %>% select(patid, weight, cancer_metastasis) 


Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]



PONS_Demographics <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box) %>% select(patid, weight, cancer_metastasis)

sum(as.numeric(PONS_Demographics$weight)) # 1424476

PONS_Demographics <- PONS_Demographics %>% filter(cancer_metastasis<=36)

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)

PONS_Demographics <- CancerDrug_Experienced %>% inner_join(PONS_Demographics)

Breast_Mets_DrugExp <- PONS_Demographics







CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[Breast_Mets_DrugExp[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))


PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


Palbociclib_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()
ImmunoTarget_pats <- CAN_Drug_Histories %>% filter(grepl(string_target, Treat)) %>% select(patient) %>% distinct()


N_months_Palbociclib <- CAN_Drug_Histories %>% inner_join(Palbociclib_pats) %>%  filter(grepl("179", Treat)) %>% group_by(patient) %>% count() %>% mutate(group="Palbociclib")
N_months_AnyTarget <- CAN_Drug_Histories %>% inner_join(ImmunoTarget_pats) %>%  filter(grepl(string_target, Treat)) %>% group_by(patient) %>% count() %>% mutate(group="Any Immuno/Target")


N_months_Palbociclib %>% bind_rows(N_months_AnyTarget) %>% group_by(group) %>% summarise(mean=mean(n), sd=sd(n))

#   group              mean    sd
# 1 Any Immuno/Target  12.0  13.2
# 2 Palbociclib        12.9  13.3

N_months_Palbociclib %>% bind_rows(N_months_AnyTarget) %>% group_by(group) %>% summarise(median=median(n), quantiles=quantile(n))

#    group             median quantiles
#  1 Any Immuno/Target      7         1
#  2 Any Immuno/Target      7         2
#  3 Any Immuno/Target      7         7
#  4 Any Immuno/Target      7        17
#  5 Any Immuno/Target      7        60
#  6 Palbociclib            8         1
#  7 Palbociclib            8         3
#  8 Palbociclib            8         8
#  9 Palbociclib            8        19
# 10 Palbociclib            8        60


N_months_Palbociclib %>% bind_rows(N_months_AnyTarget) %>%
  ggplot(aes(n, colour=group, fill=NULL)) + 
  geom_density(size=2,adjust = 0.3, kernel="gaussian") +
  theme_bw() +
  scale_colour_manual(values=c("#C34C60", "#1C80D2")) +
  xlab("\n Number of Months \n ON Pablociclib | Any Target/Immuno \n (Palbociclib/Target-experienced only)") +
  ylab("Patient density \n") +
  theme(legend.position = c(0.8, 0.85))


N_months_Palbociclib %>% bind_rows(N_months_AnyTarget) %>%
  ggplot(aes(group, n, colour=group, fill=NULL)) + 
  geom_violin(size=2, alpha=0.01) +
  geom_boxplot(size=1, alpha=0.01, notch = TRUE, notchwidth = 0.2, width=0.3) +
  geom_jitter(size=0.2, width = 0.25) + 
  theme_bw() +
  scale_colour_manual(values=c("#C34C60", "#1C80D2")) +
  ylab("Number of Months \n ON Pablociclib | Any Target/Immuno \n (Palbociclib/Target-experienced only) \n") +
  xlab("Group \n") +
  theme(legend.position = c(0.5, 0.85))


CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(Palbociclib_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl("179", Treat) & grepl("179", lag(Treat)) ) %>%
  mutate(Target=ifelse(grepl(string_target, Treat), 1,  0)) %>%
  group_by(Target) %>% summarise(n=sum(weight))
  



CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(Palbociclib_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl("179", Treat) & grepl("179", lag(Treat)) ) %>%
  group_by(patient) %>% count() %>% rename("Palbociclib_stops"="n") %>%
  left_join(
    CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(Palbociclib_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(grepl("179", Treat) & !grepl("179", lag(Treat)) ) %>%
  group_by(patient) %>% count() %>% rename("Palbociclib_starts"="n")
  )  %>% filter(Palbociclib_stops==1&Palbociclib_starts) # 68%

CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(Palbociclib_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl("179", Treat) & grepl("179", lag(Treat)) ) %>%
  group_by(patient) %>% count() %>% rename("Palbociclib_stops"="n") %>%
  inner_join(
    CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(Palbociclib_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(grepl("179", Treat) & !grepl("179", lag(Treat)) ) %>%
  group_by(patient) %>% count() %>% rename("Palbociclib_starts"="n")
  ) %>%
  ggplot(aes(Palbociclib_stops, Palbociclib_starts)) +
  geom_jitter(size=0.6, alpha=0.5, colour="#1C80D2") +
  theme_bw() +
  xlim(0,6) + ylim(0,6) +
  scale_colour_manual(values=c( "#1C80D2")) +
  xlab("Number of Times a patient \n HALTED Palbociclib \n") +
  ylab("\n Number of Times a patient was \n STARTED ON Palbociclib") 





CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(ImmunoTarget_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl(string_target, Treat) & grepl(string_target, lag(Treat)) ) %>%
  group_by(patient) %>% count() %>% rename("Target_stops"="n") %>%
  left_join(
    CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(ImmunoTarget_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(grepl(string_target, Treat) & !grepl(string_target, lag(Treat)) ) %>%
  group_by(patient) %>% count() %>% rename("Target_starts"="n")
  )  %>% filter(Target_stops==1&Target_starts) # 71%



CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(ImmunoTarget_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl(string_target, Treat) & grepl(string_target, lag(Treat)) ) %>%
  group_by(patient) %>% count() %>% rename("Target_stops"="n") %>%
  inner_join(
    CAN_Drug_Histories %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(ImmunoTarget_pats) %>% arrange(patient, Month) %>% group_by(patient) %>%
  filter(grepl(string_target, Treat) & !grepl(string_target, lag(Treat)) ) %>%
  group_by(patient) %>% count() %>% rename("Target_starts"="n")
  ) %>%
  ggplot(aes(Target_stops, Target_starts)) +
  geom_jitter(size=0.6, alpha=0.5, colour="#C34C60") +
  theme_bw() +
  xlim(0,6) + ylim(0,6) +
  scale_colour_manual(values=c( "#C34C60")) +
  xlab("Number of Times a patient \n HALTED Target/Immuno \n") +
  ylab("\n Number of Times a patient was \n STARTED ON Target/Immuno") 

ImmunoTarget_pats %>% mutate(Immuno=1) %>% full_join(Palbociclib_pats %>% mutate(Palbo=1)) %>%
  left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  group_by(Immuno, Palbo) %>% summarise(n=sum(weight))


First_stop <- Palbociclib_pats %>% left_join(CAN_Drug_Histories) %>% 
  arrange(patient, Month) %>% group_by(patient) %>%
  filter(!grepl("179", Treat) & grepl("179", lag(Treat)) ) %>% select(patient, Month) %>% filter(Month==min(Month)) %>%
  ungroup() %>% rename("First_stop"="Month")

First_stop %>% left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>% summarise(n=sum(weight)) # 73973

Palbociclib_pats %>% left_join(CAN_Drug_Histories) %>% 
  arrange(patient, Month) %>% group_by(patient) %>%
  inner_join(First_stop) %>% filter(Month>=First_stop) %>%
  filter(!grepl("179", Treat)) %>% filter(grepl(string_target, Treat)) %>% select(patient) %>% distinct() %>%
  ungroup() %>%
  left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>% summarise(n=sum(weight)) # 24135

N_months_Palbociclib %>%  bind_rows(N_months_AnyTarget) %>% 
  inner_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  ggplot(aes(cancer_metastasis, n,, colour=group, fill=group)) +
  geom_jitter(alpha=0.3, size= 0.5) + 
  geom_smooth() +
  theme_bw() +
  ylab("Number of Months ON Palbociclib \n") +
  xlab("\n Exact Month of Metastasis \n (Metastatic prior to monht 36 only) \n")  +
  scale_colour_manual(values=c("#C34C60", "#1C80D2")) +
  scale_fill_manual(values=c("#C34C60", "#1C80D2")) +
  theme(legend.position = c(0.8, 0.85))





PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age) 


N_months_Palbociclib %>% bind_rows(N_months_AnyTarget) %>% 
  inner_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  inner_join(PONS_Demographics, by=c("patient"="patid")) %>%
  ggplot(aes(age, n, colour=group, fill=group)) +
  geom_jitter(alpha=0.3, size= 0.5) + 
  geom_smooth() +
  theme_bw() +
  ylab("Number of Months ON Palbociclib \n") +
  xlab("\n Age \n (years) \n") +
  scale_colour_manual(values=c("#C34C60", "#1C80D2")) +
  scale_fill_manual(values=c("#C34C60", "#1C80D2")) +
  theme(legend.position = c(0.8, 0.85))


First_Palbo <- CAN_Drug_Histories %>% inner_join(Palbociclib_pats) %>% arrange(patient, Month) %>%
  filter(grepl("179", Treat)) %>% group_by(patient) %>% filter(Month==min(Month)) %>% ungroup()

over_time_palbos <- First_Palbo %>% select(patient, Month) %>% rename("First_Palbo"="Month") %>%
  left_join(CAN_Drug_Histories) %>% filter(Month>=First_Palbo) %>% mutate(Month=Month-First_Palbo) %>%
  select(-First_Palbo)

over_time_palbos <- separate_rows(over_time_palbos, Treat, sep = ",", convert=T)

over_time_palbos <- over_time_palbos %>% 
  left_join(PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class), by=c("Treat"="molecule")) %>%
  mutate(chemo_class=ifelse(Treat=="179", "Palbociclib", chemo_class)) %>%
  select(patient, Month, chemo_class) %>% distinct() %>% 
  left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  group_by(Month, chemo_class) %>% summarise(n=sum(weight)) 

initial_colors <- rep("#D3D3D3", 27)
initial_colors[21] <- "#1C80D2"=
initial_colors[17] <- "#C34C60"
initial_colors[15] <- "#FFC529"
initial_colors[13] <- "#000000"
initial_colors[9] <- "#A0D366"



Total_months <- First_Palbo %>% select(patient, Month) %>% rename("First_Palbo"="Month") %>%
  left_join(CAN_Drug_Histories) %>% filter(Month>=First_Palbo) %>% mutate(Month=Month-First_Palbo) %>%
  select(-First_Palbo) %>% ungroup() %>% 
  filter(Treat!="355") %>%
  left_join(Breast_Mets_DrugExp, by=c("patient"="patid")) %>%
  group_by(Month) %>% summarise(total=sum(weight)) %>% select(Month, total)



initial_colors <- rep("#D3D3D3", 26)
initial_colors[19] <- "#1C80D2"
initial_colors[16] <- "#C34C60"
initial_colors[14] <- "#FFC529"
initial_colors[9] <- "#A0D366"
  
over_time_palbos %>%
  filter(chemo_class!="Death") %>%
  left_join(Total_months) %>% mutate(n=n/total) %>%
  mutate(chemo_class=ifelse(is.na(chemo_class), "Lapsed", chemo_class)) %>%
  ggplot(aes(Month, n, colour=chemo_class, fill=chemo_class)) +
  geom_line(size=1) +
  theme_bw() +
  xlab("\n Months relative to 1st Palbociclib Initiation") + 
  ylab("Proportion Still Alive \n ON each class \n") +
  scale_colour_manual(values=initial_colors) 



Months_XY <- Palbociclib_pats %>% left_join(CAN_Drug_Histories) %>% 
  arrange(patient, Month) %>% 
  filter(grepl("179", Treat)) %>% 
  group_by(patient) %>% count() %>% rename("Months_Palbo"="n") %>%
  full_join(
    ImmunoTarget_pats %>% left_join(CAN_Drug_Histories) %>% 
  arrange(patient, Month) %>% 
  filter(grepl(string_target, Treat) & !grepl("179", Treat)) %>% 
  group_by(patient) %>% count() %>% rename("Months_Other"="n")
  )

Months_XY[is.na(Months_XY)] <- 0


Firsts <- ImmunoTarget_pats %>% left_join(CAN_Drug_Histories) %>% 
  arrange(patient, Month) %>% 
  filter(grepl(string_target, Treat))  %>%
   group_by(patient) %>% filter(Month==min(Month)) %>%
   mutate(type=ifelse(grepl("179", Treat), "Palbo", "Other"))

Firsts <- Firsts %>% select(patient, type) %>% ungroup()

fwrite(Months_XY, "Source/Months_XY_palbo_vs_other.txt")

Months_XY %>%
  filter(Months_Palbo>0&Months_Other>0) %>%
  mutate(group=ifelse(Months_Palbo<12&Months_Other<12,1,
                      ifelse(Months_Palbo<12&Months_Other>=12,2,
                             ifelse(Months_Palbo>=12&Months_Other<12,3,4)))) %>%
  inner_join(Firsts) %>% group_by(type, group) %>% count() %>%
  spread(key=type, value=n) %>% mutate(perc=Palbo/(Palbo+Other))


Months_XY %>%
  filter(Months_Palbo>0&Months_Other>0) %>%
  mutate(group=ifelse(Months_Palbo<12&Months_Other<12,1,
                      ifelse(Months_Palbo<12&Months_Other>=12,2,
                             ifelse(Months_Palbo>=12&Months_Other<12,3,4)))) %>%
  ggplot(aes(Months_Palbo, Months_Other, colour=as.factor(group), fill=as.factor(group))) +
  geom_jitter(size=2, alpha=0.3, show.legend = FALSE) +
  theme_bw() +
  xlim(0,60) + ylim(0,60) +
  xlab("\n Number of Months ON Palbociclib") + 
  ylab("Number of Months ON Other Target/Immuno \n") +
  scale_colour_manual(values=c("#D3D3D3", "#C34C60", "#1C80D2", "#FFC529")) +
  geom_hline(yintercept=12,linetype="dashed", size=0.5) + 
  geom_vline(xintercept=12, linetype="dashed", size=0.5)

All_classes <- ImmunoTarget_pats %>% left_join(CAN_Drug_Histories) %>% 
   select(-Month) %>% distinct()
 
All_classes <- separate_rows(All_classes, Treat, sep = ",", convert=T)

All_classes <- All_classes %>% distinct() %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class), by=c("Treat"="molecule"))

All_classes <- All_classes %>% select(patient, chemo_class) %>% distinct()


Months_XY %>%
  filter(Months_Palbo>0&Months_Other>0) %>%
  mutate(group=ifelse(Months_Palbo<12&Months_Other<12,1,
                      ifelse(Months_Palbo<12&Months_Other>=12,2,
                             ifelse(Months_Palbo>=12&Months_Other<12,3,4)))) %>%
  ungroup() %>% group_by(group) %>% mutate(total=n()) %>%
  inner_join(All_classes %>% drop_na()) %>% ungroup() %>%
  group_by(group, chemo_class) %>% mutate(sub_total=n()) %>%
  mutate(perc=sub_total/total) %>%
  select(group, chemo_class, perc) %>% distinct() %>%
  ggplot(aes(chemo_class, perc, colour=as.factor(group), fill=as.factor(group))) +
  geom_col(position="dodge", show.legend = FALSE, width=0.8) +
  theme_bw() +
  xlab("\n Drug Classes") + 
  ylab("Proportion ever tried \n") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_colour_manual(values=c("#D3D3D3", "#C34C60", "#1C80D2", "#FFC529")) +
  scale_fill_manual(values=c("#D3D3D3", "#C34C60", "#1C80D2", "#FFC529")) 



# ----------------
# Continue ---------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")

setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[
  Primary_Cancer %in% c("Breast Cancer"),
  .(patid, Primary_Cancer)
]

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(-Primary_Cancer)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset, cancer_metastasis) %>% drop_na() 



Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]
PONS_Demographics[, cancer_onset := as.character(cancer_onset)][, cancer_onset := substr(cancer_onset, 1L, 7L)]


PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]


PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_onset" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_onset")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_onset")}]


New_Primary_Cancer_Box <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(cancer_onset>=13)

Ms <- data.frame(seq(1,60,1))
names(Ms)[1] <- "Month"
Ms$link <- 1

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(patid) %>% mutate(link=1) %>%
  inner_join(Ms) %>% select(-link) %>% left_join(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% mutate(mets=ifelse(Month==cancer_metastasis, 1, 0 )) %>% 
  mutate(cancer=ifelse(cancer_onset==Month,1,0))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(cancer_onset<=36)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(-c(cancer_metastasis, cancer_onset))

length(unique(New_Primary_Cancer_Box$patid)) * 60 == 668400

New_Primary_Cancer_Box %>% filter(cancer==1) %>% select(patid) %>% distinct()

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% group_by(patid) %>% mutate(cancer2=cumsum(cancer==1)) %>%
  mutate(mets2=cumsum(mets)) %>% select(-c(mets, cancer))


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, death_date) %>% drop_na() 

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )


PONS_Demographics[, death_date := as.character(death_date)][, death_date := substr(death_date, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("death_date" = "Month")
  ][
    ,.SD, .SDcols = !c("death_date")
    ][, {setnames(.SD, old = "Exact_Month", new = "death_date")}]

names(PONS_Demographics)[2] <- "Month"
PONS_Demographics$dead <- 1

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(PONS_Demographics) %>% mutate(dead=ifelse(is.na(dead),0,dead)) %>%
  group_by(patid) %>% mutate(dead2=cumsum(dead==1)) %>% select(-dead)

names(New_Primary_Cancer_Box) <- c("patid", "Month", "cancer", "mets", "dead")

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age, gender) %>% drop_na() 
PONS_Demographics <- PONS_Demographics %>% mutate(gender=ifelse(gender=="M",2,1))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(PONS_Demographics)

summary(glm(as.factor(dead) ~ as.factor(mets), data=New_Primary_Cancer_Box, family=binomial()))
summary(lm(dead ~ as.factor(mets), data=New_Primary_Cancer_Box))

table(New_Primary_Cancer_Box$mets, New_Primary_Cancer_Box$dead)

New_Primary_Cancer_Box %>% group_by(mets, dead) %>% count()

summary(glm(as.factor(dead) ~ as.factor(gender), data=New_Primary_Cancer_Box, family=binomial()))
summary(glm(as.factor(dead) ~ age, data=New_Primary_Cancer_Box, family=binomial()))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% select(-gender)

summary(glm(as.factor(dead) ~ as.factor(cancer) + as.factor(mets), 
            data=New_Primary_Cancer_Box, family=binomial()))

summary(lm(dead ~ as.factor(cancer) +  as.factor(mets), data=New_Primary_Cancer_Box))



CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
setDT(New_Primary_Cancer_Box)
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule")) %>%
  mutate(chemo_class=ifelse(Treat=="-", "Lapsed", chemo_class)) %>%
  mutate(chemo_class=ifelse(Treat=="179", "Palbociclib", chemo_class))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month, chemo_class) %>% distinct() %>% mutate(on=1) %>%
  spread(key=chemo_class, value=on)

CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")
names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), "/", "_")
names(New_Primary_Cancer_Box)[1] <- "patient"

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% left_join(CAN_Drug_Histories )

summary(lm(dead ~ as.factor(Palbociclib) +  as.factor(mets), data=New_Primary_Cancer_Box))

New_Primary_Cancer_Box %>% select(patient) %>% distinct() %>%
  left_join(New_Primary_Cancer_Box %>% filter(dead==1) %>% select(patient) %>% distinct() %>%
  mutate(dead=1)) %>%
  left_join(New_Primary_Cancer_Box %>% filter(Palbociclib==1) %>% select(patient) %>% distinct() %>% 
  mutate(Palbo="Palbo")) %>% group_by(Palbo, dead) %>% count() 


New_Primary_Cancer_Box %>% select(patient) %>% distinct() %>%
  left_join(New_Primary_Cancer_Box %>% filter(dead==1) %>% select(patient) %>% distinct() %>%
  mutate(dead=1)) %>%
  left_join(New_Primary_Cancer_Box %>% filter(Palbociclib==1) %>% select(patient) %>% distinct() %>% 
  mutate(Palbo="Palbo")) %>% 
  left_join(New_Primary_Cancer_Box %>% filter(Immuno_Targeted==1) %>% select(patient) %>% distinct() %>% 
  mutate(Immuno="Immuno")) %>% group_by(Palbo, Immuno, dead) %>% count()  %>%
  spread(key=dead, value=n) %>% mutate(perc=`1`/(`1`+`<NA>`)) 


data.frame(group= c("Palbo + Other Immuno", "Palbo only", "Other Immuno only", "Other Chemo only"), 
           died= c(0.278, 0.257, 0.176, 0.106 ) ) %>%
  ggplot(aes(group, died, colour=group, fill=group)) +
  geom_col() +
  theme_bw() +
  scale_colour_manual(values=c("#D3D3D3", "#C34C60", "#FFC529", "#1C80D2")) +
  scale_fill_manual(values=c("#D3D3D3", "#C34C60", "#FFC529", "#1C80D2")) +
  ylab("Proportion of patients who died \n") +
  xlab("\n Therapy Group") +  
  geom_text(aes(label = died), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = c(0.15, 0.8))


New_Primary_Cancer_Box %>% select(patient, age) %>% distinct() %>% 
  left_join(New_Primary_Cancer_Box %>% filter(Palbociclib==1) %>% select(patient) %>% distinct() %>% 
  mutate(Palbo="Palbo")) %>% group_by(Palbo) %>% summarise(age=mean(age))

PONS_Demographics <- fread("Source/PONS Demographics.txt")

PONS_Demographics <- New_Primary_Cancer_Box %>% select(patient) %>% distinct() %>% 
  left_join(PONS_Demographics %>% select(patid, diagnosis), by=c("patient"="patid"))

PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)

PONS_Demographics %>% group_by(patient) %>% count() %>% ungroup() %>% rename("n2"="n") %>%
  group_by(n2) %>% count()


groups_to_compare <-  New_Primary_Cancer_Box %>% select(patient) %>% distinct() %>%
  left_join(New_Primary_Cancer_Box %>% filter(dead==1) %>% select(patient) %>% distinct() %>%
  mutate(dead=1)) %>%
  left_join(New_Primary_Cancer_Box %>% filter(Palbociclib==1) %>% select(patient) %>% distinct() %>% 
  mutate(Palbo="Palbo")) %>% 
  left_join(New_Primary_Cancer_Box %>% filter(Immuno_Targeted==1) %>% select(patient) %>% distinct() %>% 
  mutate(Immuno="Immuno")) 

groups_to_compare %>% group_by(Palbo, Immuno) %>% count()

groups_to_compare[is.na(groups_to_compare)] <- 0

groups_to_compare <- groups_to_compare %>% mutate(group=ifelse(Palbo=="Palbo"&Immuno=="Immuno", "Palbo & Immuno",
                                                               ifelse(Palbo=="Palbo"&Immuno=="0", "Palbo",
                                                                      ifelse(Palbo=="0"&Immuno=="Immuno", "Immuno", "Other"))))

groups_to_compare <- groups_to_compare %>% select(patient, group)



PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, died, cancer_onset, death_date)

PONS_Demographics$cancer_onset <- as.Date(PONS_Demographics$cancer_onset)
PONS_Demographics$death_date  <- as.Date(PONS_Demographics$death_date)

missingDeathDay <- ymd("2025-07-31")

PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics <- PONS_Demographics %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))

groups_to_compare %>% inner_join(PONS_Demographics, by=c("patient"="patid")) %>% group_by(group) %>% summarise(mean=mean(Survived))

groups_to_compare <- groups_to_compare %>% inner_join(PONS_Demographics, by=c("patient"="patid"))

groups_to_compare <- groups_to_compare %>% mutate(status=ifelse(died=="Y",1,0)) %>% select(-died)




Surv(groups_to_compare$Survived, groups_to_compare$status)[1:10]
  

s1 <- survfit(Surv(Survived, status) ~ 1, data = groups_to_compare)

str(s1)


survfit2(Surv(Survived, status) ~ 1, data = groups_to_compare) %>% 
  ggsurvfit() +
  labs(x = "\n Months post cancer onset",y = "Overall survival probability \n") + 
  ylim(0,1) +
  theme_bw() +
  add_confidence_interval() +
  add_risktable()


summary(survfit(Surv(Survived, status) ~ group, data = groups_to_compare), times = 60)


temp$group <- factor(temp$group, levels = c("None","Pred","Dx"))


sfit <- survfit(Surv(Survived, status)~group, data=groups_to_compare)
sfit
summary(sfit)

ggsurvplot(sfit)

ggtheme = theme_bw()

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Immuno", "Other", "Palbo", "Palbo & Immuno"), legend.title="Treatment group",  
           palette=c("#C34C60", "#D3D3D3", "#1C80D2", "#FFC529"), 
           title="Kaplan-Meier Curve for \n Cancer Survival by Treatment group", 
           risk.table.height=0.2, ggtheme = theme_bw())


groups_to_compare


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics <- PONS_Demographics %>% inner_join(groups_to_compare %>% select(patient), by=c("patid"="patient"))

PONS_Demographics <-  PONS_Demographics %>%
  group_by(patid) %>% count() %>% filter(n>=2) %>% select(patid) %>%
  left_join(PONS_Demographics) %>% filter(diagnosis != "Breast")


data.frame(PONS_Demographics %>% inner_join(groups_to_compare, by=c("patid"="patient")) %>%
  group_by(diagnosis, group, status) %>% count() %>% spread(key=status, value=n) %>%
  mutate(`0`=ifelse(is.na(`0`),0,`0`)) %>% mutate(`1`=ifelse(is.na(`1`),0,`1`)) %>%
    # filter(`0`>0&`1`>0) %>%
  mutate(perc=`1`/(`1`+`0`))) %>%
  select(diagnosis, group, perc) %>%
  spread(key=group, value=perc)
  


# ---------------
# Compare high duration palbociclib vs high duration other target ------------


Months_XY <- fread("Source/Months_XY_palbo_vs_other.txt")

Months_XY <- Months_XY %>%
  filter(Months_Palbo>0&Months_Other>0) %>%
  mutate(group=ifelse(Months_Palbo<12&Months_Other<12,1,
                      ifelse(Months_Palbo<12&Months_Other>=12,2,
                             ifelse(Months_Palbo>=12&Months_Other<12,3,4))))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age)

Months_XY %>% 
  left_join(PONS_Demographics, by=c("patient"="patid"))  %>%
  group_by(group) %>% summarise(n=mean(age))

# 1     1  63.9
# 2     2  63.1
# 3     3  63.8
# 4     4  63.2

Months_XY %>% group_by(group) %>% count()



PONS_Comorbidity_Inventories <- fread("Source/PONS Comorbidity Inventories.txt")
names(PONS_Comorbidity_Inventories)[1] <- "patient"

PONS_Comorbidity_Inventories <- Months_XY %>%   left_join(PONS_Comorbidity_Inventories)

unique(PONS_Comorbidity_Inventories$group)

unique(PONS_Comorbidity_Inventories$diagnosis)

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(group==2|group==3)
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% mutate(group=as.factor(group))

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% mutate(exp=1) %>% spread(key=diagnosis, value=exp)
PONS_Comorbidity_Inventories[is.na(PONS_Comorbidity_Inventories)] <- 0

names(PONS_Comorbidity_Inventories)

PONS_Comorbidity_Inventories2 <- PONS_Comorbidity_Inventories %>% select(-c(weight, Months_Palbo, Months_Other, patient))

library("randomForest")

modelAll_1_randomForest <- randomForest(group ~ ., data = PONS_Comorbidity_Inventories2)

summary(modelAll_1_randomForest)

RF_IMP <- modelAll_1_randomForest$importance

RF_IMP <- data.frame(RF_IMP)

names(RF_IMP)
RF_IMP %>% arrange(-MeanDecreaseGini) %>% slice(1:100)

PONS_Comorbidity_Inventories2 %>% group_by(group, F41) %>% count()
dim(PONS_Comorbidity_Inventories2)





PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")

Months_XY %>% group_by(group) %>% count()

data.frame(
  Months_XY %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>%
  group_by(group, diagnosis) %>% count() %>%
  spread(key=group, value=n) %>% 
    mutate(`2`=ifelse(is.na(`2`),0,`2`)) %>%
    mutate(`3`=ifelse(is.na(`3`),0,`3`)) %>%
    mutate(`4`=ifelse(is.na(`4`),0,`4`)) %>%
  mutate(`1`=`1`/429) %>% mutate(`2`=`2`/83) %>% mutate(`3`=`3`/251) %>% mutate(`4`=`4`/65)
  )


 Months_XY %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>%
   group_by(group, patient) %>% count() %>% rename("n2"="n") %>% ungroup() %>%
   group_by(group) %>% summarise(mean=mean(n2))
 
# 1     1  4.94
# 2     2  4.78
# 3     3  4.61
# 4     4  4.72

#           diagnosis          X1         X2          X3         X4
# 1              Bone 0.869463869 0.89156627 0.896414343 0.89230769
# 2             Brain 0.326340326 0.25301205 0.219123506 0.27692308
# 3            Breast 1.000000000 1.00000000 1.000000000 1.00000000
# 4  Gastroesophageal 0.023310023 0.04819277 0.027888446 0.01538462
# 5              Head 0.030303030 0.01204819 0.023904382 0.00000000
# 6        Intestinal 0.090909091 0.13253012 0.083665339 0.16923077
# 7            Kidney 0.027972028 0.06024096 0.019920319 0.03076923
# 8          Leukemia 0.041958042 0.01204819 0.039840637 0.03076923
# 9             Liver 0.673659674 0.53012048 0.533864542 0.56923077
# 10             Lung 0.561771562 0.44578313 0.517928287 0.52307692
# 11         Lymphoma 0.585081585 0.62650602 0.533864542 0.55384615
# 12          Myeloma 0.011655012 0.04819277 0.011952191 0.03076923
# 13            Other 0.396270396 0.34939759 0.366533865 0.30769231
# 14       Pancreatic 0.013986014 0.00000000 0.007968127 0.01538462
# 15         Prostate 0.004662005 0.02409639 0.007968127 0.03076923
# 16     Reproductive 0.069930070 0.08433735 0.075697211 0.13846154
# 17      Respiratory 0.009324009 0.00000000 0.015936255 0.01538462
# 18         Salivary 0.004662005 0.00000000 0.000000000 0.00000000
# 19             Skin 0.158508159 0.16867470 0.187250996 0.09230769
# 20          Thyroid 0.016317016 0.04819277 0.015936255 0.00000000
# 21          Urinary 0.020979021 0.04819277 0.019920319 0.03076923



# per group, Rxs around flow palbociclib to -> other 
# per group, Dxs around flow palbociclib to -> other 
 
 
 

# --------------
# Drugs before/after flow to/from palbociclib --------------
Months_XY <- fread("Source/Months_XY_palbo_vs_other.txt")

Months_XY <- Months_XY %>%
  filter(Months_Palbo>0&Months_Other>0) %>%
  mutate(group=ifelse(Months_Palbo<12&Months_Other<12,1,
                      ifelse(Months_Palbo<12&Months_Other>=12,2,
                             ifelse(Months_Palbo>=12&Months_Other<12,3,4))))



CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")

CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[Months_XY[, .(patient)], on = c("patient"="patient"), nomatch = 0]

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

setDT(CAN_Drug_Histories)

CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")

CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

CAN_Drug_Histories %>%
  left_join(Months_XY %>% select(patient, group)) %>%
  arrange(patient, Month) %>% group_by(patient) %>%
  filter( grepl("179", Treat) & !grepl("179", lead(Treat)) & grepl(string_target, lead(Treat))  ) %>%
   ungroup() %>% group_by(group) %>% count()
  

Outflows <- CAN_Drug_Histories %>%
  left_join(Months_XY %>% select(patient, group)) %>%
  arrange(patient, Month) %>% group_by(patient) %>%
  filter( grepl("179", Treat) & !grepl("179", lead(Treat)) & grepl(string_target, lead(Treat))  )  %>%
  ungroup() %>% select(patient, group, Month) %>% rename("Flow_from_Palbo"="Month")



Outflows %>% select(patient, group) %>% distinct() %>%
  group_by(group) %>% count()


data.frame(Outflows %>% filter(group %in% c(2,3)) %>%
             left_join(CAN_Drug_Histories) %>% 
  filter(Month>=(Flow_from_Palbo-6) & Month<Flow_from_Palbo ) %>%
    select(patient, group, Treat) %>% distinct() %>% 
  select(group, Treat) %>% group_by(group, Treat) %>% count() %>%
    spread(key=group, value=n) %>%
    arrange(-`2`)) 





# V2 with everyone


New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[
  Primary_Cancer %in% c("Breast Cancer"),
  .(patid, Primary_Cancer)
]

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")

CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]

CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)

setDT(CAN_Drug_Histories)

CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])

CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

CAN_Drug_Histories %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 4329706

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


Palbo <- CAN_Drug_Histories %>% filter(Treat!="-") %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()

Outflows <- Palbo %>% left_join(CAN_Drug_Histories) %>%
  arrange(patient, weight, Month) %>% group_by(patient) %>%
  filter( grepl("179", Treat) & !grepl("179", lead(Treat)) & grepl(string_target, lead(Treat)))  %>%
  ungroup() %>% select(patient, Month) %>% rename("Flow_from_Palbo"="Month")

Outflows %>% group_by(patient) %>% count() %>% arrange(-n)

temp <- Outflows %>% left_join(CAN_Drug_Histories) %>% mutate(Month2=Month-Flow_from_Palbo) %>%
  select(patient, weight, Month2, Treat)

temp <- separate_rows(temp, Treat, sep = ",", convert=T)

temp <- temp %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class), by=c("Treat"="molecule")) %>%
  mutate(chemo_class=ifelse(grepl("179", Treat), "Palbociclib", chemo_class))
  
temp <- temp %>% select(-Treat) %>% distinct()

temp[is.na(temp)] <- "Lapsed"

temp %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 10399


initial_colors <- rep("#D3D3D3", 26)
initial_colors[20] <- "#1C80D2"
initial_colors[16] <- "#C34C60"
initial_colors[14] <- "#FFC529"
initial_colors[13] <- "#000000"
initial_colors[9] <- "#A0D366"

initial_colors[4] <- "#6633A0"
initial_colors[12] <- "#32016A"


temp %>% group_by(Month2, chemo_class) %>% summarise(n=sum(weight)/10399) %>%
  filter(Month2>=(-36) & Month2<=36) %>%
  filter(Month2==0)



temp %>% group_by(Month2, chemo_class) %>% summarise(n=sum(weight)/10399) %>%
  filter(Month2>=(-36) & Month2<=36) %>%
  ggplot(aes(Month2, 100*n, colour=chemo_class)) +
  geom_line(size=2, alpha=0.7)+
  scale_colour_manual(values=initial_colors) +
  theme_bw() +
  xlab("\n Months relative to Outflow from Palbociclib to other Target/Immuno") + 
  ylab("Proportion of Patients \n") 











PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted" & PONS_Ingredients_JN_ChemoClass$molecule != "179"], collapse = "|"),")\\b")





Target <- CAN_Drug_Histories %>% filter(Treat!="-") %>% filter(grepl(string_target, Treat)&!grepl("179", Treat)) %>% select(patient) %>% distinct()

Outflows <- Target %>% left_join(CAN_Drug_Histories) %>%
  arrange(patient, weight, Month) %>% group_by(patient) %>%
  filter( grepl(string_target, Treat) & !grepl(string_target, lead(Treat)) & grepl("179", lead(Treat)))  %>%
  ungroup() %>% select(patient, Month) %>% rename("Flow_from_Target"="Month")

Outflows %>% group_by(patient) %>% count() %>% arrange(-n)

temp <- Outflows %>% left_join(CAN_Drug_Histories) %>% mutate(Month2=Month-Flow_from_Target) %>%
  select(patient, weight, Month2, Treat)

temp <- separate_rows(temp, Treat, sep = ",", convert=T)

temp <- temp %>% left_join(PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class), by=c("Treat"="molecule")) %>%
  mutate(chemo_class=ifelse(grepl("179", Treat), "Palbociclib", chemo_class))
  
temp <- temp %>% select(-Treat) %>% distinct()

temp[is.na(temp)] <- "Lapsed"

temp %>% select(patient, weight) %>% distinct() %>% summarise(n=sum(weight)) # 10399


initial_colors <- rep("#D3D3D3", 23)
initial_colors[18] <- "#1C80D2"
initial_colors[14] <- "#C34C60"
initial_colors[12] <- "#FFC529"
initial_colors[11] <- "#000000"
initial_colors[7] <- "#A0D366"

initial_colors[3] <- "#6633A0"
initial_colors[10] <- "#32016A"


temp %>% group_by(Month2, chemo_class) %>% summarise(n=sum(weight)/1804) %>%
  filter(Month2>=(-36) & Month2<=36) %>%
  filter(Month2==0)



temp %>% group_by(Month2, chemo_class) %>% summarise(n=sum(weight)/1804) %>%
  filter(Month2>=(-36) & Month2<=36) %>%
  ggplot(aes(Month2, 100*n, colour=chemo_class)) +
  geom_line(size=2, alpha=0.7)+
 scale_colour_manual(values=initial_colors) +
  theme_bw() +
  xlab("\n Months relative to Outflow from other Target/Immuno to Palbociclib") + 
  ylab("Proportion of Patients \n") 

# -------------------------

# Anemia and Neutropenia ----------------------------------

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
unique(New_Primary_Cancer_Box$Primary_Cancer)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[New_Primary_Cancer_Box$Primary_Cancer!="-"]
sum(New_Primary_Cancer_Box$weight) # 22984889

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced) %>% summarise(n=sum(weight)) # 9861087
data.frame(New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced) %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=Primary_Cancer, value=n)




PONS_Comorbidity_Inventories <- fread("Source/PONS Comorbidity Inventories.txt")
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% filter(diagnosis %in% c("D70", "D50", "D51", "D52", "D53", "D60", "D61", "D63", "D64"))
# D70
# D50, D51, D52, D53
# D60, D61, D63, D64

PONS_Comorbidity_Inventories <- New_Primary_Cancer_Box %>% select(patid) %>% inner_join(PONS_Comorbidity_Inventories)
PONS_Comorbidity_Inventories <- CancerDrug_Experienced %>% select(patid) %>% inner_join(PONS_Comorbidity_Inventories)

PONS_Comorbidity_Inventories %>% group_by(diagnosis) %>% summarise(n=sum(weight))

# 1 D50       2390178.
# 2 D51        660865.
# 3 D52        109255.
# 4 D53        507207.
# 5 D60         17001.
# 6 D61        973313.
# 7 D63       1753304.
# 8 D64       4687671 
# 9 D70       1535403.

data.frame(PONS_Comorbidity_Inventories %>%   inner_join(New_Primary_Cancer_Box) %>%
 group_by(Primary_Cancer, diagnosis) %>% summarise(n=sum(weight)) %>%
  spread(key=diagnosis, value=n))

data.frame(PONS_Comorbidity_Inventories %>% filter(diagnosis!="D70") %>%
  inner_join(New_Primary_Cancer_Box) %>%
  select(patid, Primary_Cancer, weight) %>% distinct() %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))) # 5529369


PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% inner_join(New_Primary_Cancer_Box)
names(PONS_Comorbidity_Inventories)[1] <- "patient"

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Treat)])
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Comorbidity_Inventories %>% select(patient) %>% distinct())

Stim_treated <- CAN_Drug_Histories %>% filter(grepl("30", Treat)|grepl("40", Treat)) %>% select(patient) %>% distinct()
Etin_treated <- CAN_Drug_Histories %>% filter(grepl("24", Treat)|grepl("28", Treat)|grepl("29", Treat)) %>% select(patient) %>% distinct()


PONS_Comorbidity_Inventories %>% filter(diagnosis=="D70") %>% summarise(n=sum(weight))
PONS_Comorbidity_Inventories %>% inner_join(Stim_treated) %>% filter(diagnosis=="D70") %>% summarise(n=sum(weight)) # 0.8205553


data.frame(PONS_Comorbidity_Inventories %>% inner_join(Stim_treated) %>% filter(diagnosis=="D70") %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight)))


PONS_Comorbidity_Inventories %>% filter(diagnosis!="D70") %>%
  group_by(diagnosis) %>%
  inner_join(Etin_treated) %>%
  summarise(n=sum(weight))


PONS_Comorbidity_Inventories %>% filter(diagnosis!="D70") %>%
  select(patient, weight) %>% distinct() %>%
  inner_join(Etin_treated) %>%
  summarise(n=sum(weight))


data.frame(PONS_Comorbidity_Inventories %>% filter(diagnosis!="D70") %>%
  group_by(diagnosis, Primary_Cancer) %>%
  inner_join(Etin_treated) %>%
  summarise(n=sum(weight)) %>%
  spread(key=diagnosis, value = n))


data.frame(PONS_Comorbidity_Inventories %>% filter(diagnosis!="D70") %>%    
  select(patient, Primary_Cancer, weight) %>% distinct() %>%
  group_by(Primary_Cancer) %>%
  inner_join(Etin_treated) %>%
  summarise(n=sum(weight)))


# HOW MANY DX BEFORE CANCER; AFTER; BEFORE RX; AFTER 


New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[New_Primary_Cancer_Box$Primary_Cancer!="-"]
sum(New_Primary_Cancer_Box$weight) # 22984889

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced) %>% summarise(n=sum(weight)) # 9861087
data.frame(New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced) %>% group_by(Primary_Cancer) %>% summarise(n=sum(weight))) %>%
  spread(key=Primary_Cancer, value=n)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)


Dxs_Anemia_Neutropenia <- fread("Source/Dxs_Anemia_Neutropenia.txt")

First_Neutro_Dx <- Dxs_Anemia_Neutropenia %>% filter(grepl("D70", diag)) %>% select(patid, date) %>% distinct() %>%
  mutate(date=as.Date(date)) %>% group_by(patid) %>% filter(date==min(date)) %>% slice(1) %>% ungroup

First_Neutro_Dx <- New_Primary_Cancer_Box %>% left_join(First_Neutro_Dx)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

First_Neutro_Dx <- First_Neutro_Dx %>% left_join(PONS_Demographics)

CAN_Doses <- fread("Source/CAN Doses.txt")
CAN_Doses <- CAN_Doses %>% inner_join(First_Neutro_Dx %>% select(patid), by=c("pat_id"="patid"))
unique(CAN_Doses$drug_group)

CAN_Doses <- CAN_Doses %>% filter(drug_group=="Anticancer" | drug_group=="GDF15")
CAN_Doses <- CAN_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% select(pat_id, from_dt) %>% distinct() %>%
  group_by(pat_id) %>% filter(from_dt==min(from_dt)) %>% slice(1) %>% ungroup() %>% rename("patid"="pat_id")

First_Neutro_Dx <- First_Neutro_Dx %>% left_join(CAN_Doses)

sum(First_Neutro_Dx$weight)  # 9861087
First_Neutro_Dx %>% filter(!is.na(date)) %>% summarise(n=sum(weight)) # 1567510
First_Neutro_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>% summarise(n=sum(weight)) # 1393404

First_Neutro_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  summarise(n=sum(weight)) # 1282776



PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis),0,1))


data.frame(First_Neutro_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight))) 


data.frame(First_Neutro_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
    left_join(PONS_Demographics) %>%
  group_by(Primary_Cancer, cancer_metastasis) %>%
  summarise(n=sum(weight))) 





First_Neutro_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  inner_join(Stim_treated, by=c("patid"="patient")) %>%
  summarise(n=sum(weight)) # 1282776


data.frame(First_Neutro_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  inner_join(Stim_treated, by=c("patid"="patient")) %>%
      group_by(Primary_Cancer) %>%
  summarise(n=sum(weight)))



data.frame(First_Neutro_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  inner_join(Stim_treated, by=c("patid"="patient")) %>%
        left_join(PONS_Demographics) %>%
  group_by(Primary_Cancer, cancer_metastasis) %>%
      group_by(Primary_Cancer,cancer_metastasis) %>%
  summarise(n=sum(weight)))



# Anemia

First_Anemia_Dx <- Dxs_Anemia_Neutropenia %>% filter(grepl("D50", diag)|grepl("D51", diag)|
                                                       grepl("D52", diag)|grepl("D53", diag)|
                                                       grepl("D60", diag)|grepl("D61", diag)|
                                                       grepl("D63", diag)|grepl("D64", diag)) %>% select(patid, date) %>% distinct() %>%
  mutate(date=as.Date(date)) %>% group_by(patid) %>% filter(date==min(date)) %>% slice(1) %>% ungroup

First_Anemia_Dx <- New_Primary_Cancer_Box %>% left_join(First_Anemia_Dx)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

First_Anemia_Dx <- First_Anemia_Dx %>% left_join(PONS_Demographics)

CAN_Doses <- fread("Source/CAN Doses.txt")
CAN_Doses <- CAN_Doses %>% inner_join(First_Anemia_Dx %>% select(patid), by=c("pat_id"="patid"))
unique(CAN_Doses$drug_group)

CAN_Doses <- CAN_Doses %>% filter(drug_group=="Anticancer" | drug_group=="GDF15")
CAN_Doses <- CAN_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% select(pat_id, from_dt) %>% distinct() %>%
  group_by(pat_id) %>% filter(from_dt==min(from_dt)) %>% slice(1) %>% ungroup() %>% rename("patid"="pat_id")

First_Anemia_Dx <- First_Anemia_Dx %>% left_join(CAN_Doses)

sum(First_Anemia_Dx$weight)  # 9861087
First_Anemia_Dx %>% filter(!is.na(date)) %>% summarise(n=sum(weight)) # 5601342
First_Anemia_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>% summarise(n=sum(weight)) # 3460135


First_Anemia_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  summarise(n=sum(weight))  # 2581556


First_Anemia_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  inner_join(Etin_treated, by=c("patid"="patient")) %>%
  summarise(n=sum(weight))


data.frame(First_Anemia_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  group_by(Primary_Cancer) %>%
  summarise(n=sum(weight)))  # 2581556

data.frame(First_Anemia_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Anemia_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  inner_join(Etin_treated, by=c("patid"="patient")) %>%
    group_by(Primary_Cancer) %>%
  summarise(n=sum(weight)))



# Bone mets
PONS_Events <- fread("Source/PONS Events.txt")
PONS_Events <- PONS_Events %>% filter(grepl("C795", code))
PONS_Events <- PONS_Events %>% mutate(claimed=as.Date(claimed)) %>% group_by(patid) %>% filter(claimed==min(claimed)) %>% 
  select(patid, claimed)

PONS_Events <- PONS_Events %>% slice(1) %>% ungroup()

First_BoneMets_Dx <- New_Primary_Cancer_Box %>% left_join(PONS_Events)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_onset)

First_BoneMets_Dx <- First_BoneMets_Dx %>% left_join(PONS_Demographics)

CAN_Doses <- fread("Source/CAN Doses.txt")
CAN_Doses <- CAN_Doses %>% inner_join(First_BoneMets_Dx %>% select(patid), by=c("pat_id"="patid"))
unique(CAN_Doses$drug_group)

CAN_Doses <- CAN_Doses %>% filter(drug_group=="Anticancer" | drug_group=="GDF15")
CAN_Doses <- CAN_Doses %>% mutate(from_dt=as.Date(from_dt)) %>% select(pat_id, from_dt) %>% distinct() %>%
  group_by(pat_id) %>% filter(from_dt==min(from_dt)) %>% slice(1) %>% ungroup() %>% rename("patid"="pat_id")

First_BoneMets_Dx <- First_BoneMets_Dx %>% left_join(CAN_Doses)


sum(First_BoneMets_Dx$weight)  # 9861087
First_BoneMets_Dx %>% filter(!is.na(claimed)) %>% summarise(n=sum(weight)) # 1290929
First_BoneMets_Dx %>% filter(!is.na(claimed)) %>%
  anti_join(First_BoneMets_Dx %>% filter(claimed<cancer_onset) %>% select(patid)) %>% summarise(n=sum(weight)) # 1279656


Zoledronic_Acid_procedures <- fread("Source/Zoledronic_Acid_procedures.txt")
Zoledronic_Acid_procedures <- Zoledronic_Acid_procedures %>% select(patid) %>% distinct()
Zoledronic_Acid_rxs <- fread("Source/Zoledronic_Acid_rxs.txt")
Zoledronic_Acid_rxs <- Zoledronic_Acid_rxs %>% select(patid) %>% distinct()

First_BoneMets_Dx %>% filter(!is.na(claimed)) %>%
  inner_join(Zoledronic_Acid_rxs %>% full_join(Zoledronic_Acid_procedures)) %>%
  summarise(n=sum(weight)) # 252053



# # ANEMIA
# 
# PONS_Measures <- fread("Source/PONS Measures.txt")
# unique(PONS_Measures$test)
# PONS_Measures <- PONS_Measures %>% filter(test=="Hemoglobin")
# PONS_Measures <- PONS_Measures %>% select(patid, value, claimed)
# 
# PONS_Demographics <- fread("Source/PONS Demographics.txt")
# PONS_Demographics <- PONS_Demographics %>% select(patid, gender, cancer_onset)
# 
# PONS_Measures <- PONS_Measures %>% left_join(PONS_Demographics) %>% 
#   inner_join(New_Primary_Cancer_Box) %>% inner_join(CancerDrug_Experienced)
# 
# length(unique(PONS_Measures$patid))
# 
# Extra_Anemia <- PONS_Measures %>%   filter(value>5 & value<20) %>%
#  filter( (gender=="F" & value<= 11.9)  |  (gender=="M" & value<= 12.9) ) %>%
#   select(patid) %>% distinct() %>% rename("patient"="patid")
# 
# 
# data.frame(PONS_Comorbidity_Inventories %>% filter(diagnosis!="D70") %>%    
#   select(patient) %>% distinct() %>%
#     full_join(Extra_Anemia) %>% distinct() %>%
#   left_join(New_Primary_Cancer_Box, by=c("patient"="patid")) %>%
#     summarise(n=sum(weight)))
# 
# # 5,529,369 -> 6,317,307
# 
# 
# PONS_Measures  %>%
#   select(value) %>%
#   ggplot(aes(value)) + geom_density()
# 
# PONS_Measures <- data.frame(PONS_Measures)
# 
# PONS_Demographics <- data.frame(PONS_Demographics)
# 
# plot <- PONS_Measures %>% left_join(PONS_Demographics %>% select(-cancer_onset)) %>%
#   mutate(month=as.numeric(claimed)-as.numeric(cancer_onset)) %>%
#   arrange(patid, month, value) %>%
#   select(patid, month, value, gender, Primary_Cancer)
# 
# 
# plot %>%
#   filter(Primary_Cancer=="Intestinal Cancer") %>%
#   select(patid) %>% distinct() %>% # sample_n(10000) %>%
#   left_join(plot) %>%
#   filter(value>5 & value<20) %>%
#   ggplot(aes(month/30.5, value, colour=gender, fill=gender)) +
#   #geom_jitter(size=0.1, colour="black", alpha=0.2) +
#   coord_cartesian(ylim = c(8, 18))+
#   #geom_line(aes(group=patid), col="black" , alpha=0.2) +
#   geom_smooth(method="gam",  formula = y ~ s(x, bs = "cs", k =5)) +
#   theme_bw() +
#   scale_colour_manual(values=c("#C34C60", "#1C80D2")) +
#   scale_fill_manual(values=c("#C34C60", "#1C80D2")) +
#   xlab("\n Number of Months \n Relative to Cancer Diagnosis") +
#   ylab("Exact Hemoglobin \n") +
#   theme(legend.position = c(0.9, 0.8))








# # NEUROPENIA
# 
# labs_Anemia_Neutropenia <- fread("Source/labs_Anemia_Neutropenia.txt")
# 
# 
# # 26499-4	Neutrophils [#/volume] in Blood	Neutrophils
# # 751-8	Neutrophils [#/volume] in Blood by Automated count	Neutrophils
# # 752-6	Deprecated Neutrophils [#/volume] in Blood by Automated count	Neutrophils
# # 753-4	Neutrophils [#/volume] in Blood by Manual count	Neutrophils
# # 26464-8	Leukocytes [#/volume] in Blood	Leukocytes
# # 30406-3	Leukocytes other [#/volume] in Blood	Leukocytes other
# # 49498-9	Leukocytes [#/volume] in Blood by Estimate	Leukocytes
# # 51383-8	Leukocytes other [#/volume] in Blood by Automated count	Leukocytes other
# # 6690-2	Leukocytes [#/volume] in Blood by Automated count	Leukocytes
# # 729-4	Leukocytes other [#/volume] in Blood by Manual count	Leukocytes other
# # 26453-1	Erythrocytes [#/volume] in Blood	Erythrocytes
# # 789-8	Erythrocytes [#/volume] in Blood by Automated count	Erythrocytes
# # 790-6	Erythrocytes [#/volume] in Blood by Manual count	Erythrocytes
# # 20570-8	Hematocrit [Volume Fraction] of Blood	Hematocrit
# # 41654-5	Hematocrit [Volume Fraction] of Venous blood	Hematocrit
# # 4544-3	Hematocrit [Volume Fraction] of Blood by Automated count	Hematocrit
# # 4545-0	Hematocrit [Volume Fraction] of Blood by Centrifugation	Hematocrit
# # 48703-3	Hematocrit [Volume Fraction] of Blood by Estimated	Hematocrit
# # 71829-6	Hematocrit [Pure volume fraction] of Venous blood	Hematocrit
# 
# Neutros_labs <- labs_Anemia_Neutropenia %>% filter(loinc_cd %in% c("26499-4", "751-8", "752-6", "753-4")) 
# length(unique(Neutros_labs$patid))
# 
# unique(Neutros_labs$hi_nrml)
# 
# unique(Neutros_labs$rslt_unit_nm)
# 
# Neutros_labs %>% ggplot(aes(rslt_nbr)) + geom_histogram(bins=30)
# 
# data.frame(Neutros_labs %>% group_by(rslt_unit_nm) %>% count()  %>% arrange(-n))
# 
# Neutros_labs %>% filter()


# ----------------------------

# Temp hemoglobin vs persitency ON Palbociclib --------------
CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_MeasuresLabs <- fread("Source/PONS Measures Labs.txt")
unique(PONS_MeasuresLabs$test)
PONS_MeasuresLabs <- New_Primary_Cancer_Box %>% inner_join(PONS_MeasuresLabs)
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% filter(test != "Cancer Stage" & test != "Body Height")
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% select(patid, test, unit, claimed, value) %>% distinct()
PONS_MeasuresLabs[, claimed := as.character(claimed)][, claimed := substr(claimed, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_MeasuresLabs <- PONS_MeasuresLabs[Months_lookup, on = c("claimed" = "Month")]
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% filter(test=="Hemoglobin") %>% select(patid, value, Exact_Month)
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% group_by(patid, Exact_Month) %>% summarise(value=min(value)) %>% ungroup()




CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))


PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

Palbociclib_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()

CAN_Drug_Histories <- Palbociclib_pats %>% inner_join(CAN_Drug_Histories) 
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(
  CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% 
  group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, Month) %>% rename("Min"="Month")
)  %>% mutate(Elapsed=Month-Min)
  
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month, Elapsed, ON_Palbo) %>% 
  left_join(PONS_MeasuresLabs, by=c("patient"="patid", "Month"="Exact_Month"))



CAN_Drug_Histories %>% 
  filter(Elapsed>=(0)) %>%
  ggplot(aes(Elapsed, ON_Palbo )) +
  geom_col() +
  geom_smooth()


CAN_Drug_Histories %>% 
  filter(Elapsed>=(0)) %>%
  ggplot(aes(Elapsed, value )) +
  #geom_col() +
  geom_smooth()



CAN_Drug_Histories %>% 
  filter(Elapsed>=(0)) %>%
  ggplot(aes(value, ON_Palbo )) +
  #geom_col() +
  geom_smooth()

library(plotly)
library(splines)

temp <- CAN_Drug_Histories %>% filter(Elapsed>=0) %>% drop_na()



spline_model <- glm(ON_Palbo ~ ns(Elapsed, df = 10) + ns(value, df = 5), 
                    data = temp, family = "binomial")



# gam_model <- mgcv::gam(ON_Palbo ~ s(Elapsed) + s(value), data = temp, family = binomial)
# summary(gam_model)

logit_model <- glm(ON_Palbo ~ Elapsed + value, data = temp, family = "binomial")


time_seq <- seq(min(temp$Elapsed), max(temp$Elapsed), length.out = 100)
biomarker_seq <- seq(min(temp$value), max(temp$value), length.out = 100)
grid <- expand.grid(Elapsed = time_seq, value = biomarker_seq)

predicted_probs <- predict(spline_model, newdata = grid, type = "response")
grid$probability <- predicted_probs



plot(grid$Elapsed, grid$probability)


ggplot(grid, aes(x = Elapsed, y = value, z = probability)) +
  xlim(0,60) +
  #geom_point(aes(color = probability), size = 3) +
  geom_tile(aes(fill = probability)) +
  scale_fill_gradient(low = "blue", high = "red") 


# ---------------

# Flows 4 Therapy Lines Breast Cancer With Rank   ---------------------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
setDT(New_Primary_Cancer_Box)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer %in% c("Breast Cancer"), .(patid, Primary_Cancer)]

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics[ , .(patid, cancer_metastasis)]
PONS_Demographics <- PONS_Demographics[!is.na(cancer_metastasis)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics, by=c("patient"="patid")) %>% 
  select(-disease) %>% filter(Month>=cancer_metastasis)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-cancer_metastasis)
  
CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% mutate(Month2=row_number()-1) %>% select(-Month)

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

unique(PONS_Ingredients_JN_ChemoClass$generic_name)



string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")


string_CDK <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Palbociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Ribociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Abemaciclib"], collapse = "|"),")\\b")


string_EveroFul <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Everolimus"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Fulvestrant"], collapse = "|"),")\\b")


string_Niche <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Alpelisib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Elacestrant"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Olaparib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Talazoparib"], collapse = "|"),")\\b")


string_Dead <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$drug_group=="Death"], collapse = "|"),")\\b")


string_OtherTarget <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[ (PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"|
                                                                                       PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic") &
                                                                                       PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"], collapse = "|"),")\\b")






string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$drug_group!="Death"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"], collapse = "|"),")\\b")



CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat != "-") %>%
  mutate(Dead=ifelse(grepl(string_Dead, Treat), 1, 0)) %>%
  mutate(Hormone=ifelse(grepl(string_Hormonal, Treat), 1, 0)) %>%
  mutate(CDK=ifelse(grepl(string_CDK, Treat), 1, 0)) %>%
  mutate(OtherTarget=ifelse(grepl(string_OtherTarget, Treat), 1, 0)) %>%
  mutate(OtherCCh=ifelse(grepl(string_OtherChemo, Treat), 1, 0)) %>%
  mutate(EveroFul=ifelse(grepl(string_EveroFul, Treat), 1, 0)) %>%
  mutate(Niche=ifelse(grepl(string_Niche, Treat), 1, 0)) 


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Dead==1|Hormone==1|CDK==1|OtherTarget==1|OtherCCh==1|EveroFul==1|Niche==1)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(rank=
                                                      ifelse(Dead==1, 1,
                                                      ifelse(Niche==1, 2,
                                                      ifelse(EveroFul==1, 3,
                                                      ifelse(CDK==1,4, 
                                                      ifelse(OtherTarget==1,5,
                                                      ifelse(Hormone==1,6,
                                                      ifelse(OtherCCh==1,7,NA))))))))

temp <- CAN_Drug_Histories %>% select(patient, weight, Month2, rank)

temp <- temp %>% group_by(patient) %>% mutate(cumrank=cummin(rank))


groups <- fread("Source/Groups_HR_HER2_status.txt")
groups <- groups %>% filter(group=="HRPosHER2Neg") %>% select(patid)

data.frame(temp %>% 
             inner_join(groups, by=c("patient"="patid")) %>%
             select(patient, weight, cumrank) %>% distinct() %>%
  group_by(patient, weight) %>% mutate(cumrank=paste(cumrank, collapse = "+")) %>% distinct() %>%
  ungroup() %>%
  group_by(cumrank) %>% summarise(n=sum(weight)) %>%
  arrange(-n) %>% ungroup() %>% mutate(tot=sum(n), perc=100*n/tot)
  )


data.frame(temp %>% select(patient, weight, cumrank) %>% distinct() %>%
  group_by(patient, weight) %>% mutate(cumrank=paste(cumrank, collapse = "+")) %>% distinct() %>%
  ungroup() %>%
  group_by(cumrank) %>% summarise(n=sum(weight)) %>%
  arrange(-n) %>% ungroup() %>% mutate(tot=sum(n), perc=100*n/tot) %>%
    ungroup() %>%
    filter(grepl("5", cumrank)|grepl("4", cumrank)|grepl("3", cumrank)|grepl("2", cumrank)) %>% summarise(total=sum(n))
    )


# ---------------------

# Flows every 3 months Therapy Lines Breast Cancer With Rank 24m VIZ   ---------------------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
setDT(New_Primary_Cancer_Box)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer %in% c("Breast Cancer"), .(patid, Primary_Cancer)]

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics[ , .(patid, cancer_metastasis)]
PONS_Demographics <- PONS_Demographics[!is.na(cancer_metastasis)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics, by=c("patient"="patid")) %>% 
  select(-disease) %>% filter(Month>= (cancer_metastasis-3) )


CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month)



CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% mutate(Month2=Month-cancer_metastasis) %>% 
  select(-c(Month, cancer_metastasis))


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month2>=(-3)) %>% filter(Month2<=18) %>%
  group_by(patient) %>% count() %>%
  filter(n==22) %>% select(patient) %>%
  left_join(CAN_Drug_Histories)  %>% filter(Month2>=(-3)) %>% filter(Month2<=18)

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

unique(PONS_Ingredients_JN_ChemoClass$generic_name)

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

string_CDK <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Palbociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Ribociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Abemaciclib"], collapse = "|"),")\\b")


string_EveroFul <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Everolimus"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Fulvestrant"], collapse = "|"),")\\b")


string_Niche <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Alpelisib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Elacestrant"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Olaparib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Talazoparib"], collapse = "|"),")\\b")


string_Dead <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$drug_group=="Death"], collapse = "|"),")\\b")


string_OtherTarget <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[ (PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"|
                                                                                       PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic") &
                                                                                       PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"], collapse = "|"),")\\b")



string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$drug_group!="Death"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"], collapse = "|"),")\\b")


range(CAN_Drug_Histories$Month2)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Period=
                                                      ifelse(Month2<0,1,
                                                      ifelse(Month2<=6,2, 
                                                      ifelse(Month2<=12,3,
                                                      ifelse(Month2<=18,4,NA)))))

CAN_Drug_Histories <- CAN_Drug_Histories %>% # filter(Treat != "-") %>%
  mutate(Dead=ifelse(grepl(string_Dead, Treat), 1, 0)) %>%
  mutate(Hormone=ifelse(grepl(string_Hormonal, Treat), 1, 0)) %>%
  mutate(CDK=ifelse(grepl(string_CDK, Treat), 1, 0)) %>%
  mutate(OtherTarget=ifelse(grepl(string_OtherTarget, Treat), 1, 0)) %>%
  mutate(OtherCCh=ifelse(grepl(string_OtherChemo, Treat), 1, 0)) %>%
  mutate(EveroFul=ifelse(grepl(string_EveroFul, Treat), 1, 0)) %>%
  mutate(Niche=ifelse(grepl(string_Niche, Treat), 1, 0)) 

# CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Dead==1|Hormone==1|CDK==1|OtherTarget==1|OtherCCh==1|EveroFul==1|Niche==1)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(rank=
                                                      ifelse(Dead==1, 1,
                                                      ifelse(Niche==1, 2,
                                                      ifelse(EveroFul==1, 3,
                                                      ifelse(CDK==1,4, 
                                                      ifelse(OtherTarget==1,5,
                                                      ifelse(Hormone==1,6,
                                                      ifelse(OtherCCh==1,7,8))))))))

temp <- CAN_Drug_Histories %>% select(patient, weight, Month2, Period, rank)

temp <- temp %>% group_by(patient, Period) %>% mutate(cumrank=cummin(rank))


groups <- fread("Source/Groups_HR_HER2_status.txt")
groups <- groups %>% filter(group=="HRPosHER2Neg") %>% select(patid) %>% rename("patient"="patid")

# summary_df <- data.frame(
#   temp %>% 
#     inner_join(groups) %>%
#     select(patient, weight, Period, cumrank) %>% distinct() %>%
#     group_by(patient, weight, Period) %>% filter(cumrank==min(cumrank)) %>% ungroup() %>%
#     group_by(patient, weight) %>% mutate(cumrank=paste(cumrank, collapse = "+")) %>% distinct() %>%
#     ungroup() %>% 
#     group_by(cumrank) %>% summarise(n=sum(weight)) %>%
#     arrange(-n) %>% ungroup() %>% mutate(tot=sum(n), perc=100*n/tot)
#   )
# 
# 
# fwrite(summary_df, "Source/summary_df_BreastMets_Flows6month.csv")
# 
# summary_df <- summary_df %>% select(cumrank, n) 
# 
# split_columns <- str_split(summary_df$cumrank, "\\+")
# 
# split_columns_df <- as.data.frame(do.call(rbind, split_columns))
# 
# colnames(split_columns_df) <- c("m1", "m2", "m3", "m4")
# 
# summary_df <- cbind(summary_df, split_columns_df)
# 
# summary_df <- summary_df %>% select(-cumrank)


r_sank <- temp %>% 
    inner_join(groups) %>%
    select(patient, weight, Period, cumrank) %>% distinct() %>%
    group_by(patient, weight, Period) %>% filter(cumrank==min(cumrank)) %>% ungroup()



# r_sank <- r_sank %>% filter(cumrank!=8&cumrank!=1)

r_sank %>% group_by(Period) %>% summarise(n=sum(weight))

levels(r_sank$cumrank) <- rev(levels(r_sank$cumrank))


r_sank <- r_sank %>% 
  group_by(Period) %>% 
  mutate(pct = weight / sum(weight))



r_sank <- r_sank %>% mutate(cumrank=ifelse(cumrank==1, "1- Dead",
                                 ifelse(cumrank==2, "2- Niche",
                                        ifelse(cumrank==3,"3- EveroFul",
                                               ifelse(cumrank==4, "4- CDK",
                                                      ifelse(cumrank==5, "5- OtherTarget",
                                                             ifelse(cumrank==6, "6- Hormone",
                                                                    ifelse(cumrank==7, "7- OtherChemo", "8- none"))))))))


levels(r_sank$cumrank) <- rev(levels(as.factor(r_sank$cumrank)))


alluvial_colors <- rep("#F4FBFD", 8)  
# alluvial_colors[2] <- "firebrick"  
alluvial_colors[3] <- "#D9CF68"  
alluvial_colors[4] <- "#2B8FC8"  
alluvial_colors[6] <- "#9D3537"  
alluvial_colors[8] <- "#B3C385"  



r_sank %>% group_by(Period, cumrank) %>% summarise(n=sum(weight)) %>%
  spread(key=Period, value=n)

ggplot(r_sank,
       aes(x = Period, stratum = as.factor(cumrank), alluvium = patient,
           y = pct,
           fill = as.factor(cumrank), 
           # colour = as.factor(cumrank), 
           label = as.factor(cumrank) ) 
       ) +
  scale_x_discrete(expand = c(.1, .1)) +
  scale_y_continuous(label = scales::percent_format()) +
  geom_flow(width=0.6) +
  geom_stratum(width=0.6, alpha = .9) +
  theme(legend.position = "none") +
   theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
     panel.background = element_blank()
  ) +
  scale_fill_manual(values = alluvial_colors) + 
  scale_color_manual(values = alluvial_colors) +
  #ggsci::scale_color_futurama() +
 # ggsci::scale_fill_futurama() +
  geom_text(stat = "stratum", size = 3, aes(fontface = "bold")) +
  #geom_label(stat = "stratum", size = 3, aes(label = cumrank, fontface = "bold"),
  #           fill = "white", color = "black", alpha = 0.8, label.padding = unit(0.2, "lines")) +
  xlab("\n Post-metastasis\n 6-month evaluation") +
  ylab("Population \n")







# ---------------------



# Flows every 6 months before after mets Therapy Lines Breast Cancer With Rank 12m VIZ   ---------------------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
setDT(New_Primary_Cancer_Box)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer %in% c("Breast Cancer"), .(patid, Primary_Cancer)]

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics[ , .(patid, cancer_metastasis)]
PONS_Demographics <- PONS_Demographics[!is.na(cancer_metastasis)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics, by=c("patient"="patid")) %>% 
  select(-disease) %>% filter(Month>= (cancer_metastasis-6) )


CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% mutate(Month2=Month-cancer_metastasis) %>% 
  select(-c(Month, cancer_metastasis))


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month2>=(-6)) %>% filter(Month2<=6) %>%
  group_by(patient) %>% count() %>%
  filter(n==13) %>% select(patient) %>%
  left_join(CAN_Drug_Histories)  %>% filter(Month2>=(-6)) %>% filter(Month2<=6)

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

unique(PONS_Ingredients_JN_ChemoClass$generic_name)

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

string_CDK <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Palbociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Ribociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Abemaciclib"], collapse = "|"),")\\b")


string_EveroFul <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Everolimus"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Fulvestrant"], collapse = "|"),")\\b")


string_Niche <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Alpelisib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Elacestrant"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Olaparib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Talazoparib"], collapse = "|"),")\\b")


string_Dead <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$drug_group=="Death"], collapse = "|"),")\\b")


string_OtherTarget <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[ (PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"|
                                                                                       PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic") &
                                                                                       PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"], collapse = "|"),")\\b")



string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$drug_group!="Death"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"], collapse = "|"),")\\b")


range(CAN_Drug_Histories$Month2)
# 
# CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Period=
#                                                       ifelse(Month2<0,1,
#                                                       ifelse(Month2<=6,2, 
#                                                       ifelse(Month2<=12,3,
#                                                       ifelse(Month2<=18,4,NA)))))


CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Period=Month2)

CAN_Drug_Histories <- CAN_Drug_Histories %>% # filter(Treat != "-") %>%
  mutate(Dead=ifelse(grepl(string_Dead, Treat), 1, 0)) %>%
  mutate(Hormone=ifelse(grepl(string_Hormonal, Treat), 1, 0)) %>%
  mutate(CDK=ifelse(grepl(string_CDK, Treat), 1, 0)) %>%
  mutate(OtherTarget=ifelse(grepl(string_OtherTarget, Treat), 1, 0)) %>%
  mutate(OtherCCh=ifelse(grepl(string_OtherChemo, Treat), 1, 0)) %>%
  mutate(EveroFul=ifelse(grepl(string_EveroFul, Treat), 1, 0)) %>%
  mutate(Niche=ifelse(grepl(string_Niche, Treat), 1, 0)) 

# CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Dead==1|Hormone==1|CDK==1|OtherTarget==1|OtherCCh==1|EveroFul==1|Niche==1)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(rank=
                                                      ifelse(Dead==1, 1,
                                                      ifelse(Niche==1, 2,
                                                      ifelse(EveroFul==1, 3,
                                                      ifelse(CDK==1,4, 
                                                      ifelse(OtherTarget==1,5,
                                                      ifelse(Hormone==1,6,
                                                      ifelse(OtherCCh==1,7,8))))))))

temp <- CAN_Drug_Histories %>% select(patient, weight, Month2, Period, rank)

temp <- temp %>% group_by(patient, Period) %>% mutate(cumrank=cummin(rank))

groups <- fread("Source/Groups_HR_HER2_status.txt")
groups <- groups %>% filter(group=="HRPosHER2Neg") %>% select(patid) %>% rename("patient"="patid") 

temp2 <- temp %>% mutate(cumrank=paste0("S", cumrank)) %>% 
  inner_join(groups) %>%
  select(patient, Period, cumrank) %>% mutate(Period=paste0("M_", Period)) %>% ungroup() %>%
  spread(key=Period, value=cumrank) %>% select(patient, `M_-6`, `M_-5`, `M_-4`, `M_-3`,`M_-2`,`M_-1`,`M_0`,
                                               `M_1`,`M_2`,`M_3`,`M_4`,`M_5`,`M_6`)


fwrite(temp2, "temp_for_Sankey_HRPosHER2Neg.csv", sep=",")

# ----------
# Flows every 12 months before after mets Therapy Lines Breast Cancer With Rank 12m VIZ   ---------------------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
setDT(New_Primary_Cancer_Box)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer %in% c("Breast Cancer"), .(patid, Primary_Cancer)]

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics[ , .(patid, cancer_metastasis)]
PONS_Demographics <- PONS_Demographics[!is.na(cancer_metastasis)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics, by=c("patient"="patid")) %>% 
  select(-disease) %>% filter(Month>= (cancer_metastasis-12) )


CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% mutate(Month2=Month-cancer_metastasis) %>% 
  select(-c(Month, cancer_metastasis))


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month2>=(-12)) %>% filter(Month2<=12) %>%
  group_by(patient) %>% count() %>%
  filter(n==25) %>% select(patient) %>%
  left_join(CAN_Drug_Histories)  %>% filter(Month2>=(-12)) %>% filter(Month2<=12)

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

unique(PONS_Ingredients_JN_ChemoClass$generic_name)

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

string_CDK <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Palbociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Ribociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Abemaciclib"], collapse = "|"),")\\b")


string_EveroFul <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Everolimus"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Fulvestrant"], collapse = "|"),")\\b")


string_Niche <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Alpelisib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Elacestrant"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Olaparib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Talazoparib"], collapse = "|"),")\\b")


string_Dead <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$drug_group=="Death"], collapse = "|"),")\\b")


string_OtherTarget <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[ (PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"|
                                                                                       PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic") &
                                                                                       PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"], collapse = "|"),")\\b")



string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$drug_group!="Death"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"], collapse = "|"),")\\b")


range(CAN_Drug_Histories$Month2)
# 
# CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Period=
#                                                       ifelse(Month2<0,1,
#                                                       ifelse(Month2<=6,2, 
#                                                       ifelse(Month2<=12,3,
#                                                       ifelse(Month2<=18,4,NA)))))


CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Period=Month2)

CAN_Drug_Histories <- CAN_Drug_Histories %>% # filter(Treat != "-") %>%
  mutate(Dead=ifelse(grepl(string_Dead, Treat), 1, 0)) %>%
  mutate(Hormone=ifelse(grepl(string_Hormonal, Treat), 1, 0)) %>%
  mutate(CDK=ifelse(grepl(string_CDK, Treat), 1, 0)) %>%
  mutate(OtherTarget=ifelse(grepl(string_OtherTarget, Treat), 1, 0)) %>%
  mutate(OtherCCh=ifelse(grepl(string_OtherChemo, Treat), 1, 0)) %>%
  mutate(EveroFul=ifelse(grepl(string_EveroFul, Treat), 1, 0)) %>%
  mutate(Niche=ifelse(grepl(string_Niche, Treat), 1, 0)) 

# CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Dead==1|Hormone==1|CDK==1|OtherTarget==1|OtherCCh==1|EveroFul==1|Niche==1)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(rank=
                                                      ifelse(Dead==1, 1,
                                                      ifelse(Niche==1, 2,
                                                      ifelse(EveroFul==1, 3,
                                                      ifelse(CDK==1,4, 
                                                      ifelse(OtherTarget==1,5,
                                                      ifelse(Hormone==1,6,
                                                      ifelse(OtherCCh==1,7,8))))))))

temp <- CAN_Drug_Histories %>% select(patient, weight, Month2, Period, rank)

temp <- temp %>% group_by(patient, Period) %>% mutate(cumrank=cummin(rank))

groups <- fread("Source/Groups_HR_HER2_status.txt")
#groups <- groups %>% filter(group=="HRPosHER2Neg") %>% select(patid) %>% rename("patient"="patid") 
#groups <- groups %>% filter(group=="HRNegHER2Pos") %>% select(patid) %>% rename("patient"="patid") 
groups <- groups %>% filter(group=="HRPosHER2Pos") %>% select(patid) %>% rename("patient"="patid") 

temp2 <- temp %>% mutate(cumrank=paste0("S", cumrank)) %>% 
  inner_join(groups) %>%
  select(patient, Period, cumrank) %>% mutate(Period=paste0("M_", Period)) %>% ungroup() %>%
  spread(key=Period, value=cumrank) %>% 
  select(patient, `M_-12`, `M_-11`, `M_-10`, `M_-9`, `M_-8`, `M_-7`, `M_-6`, `M_-5`, `M_-4`, `M_-3`,`M_-2`,`M_-1`,`M_0`,
                                               `M_1`,`M_2`,`M_3`,`M_4`,`M_5`,`M_6`,`M_7`,`M_8`,`M_9`,`M_10`,`M_11`,`M_12`)


fwrite(temp2, "temp_for_Sankey_HRPosHER2Pos_12m.csv", sep=",")


# -----------
# Parse Neutrophiles data --------------

labs_Anemia_Neutropenia <- fread("Source/labs_Anemia_Neutropenia.txt")

unique(labs_Anemia_Neutropenia$rslt_unit_nm)

data.frame(labs_Anemia_Neutropenia %>% group_by(rslt_unit_nm) %>% count() %>% arrange(-n))

labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="%", "%",
                                  ifelse(rslt_unit_nm=="x10E3/uL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="x10E6/uL", "x10E6/uL",
                                                ifelse(rslt_unit_nm==" ", " ",
                                                       ifelse(rslt_unit_nm=="Million/uL", "x10E6/uL",
                                                              ifelse(rslt_unit_nm=="Thousand/uL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="K/MM3", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="cells/uL", "/uL",
                                                                                   ifelse(rslt_unit_nm=="M/MM3", "x10E6/uL",
                                                                                          ifelse(rslt_unit_nm=="10*3/uL", "x10E3/uL", NA)))))))))))

labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="x10/3 uL", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="X10E3/UL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="x10exp12/L", "x10E6/uL",
                                                ifelse(rslt_unit_nm=="K/uL", "x10E3/uL",
                                                       ifelse(rslt_unit_nm=="K/UL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="10*6/uL", "x10E6/uL",
                                                                     ifelse(rslt_unit_nm=="x10/6 uL", "x10E6/uL",
                                                                            ifelse(rslt_unit_nm=="x 10/3", "x10E3/uL",
                                                                                   ifelse(rslt_unit_nm=="K/CUMM", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="X10E6/UL", "x10E6/uL", rslt_unit_nm_V2)))))))))))
                                  


labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="K/mcL", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="M/U", "x10E6/uL",
                                         ifelse(rslt_unit_nm=="k/uL", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="M/u", "x10E6/uL",
                                                       ifelse(rslt_unit_nm=="THOUS/uL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="x10(3)/uL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="x10(6)/uL", "x10E6/uL",
                                                                            ifelse(rslt_unit_nm=="M/CUMM", "x10E6/uL",
                                                                                   ifelse(rslt_unit_nm=="MIL/uL", "x10E6/uL",
                                                                                          ifelse(rslt_unit_nm=="k/mm3", "x10E3/uL", rslt_unit_nm_V2)))))))))))
                                  

labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="m/mm3", "x10E6/uL",
                                  ifelse(rslt_unit_nm=="M/mcL", "x10E6/uL",
                                         ifelse(rslt_unit_nm=="x10e3/uL", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="THDS/CMM", "x10E3/uL",
                                                       ifelse(rslt_unit_nm=="MILL/CMM", "x10E6/uL",
                                                              ifelse(rslt_unit_nm=="10 6 uL", "x10E6/uL",
                                                                     ifelse(rslt_unit_nm=="Thous/mcL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="Mill/mcL", "x10E6/uL",
                                                                                   ifelse(rslt_unit_nm=="THO/uL", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="10E3/uL", "x10E3/uL", rslt_unit_nm_V2)))))))))))
                                  


labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="Cells/mcL", "/uL",
                                  ifelse(rslt_unit_nm=="k/cu mm", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="X10(3)/UL", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="X10(6)/UL", "x10E6/uL", 
                                                       ifelse(rslt_unit_nm=="10 3 uL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="k/ul", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="x10e6/uL", "x10E6/uL",
                                                                            ifelse(rslt_unit_nm=="m/uL", "x10E6/uL",
                                                                                   ifelse(rslt_unit_nm=="mill/cmm", "x10E6/uL",
                                                                                          ifelse(rslt_unit_nm=="10 3 ul", "x10E3/uL", rslt_unit_nm_V2)))))))))))
                                  


labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="thou/cmm", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="percent", "%",
                                         ifelse(rslt_unit_nm=="K/ul", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="10e6/uL", "x10E6/uL", 
                                                       ifelse(rslt_unit_nm=="x10", "x10",
                                                              ifelse(rslt_unit_nm=="Mill/uL", "x10E6/uL",
                                                                     ifelse(rslt_unit_nm=="k/mcL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="10E6/uL", "x10E6/uL",
                                                                                   ifelse(rslt_unit_nm=="THOU/uL", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="MILLION/uL", "x10E6/uL", rslt_unit_nm_V2)))))))))))
                                  

labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="m/cu mm", "x10E6/uL",
                                  ifelse(rslt_unit_nm=="Thous/mm3", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="10n3 ul", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="Percent", "%", 
                                                       ifelse(rslt_unit_nm=="Thou/uL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="bil/L", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="X10E12/L", "x10E6/uL",
                                                                            ifelse(rslt_unit_nm=="K/mm3", "x10E3/uL", 
                                                                                   ifelse(rslt_unit_nm=="X10E9/L", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="PERCENT", "%", rslt_unit_nm_V2)))))))))))
                                  

labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="tril/L", "x10E6/uL",
                                  ifelse(rslt_unit_nm=="Million/mcL", "x10E6/uL",
                                         ifelse(rslt_unit_nm=="X10", "x10",
                                                ifelse(rslt_unit_nm=="Cells/uL", "/uL", 
                                                       ifelse(rslt_unit_nm=="THOUS/MCL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="MILL/MCL", "x10E6/uL",
                                                                     ifelse(rslt_unit_nm=="10e3/uL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="CELLS/MCL", "/uL", 
                                                                                   ifelse(rslt_unit_nm=="m/mcL", "x10E6/uL",
                                                                                          ifelse(rslt_unit_nm=="M/mm3", "x10E6/uL", rslt_unit_nm_V2)))))))))))
                                  


labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="thous/uL", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="thou/mcL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="X10//S//3/uL", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="10(3)/uL", "x10E3/uL", 
                                                       ifelse(rslt_unit_nm=="K/cumm", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="{x10E3/uL}", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="Mil/mm3", "x10E6/uL",
                                                                            ifelse(rslt_unit_nm=="M/??L", "x10E6/uL", 
                                                                                   ifelse(rslt_unit_nm=="million/uL", "x10E6/uL",
                                                                                          ifelse(rslt_unit_nm=="M/ul", "x10E6/uL", rslt_unit_nm_V2)))))))))))
                                  




labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="x1000/??L", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="thousand/uL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="x 1000/??L", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="thous/mm3", "x10E3/uL", 
                                                       ifelse(rslt_unit_nm=="x10(9)/L", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="mil/mm3", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="M/cumm", "x10E6/uL",
                                                                            ifelse(rslt_unit_nm=="X 10*3/uL", "x10E3/uL", 
                                                                                   ifelse(rslt_unit_nm=="/uL", "/uL",
                                                                                          ifelse(rslt_unit_nm=="Thous/uL", "x10E3/uL", rslt_unit_nm_V2)))))))))))
                                  




labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="th/mm3", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="k/cumm", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="10(6)/uL", "x10E6/uL",
                                                ifelse(rslt_unit_nm=="10*6/mm3", "x10E6/uL", 
                                                       ifelse(rslt_unit_nm=="x10(12)/L", "x10E6/uL",
                                                              ifelse(rslt_unit_nm=="x10(3)/mcL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="10*3/mm3", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="{x10E6/uL}", "x10E6/uL", 
                                                                                   ifelse(rslt_unit_nm=="x10*3/uL", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="K/CMM", "x10E3/uL", rslt_unit_nm_V2)))))))))))
                                  


labs_Anemia_Neutropenia <- labs_Anemia_Neutropenia %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="Th/mm3", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="X10E3/uL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="x 1000/L", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="x10 3/uL", "x10E3/uL", 
                                                       ifelse(rslt_unit_nm=="x1000/L", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="M/CMM", "x10E6/uL",
                                                                     ifelse(rslt_unit_nm=="10//S//3/uL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="10//S//6/uL", "x10E6/uL", 
                                                                                   ifelse(rslt_unit_nm=="M/L", "x10E6/uL",
                                                                                          ifelse(rslt_unit_nm=="x10(6)/mcL", "x10E6/uL", rslt_unit_nm_V2))))))))))) # 128


labs_Anemia_Neutropenia %>% group_by(rslt_unit_nm_V2) %>% count() %>% arrange(-n) # leaving 370k behind

labs_Anemia_Neutropenia %>%
  filter( (rslt_unit_nm_V2=="x10E3/uL" & rslt_nbr!=0 & rslt_nbr<50) | 
            (rslt_unit_nm_V2=="x10E6/uL" & rslt_nbr!=0 ) ) %>%
 # mutate(rslt_nbr = ifelse(rslt_unit_nm_V2=="x10E6/uL", rslt_nbr/1000, rslt_nbr) ) %>%
  ggplot(aes(rslt_nbr)) + geom_density() + facet_wrap(~rslt_unit_nm_V2)

  

Labs_Neutrophiles <- labs_Anemia_Neutropenia %>%
  filter( (rslt_unit_nm_V2=="x10E3/uL" & rslt_nbr!=0 & rslt_nbr<100) ) %>%
  select(patid, fst_dt, rslt_nbr) %>% distinct()


fwrite(Labs_Neutrophiles, "Source/Labs_Neutrophiles.txt")






# ----------------------
# Parse eGFR ---------

labs_Breast_Cancer_pts <- fread("Source/labs_Breast_Cancer_pts.txt")

Anion_gap <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("10466-1","1863-0",
                                                  "33037-3","41276-7","47561-6","73578-7",
                                                  "73582-9","77341-6","95718-3"))

length(unique(Anion_gap$patid))


Bicarb <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("14627-4","19229-4",
                                                  "19232-8","19233-6","1959-6","1962-0",
                                                  "1963-8","69964-5","97543-3", "97544-1"))

length(unique(Bicarb$patid))


eGFR <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("69406-7","33914-3","48642-3",
                                                          "48643-1","50044-7","50210-4","50383-9",
                                                          "50384-7","62238-1","69405-9",
                                                          "70969-1","76633-7","77147-7",
                                                          "78006-4","88293-6","88294-4",
                                                          "94677-2","96591-3","96592-1",
                                                          "97952-6","98979-8","98980-6"))

length(unique(eGFR$patid))

data.frame(eGFR %>% group_by(rslt_unit_nm) %>% count() %>% arrange(-n))

eGFR <- eGFR %>% filter(rslt_nbr!=0)
eGFR <- eGFR %>% select(patid, fst_dt, rslt_nbr) %>% distinct()

fwrite(eGFR, "Source/eGFR.txt")

# ------------------
# Parse Bicarb  ---------


Bicarb <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("14627-4","19229-4",
                                                  "19232-8","19233-6","1959-6","1962-0",
                                                  "1963-8","69964-5","97543-3", "97544-1"))

length(unique(Bicarb$patid))
Bicarb <- Bicarb %>% filter(rslt_nbr!=0)
unique(Bicarb$rslt_unit_nm) 
Bicarb %>% group_by(rslt_unit_nm) %>% count() %>% arrange(-n)
range(Bicarb$rslt_nbr)

Bicarb <- Bicarb %>% select(patid, fst_dt, rslt_nbr) %>% distinct()

Bicarb %>% ggplot(aes(fst_dt, rslt_nbr)) + geom_smooth() 

fwrite(Bicarb, "Source/Bicarb.txt")



# ---------
# Parse Lymphocytes  ---------

labs_Anemia_Neutropenia <- fread("Source/labs_Anemia_Neutropenia.txt")

Lymphs <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("11130-2", "20585-6", "26474-7",
                                                            "30364-4",  "6744-7",  "731-0", "732-8"))

length(unique(Lymphs$patid))
Lymphs <- Lymphs %>% filter(rslt_nbr!=0)
unique(Lymphs$rslt_unit_nm) 
data.frame(Lymphs %>% group_by(rslt_unit_nm) %>% count() %>% arrange(-n))
range(Lymphs$rslt_nbr)



Lymphs <- Lymphs %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="K/MM3", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="x10E3/uL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="cells/uL", "/uL",
                                                ifelse(rslt_unit_nm==" ", " ",
                                                       ifelse(rslt_unit_nm=="X10E3/UL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="10*3/uL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="x10/3 uL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="x10exp9/L", "x10E3/uL",
                                                                                   ifelse(rslt_unit_nm=="K/uL", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="k/uL", "x10E3/uL", NA)))))))))))



Lymphs <- Lymphs %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="K/UL", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="K/CUMM", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="K/mcL", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="Cells/mcL", "/uL",
                                                       ifelse(rslt_unit_nm=="THOUS/uL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="thou/cmm", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="x10e3/uL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="Thou/uL", "x10E3/uL",
                                                                                   ifelse(rslt_unit_nm=="k/cu mm", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="k/cu mm", "x10E3/uL", rslt_unit_nm_V2)))))))))))


Lymphs <- Lymphs %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="10E3/uL", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="10 3 uL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="thou/mcL", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="X10E9/L", "x10E3/uL",
                                                       ifelse(rslt_unit_nm=="X10(3)/UL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="10e3/uL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="K/ul", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="x 1000/??L", "x10E3/uL",
                                                                                   ifelse(rslt_unit_nm=="10 3 ul", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="K/mm3", "x10E3/uL", rslt_unit_nm_V2)))))))))))



Lymphs <- Lymphs %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="10(3)/uL", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="k/mcL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="bil/L", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="THOU/uL", "x10E3/uL",
                                                       ifelse(rslt_unit_nm=="X 10*3/uL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="thous/uL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="x 1000/L", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="Thous/mm3", "x10E3/uL",
                                                                                   ifelse(rslt_unit_nm=="THO/uL", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="X10E3/uL", "x10E3/uL", rslt_unit_nm_V2)))))))))))


data.frame(Lymphs %>% group_by(rslt_unit_nm_V2) %>% count() %>% arrange(-n))

Lymphs <- Lymphs %>% mutate(rslt_nbr=ifelse(rslt_unit_nm_V2=="/uL", rslt_nbr/1000, rslt_nbr)) %>%
  mutate(rslt_unit_nm_V2=ifelse(rslt_unit_nm_V2=="/uL", "x10E3/uL", rslt_unit_nm_V2 ))

Lymphs %>% filter(!is.na(rslt_unit_nm_V2)) %>%
  select(rslt_nbr) %>% 
  ggplot(aes(rslt_nbr)) + geom_density() + xlim(0,10) 

Lymphs <- Lymphs %>% filter(!is.na(rslt_unit_nm_V2))  %>% filter(rslt_nbr<10 & rslt_nbr>0) %>%
  select(patid, fst_dt, rslt_nbr) %>% distinct()

fwrite(Lymphs, "Source/Lymphs.txt")



# ---------
# Parse Bilirub  ---------

labs_Anemia_Neutropenia <- fread("Source/labs_Anemia_Neutropenia.txt")

Bilirub <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("14629-0","14630-8","14631-6","15152-2",
                                                            "15153-0","18264-2","1968-7","1971-1",
                                                            "1975-2","22665-4","29760-6","33898-8",
                                                            "33899-6","34442-4","35191-6","35192-4",
                                                            "35672-5","42719-5","54363-7","59828-4",
                                                            "77137-8","89871-8"))

length(unique(Bilirub$patid))
Bilirub <- Bilirub %>% filter(rslt_nbr!=0)
unique(Bilirub$rslt_unit_nm) 
data.frame(Bilirub %>% group_by(rslt_unit_nm) %>% count() %>% arrange(-n))
range(Bilirub$rslt_nbr)
Bilirub <- Bilirub %>% filter(rslt_nbr<30)


Bilirub <- Bilirub %>% select(patid, fst_dt, rslt_nbr) %>% distinct()

fwrite(Bilirub, "Source/Bilirub.txt")

Bilirub %>% ggplot(aes(fst_dt, rslt_nbr)) + geom_smooth(method="lm")


# ---------
# Parse WBC  ---------

labs_Anemia_Neutropenia <- fread("Source/labs_Anemia_Neutropenia.txt")

WBC <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("12227-5","15190-2","26466-3","33028-2",
                                                             "30406-3","33765-9","49498-9","57845-0",
                                                             "6690-2","6743-9", "804-5"))

length(unique(WBC$patid))
WBC <- WBC %>% filter(rslt_nbr!=0)
unique(WBC$rslt_unit_nm) 
data.frame(WBC %>% group_by(rslt_unit_nm) %>% count() %>% arrange(-n))
range(Bilirub$rslt_nbr)

WBC <- WBC %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="K/MM3", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="x10E3/uL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="Thousand/uL", "/uL",
                                                ifelse(rslt_unit_nm==" ", " ",
                                                       ifelse(rslt_unit_nm=="X10E3/UL", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="10*3/uL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="x10/3 uL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="x 10/3", "x10E3/uL",
                                                                                   ifelse(rslt_unit_nm=="x10(3)/uL", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="K/CUMM", "x10E3/uL", NA)))))))))))



WBC <- WBC %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="K/uL", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="K/mcL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="K/mcL", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="k/mm3", "x10E3/u",
                                                       ifelse(rslt_unit_nm=="THDS/CMM", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="X10(3)/UL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="THOUS/uL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="Thous/mcL", "x10E3/uL",
                                                                                   ifelse(rslt_unit_nm=="k/ul", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="10 3 ul", "x10E3/uL", rslt_unit_nm_V2)))))))))))





WBC <- WBC %>%
  mutate(rslt_unit_nm_V2 = ifelse(rslt_unit_nm=="10E3/uL", "x10E3/uL",
                                  ifelse(rslt_unit_nm=="x10e3/uL", "x10E3/uL",
                                         ifelse(rslt_unit_nm=="k/cu mm", "x10E3/uL",
                                                ifelse(rslt_unit_nm=="THO/uL", "x10E3/u",
                                                       ifelse(rslt_unit_nm=="bil/L", "x10E3/uL",
                                                              ifelse(rslt_unit_nm=="k/uL", "x10E3/uL",
                                                                     ifelse(rslt_unit_nm=="THOUS/MCL", "x10E3/uL",
                                                                            ifelse(rslt_unit_nm=="10n3 ul", "x10E3/uL",
                                                                                   ifelse(rslt_unit_nm=="x1000/??L", "x10E3/uL",
                                                                                          ifelse(rslt_unit_nm=="K/ul", "x10E3/uL", rslt_unit_nm_V2)))))))))))




data.frame(WBC %>% group_by(rslt_unit_nm_V2) %>% count() %>% arrange(-n))


WBC %>% filter(rslt_unit_nm_V2=="x10E3/uL") %>%
  ggplot(aes(rslt_nbr)) + geom_density() + xlim(0,30)


WBC <- WBC %>% filter(rslt_unit_nm_V2=="x10E3/uL" & rslt_nbr<=30) %>%
  select(patid, fst_dt, rslt_nbr) %>% distinct()

fwrite(WBC, "Source/WBC.txt")


# ---------
# Parse AST ALT  ---------

labs_Anemia_Neutropenia <- fread("Source/labs_Anemia_Neutropenia.txt")

Liver <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("1742-6","1743-4","1744-2","1919-0",
                                                         "1920-8","25302-1","30239-8","48134-1",
                                                         "48136-6","54500-4","76625-3","77144-4", "88112-8"))


AST <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("1919-0","1920-8","30239-8",
                                                         "48136-6","54500-4", "88112-8"))

AST <- AST %>% filter(rslt_nbr!=0 & rslt_nbr<200)  %>% select(patid, fst_dt, rslt_nbr) %>% distinct() 


ALT <- labs_Breast_Cancer_pts %>% filter(loinc_cd %in% c("1742-6","1743-4","1744-2",
                                                         "25302-1","48134-1","76625-3","77144-4"))

ALT <- ALT %>% filter(rslt_nbr!=0 & rslt_nbr<200)  %>% select(patid, fst_dt, rslt_nbr) %>% distinct() 

fwrite(AST, "Source/AST.txt")
fwrite(ALT, "Source/ALT.txt")


# ---------
# Add NLP LABS --------------------------

labs_NLPMeas_Breast_Cancer_pts <- fread("Source/labs_NLPMeas_Breast_Cancer_pts.txt")

unique(labs_NLPMeas_Breast_Cancer_pts$measurement_type)

#  [1] "ABSOLUTE LYMPHOCYTE COUNT" "ABSOLUTE MONOCYTE COUNT"   "ABSOLUTE NEUTROPHIL COUNT" "ALKALINE PHOSPHATASE"     
#  [5] "ALP"                       "ALT"                       "ANION GAP"                 "AST"                      
#  [9] "BICARBONATE"               "BILIRUBIN"                 "BUN"                       "CALCIUM"                  
# [13] "CREATININE"                "EGFR"                      "FREE T4"                   "GFR"                      
# [17] "INR"                       "K"                         "K+"                        "LYMPHOCYTE"               
# [21] "MAGNESIUM"                 "MONOCYTES"                 "NEUTROPHILS"               "PLATELETS"                
# [25] "POTASSIUM"                 "POTASSIUM LEVEL"           "TOTAL BILIRUBIN"  

labs_NLPMeas_Breast_Cancer_pts <- labs_NLPMeas_Breast_Cancer_pts %>% filter(measurement_value!=0)

labs_NLPMeas_Breast_Cancer_pts <- labs_NLPMeas_Breast_Cancer_pts %>% mutate(measurement_value=as.numeric(measurement_value)) %>% drop_na()

labs_NLPMeas_Breast_Cancer_pts %>% group_by(measurement_detail) %>% count()


Lymphs <- fread("Source/Lymphs.txt")

Lymphs <- Lymphs %>%
  bind_rows(
    labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="ABSOLUTE LYMPHOCYTE COUNT"|measurement_type=="LYMPHOCYTE") %>%
  filter(measurement_value<10) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value")
  ) %>% distinct()

fwrite(Lymphs, "Source/Lymphs.txt")




Labs_Neutrophiles <- fread("Source/Labs_Neutrophiles.txt")

range(Labs_Neutrophiles$rslt_nbr)

Labs_Neutrophiles <- Labs_Neutrophiles %>% filter(rslt_nbr<30) %>%
  bind_rows(
    labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="ABSOLUTE NEUTROPHIL COUNT"|measurement_type=="NEUTROPHILS") %>%
  filter(measurement_value<30) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value")
  ) %>% distinct()

fwrite(Labs_Neutrophiles, "Source/Labs_Neutrophiles.txt")





eGFR <- fread("Source/eGFR.txt")

range(eGFR$rslt_nbr)
eGFR <- eGFR %>% filter(rslt_nbr>10&rslt_nbr<150)

eGFR <- eGFR %>% 
  bind_rows(
    labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="GFR"|measurement_type=="EGFR") %>%
  filter(measurement_value<150&measurement_value>10) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value")
  ) %>% distinct()

fwrite(eGFR, "Source/eGFR.txt")



AST <- fread("Source/AST.txt")

range(AST$rslt_nbr)
AST <- AST %>% filter(rslt_nbr>0&rslt_nbr<200)

AST <- AST %>% 
  bind_rows(
    labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="AST") %>%
  filter(measurement_value<200&measurement_value>0) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value")
  ) %>% distinct()

fwrite(AST, "Source/AST.txt")




ALT <- fread("Source/ALT.txt")

range(ALT$rslt_nbr)
ALT <- ALT %>% filter(rslt_nbr>0&rslt_nbr<200)

ALT <- ALT %>% 
  bind_rows(
    labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="ALT") %>%
  filter(measurement_value<200&measurement_value>0) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value")
  ) %>% distinct()

fwrite(ALT, "Source/ALT.txt")



Bicarb <- fread("Source/Bicarb.txt")

range(Bicarb$rslt_nbr)
Bicarb <- Bicarb %>% filter(rslt_nbr>0&rslt_nbr<50)

Bicarb <- Bicarb %>% 
  bind_rows(
    labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="BICARBONATE") %>%
  filter(measurement_value<50&measurement_value>0) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value")
  ) %>% distinct()

fwrite(Bicarb, "Source/Bicarb.txt")




Bilirub <- fread("Source/Bilirub.txt")

range(Bilirub$rslt_nbr)
Bilirub <- Bilirub %>% filter(rslt_nbr>0&rslt_nbr<30)

Bilirub <- Bilirub %>% 
  bind_rows(
    labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="BILIRUBIN") %>%
  filter(measurement_value<30&measurement_value>0) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value")
  ) %>% distinct()

fwrite(Bicarb, "Source/Bilirub.txt")





ALP <- labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="ALKALINE PHOSPHATASE") %>%
  filter(measurement_value<500&measurement_value>0) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value") %>% distinct()

fwrite(ALP, "Source/ALP.txt")






PLATELETS <- labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="PLATELETS") %>%
  filter(measurement_value<1000&measurement_value>0) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value") %>% distinct()

fwrite(PLATELETS, "Source/PLATELETS.txt")





CALCIUM <- labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="CALCIUM") %>%
  filter(measurement_value<15&measurement_value>5) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value") %>% distinct()

fwrite(CALCIUM, "Source/CALCIUM.txt")


POTASSIUM <- labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="POTASSIUM"|measurement_type=="K"|measurement_type=="K+") %>%
  filter(measurement_value<10&measurement_value>1) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value") %>% distinct()

fwrite(POTASSIUM, "Source/POTASSIUM.txt")




ANIONGAP <- labs_NLPMeas_Breast_Cancer_pts %>%
  filter(measurement_type=="ANION GAP") %>%
  filter(measurement_value<30&measurement_value>1) %>%
  select(patid, note_date, measurement_value) %>%
  distinct() %>% rename("fst_dt"="note_date", "rslt_nbr"="measurement_value") %>% distinct()

fwrite(ANIONGAP, "Source/ANIONGAP.txt")

# --------------------



# Biomarkers vs persitency ON Palbociclib MIXED MODELS  --------------
CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_MeasuresLabs <- fread("Source/PONS Measures Labs.txt")
unique(PONS_MeasuresLabs$test)
PONS_MeasuresLabs <- New_Primary_Cancer_Box %>% inner_join(PONS_MeasuresLabs)
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% filter(test != "Cancer Stage" & test != "Body Height")
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% select(patid, test, unit, claimed, value) %>% distinct()
PONS_MeasuresLabs[, claimed := as.character(claimed)][, claimed := substr(claimed, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_MeasuresLabs <- PONS_MeasuresLabs[Months_lookup, on = c("claimed" = "Month")]
#PONS_MeasuresLabs <- PONS_MeasuresLabs %>% filter(test=="Hemoglobin") %>% select(patid, value, Exact_Month)
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% group_by(patid, test, Exact_Month) %>% summarise(value=mean(value)) %>% ungroup()


CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

Palbociclib_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()

CAN_Drug_Histories <- Palbociclib_pats %>% inner_join(CAN_Drug_Histories) 
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(
  CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% 
  group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, Month) %>% rename("Min"="Month")
)  %>% mutate(Elapsed=Month-Min)
  
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month, Elapsed, ON_Palbo) %>% 
  left_join(PONS_MeasuresLabs, by=c("patient"="patid", "Month"="Exact_Month"))


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(test!="Body Weight")



CAN_Drug_Histories %>% 
  filter(Elapsed>=(0)) %>%
    filter( (test=="Albumin"&value>=2&value<=6) |  
             (test=="BMI"&value>=15&value<=50) |  
             (test=="C-Reactive Protein"&value>=0&value<=200) | 
             (test=="Hemoglobin"&value>=7&value<=15)) %>%
  ggplot(aes(value, ON_Palbo, colour=test, fill=test )) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="black") +
  stat_smooth(method="gam", alpha=0.3, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =8))+
  facet_wrap(~test, scales = "free") +
  xlab("\n Lab Result") + ylab("Probability / Proporiton of \n STILL being ON Palbo\n ") +
  theme_minimal() +
  scale_colour_manual(values=c("#2986cc", "#16537e", "#a64d79", "#c90076" )) +
  scale_fill_manual(values=c("#2986cc", "#16537e", "#a64d79", "#c90076" ))




CAN_Drug_Histories %>% 
  filter(Elapsed>=(0)) %>%
  filter( (test=="Albumin"&value>=2&value<=6) |  
             (test=="BMI"&value>=15&value<=50) |  
             (test=="C-Reactive Protein"&value>=0&value<=200) | 
             (test=="Hemoglobin"&value>=7&value<=15)) %>%
  ggplot(aes(value, colour=test, fill=test )) +
  #stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="black") +
  #stat_smooth(method="gam", alpha=0.3, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =8))+
  geom_density(alpha=0.5) +
  facet_wrap(~test, scales = "free") +
  xlab("\n Lab Result") + ylab("Density of records \n ") +
  theme_minimal() +
  scale_colour_manual(values=c("#2986cc", "#16537e", "#a64d79", "#c90076" )) +
  scale_fill_manual(values=c("#2986cc", "#16537e", "#a64d79", "#c90076" ))




# Rela to baseline


CAN_Drug_Histories %>% 
  filter(Elapsed>=(0)) %>%
    filter( (test=="Albumin"&value>=2&value<=6) |  
             (test=="BMI"&value>=15&value<=50) |  
             (test=="C-Reactive Protein"&value>=0&value<=200) | 
             (test=="Hemoglobin"&value>=7&value<=15)) %>%
  group_by(patient, test) %>% filter(Month==min(Month)) %>% select(patient, test, Month, value) %>% rename("First"="Month", "Start"="value") %>%
  left_join(CAN_Drug_Histories) %>%
  filter(Elapsed>=(0)) %>%
    filter( (test=="Albumin"&value>=2&value<=6) |  
             (test=="BMI"&value>=15&value<=50) |  
             (test=="C-Reactive Protein"&value>=0&value<=200) | 
             (test=="Hemoglobin"&value>=7&value<=15)) %>%
  mutate(value=100*(value-Start)/Start) %>%
  ggplot(aes(value, ON_Palbo, colour=test, fill=test )) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="black") +
  #stat_smooth(method="gam", alpha=0.3, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =3))+
  facet_wrap(~test, scales = "free") +
  xlab("\n Lab Result \n RELATIVE TO START Value \n At the time OF Palbociclib initiation (%)") + ylab("Probability / Proporiton of \n STILL being ON Palbo\n ") +
  theme_minimal() +
  scale_colour_manual(values=c("#2986cc", "#16537e", "#a64d79", "#c90076" )) +
  scale_fill_manual(values=c("#2986cc", "#16537e", "#a64d79", "#c90076" ))





CAN_Drug_Histories %>% 
  filter(Elapsed>=(0)) %>%
    filter( (test=="Albumin"&value>=2&value<=6) |  
             (test=="BMI"&value>=15&value<=50) |  
             (test=="C-Reactive Protein"&value>=0&value<=200) | 
             (test=="Hemoglobin"&value>=7&value<=15)) %>%
  group_by(patient, test) %>% filter(Month==min(Month)) %>% select(patient, test, Month, value) %>% rename("First"="Month", "Start"="value") %>%
  left_join(CAN_Drug_Histories) %>%
  filter(Elapsed>=(0)) %>%
    filter( (test=="Albumin"&value>=2&value<=6) |  
             (test=="BMI"&value>=15&value<=50) |  
             (test=="C-Reactive Protein"&value>=0&value<=200) | 
             (test=="Hemoglobin"&value>=7&value<=15)) %>%
  mutate(value=100*(value-Start)/Start) %>%
  ggplot(aes(value, colour=test, fill=test )) +
  #stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="black") +
  #stat_smooth(method="gam", alpha=0.3, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =8))+
  geom_density(alpha=0.5) +
  facet_wrap(~test, scales = "free") +
  xlab("\n Lab Result") + ylab("Density of records \n ") +
  theme_minimal() +
  scale_colour_manual(values=c("#2986cc", "#16537e", "#a64d79", "#c90076" )) +
  scale_fill_manual(values=c("#2986cc", "#16537e", "#a64d79", "#c90076" ))




temp <- CAN_Drug_Histories %>% 
  filter(Elapsed>=(0))  %>%
  select(patient, Elapsed, ON_Palbo) %>% distinct() %>%
  left_join(
    CAN_Drug_Histories %>% 
  filter(Elapsed>=(0)) %>%
    filter( (test=="Albumin"&value>=2&value<=6) |  
             (test=="BMI"&value>=15&value<=50) |  
             (test=="C-Reactive Protein"&value>=0&value<=200) | 
             (test=="Hemoglobin"&value>=7&value<=15) | 
              is.na(test)) %>% spread(key=test, value=value) %>%
  select(patient, Elapsed, Albumin, BMI, `C-Reactive Protein`, Hemoglobin) %>% 
    distinct()
  ) 

fwrite(temp, "Source/AfterPalbo_Month_Over_Month_Markers.txt")
temp <- fread("Source/AfterPalbo_Month_Over_Month_Markers.txt")

library(lme4)


model_All <- glmer(ON_Palbo ~ 
                 Elapsed  + Albumin  + BMI + Hemoglobin + `C-Reactive Protein`  + (1 |patient)  , 
                  data = temp, family = binomial)

summary(model_All)




model_Elapsed <- glmer(ON_Palbo ~ 
                 Elapsed  +  (Elapsed |patient)  , 
                  data = temp, family = binomial)

summary(model_Elapsed)



model_Alb <- glmer(ON_Palbo ~ 
                 Elapsed + Albumin +  (Albumin |patient)  , 
                  data = temp, family = binomial)

summary(model_Alb)



model_BMI <- glmer(ON_Palbo ~ 
                 Elapsed + BMI + (BMI |patient)  , 
                  data = temp, family = binomial)

summary(model_BMI)


model_Hemoglobin <- glmer(ON_Palbo ~ 
                 Elapsed + Hemoglobin + (Hemoglobin |patient)  , 
                  data = temp, family = binomial)

summary(model_Hemoglobin)



model_CRP <- glmer(ON_Palbo ~ 
                 Elapsed + `C-Reactive Protein` + (1|patient)  , 
                  data = temp, family = binomial)

summary(model_CRP)


# TODO
# https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/




to_predict <- temp %>% filter(!is.na(Albumin)) %>% 
  select(patient, Elapsed, Albumin, ON_Palbo) %>% distinct()

to_predict %>% bind_cols(data.frame(predict(model_Alb, data=to_predict, type="response"))) %>%
  ggplot(aes(Albumin, `predict.model_Alb..data...to_predict..type....response..`)) +
  geom_smooth(colour="black", fill="black") +
  theme_minimal() +
  xlab("\n Albumin level") + 
      coord_cartesian(ylim=c(0,1)) +
  ylab("Probability of \n STILL being ON Palbociclib \n")




to_predict <- temp %>% filter(!is.na(BMI)) %>% 
  select(patient, Elapsed, BMI, ON_Palbo) %>% distinct()

to_predict %>% bind_cols(data.frame(predict(model_BMI, data=to_predict, type="response"))) %>%
  ggplot(aes(BMI, `predict.model_BMI..data...to_predict..type....response..`)) +
  geom_smooth(colour="black", fill="black") +
  theme_minimal() +
  xlab("\n BMI level") + 
    coord_cartesian(ylim=c(0,1)) +
  ylab("Probability of \n STILL being ON Palbociclib \n")




to_predict <- temp %>% filter(!is.na(Hemoglobin)) %>% 
  select(patient, Elapsed, Hemoglobin, ON_Palbo) %>% distinct()

to_predict %>% bind_cols(data.frame(predict(model_Hemoglobin, data=to_predict, type="response"))) %>%
  ggplot(aes(Hemoglobin, `predict.model_Hemoglobin..data...to_predict..type....response..`)) +
  geom_smooth(colour="black", fill="black") +
  theme_minimal() +
  xlab("\n Hemoglobin level") + 
  coord_cartesian(ylim=c(0,1)) +
  ylab("Probability of \n STILL being ON Palbociclib \n")




to_predict <- temp %>% filter(!is.na(`C-Reactive Protein`)) %>% 
  select(patient, Elapsed, `C-Reactive Protein`, ON_Palbo) %>% distinct()

to_predict %>% bind_cols(data.frame(predict(model_CRP, data=to_predict, type="response"))) %>%
  ggplot(aes(`C-Reactive Protein`, `predict.model_CRP..data...to_predict..type....response..`)) +
  geom_smooth(colour="black", fill="black") +
  theme_minimal() +
  xlab("\n CRP level") + 
  coord_cartesian(ylim=c(0,1)) +
  ylab("Probability of \n STILL being ON Palbociclib \n")




to_predict <- temp %>% filter(!is.na(Elapsed)) %>% 
  select(patient, Elapsed, ON_Palbo) %>% distinct()

to_predict %>% bind_cols(data.frame(predict(model_Elapsed, data=to_predict, type="response"))) %>%
  ggplot(aes(Elapsed, `predict.model_Elapsed..data...to_predict..type....response..`)) +
  geom_smooth(colour="black", fill="black") +
  theme_minimal() +
  xlab("\n Number of Elapsed Months \n Since Palbociclib Initiation") + 
  coord_cartesian(ylim=c(0,1)) +
  ylab("Probability of \n STILL being ON Palbociclib \n")




Labs_Neutrophiles <- fread("Source/Labs_Neutrophiles.txt")

Labs_Neutrophiles[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Labs_Neutrophiles <- Labs_Neutrophiles %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Labs_Neutrophiles <- Labs_Neutrophiles %>% ungroup()

Labs_Neutrophiles <- setDT(Labs_Neutrophiles)[Months_lookup, on = c("fst_dt" = "Month")]

temp <- temp %>% select(-Neutrophiles)

Labs_Neutrophiles %>% inner_join(temp %>% select(patient) %>% distinct(), by=c("patid"="patient")) %>%
  ggplot(aes(Exact_Month, rslt_nbr)) + geom_smooth()

# get temp model from before with the other tests, see above

temp %>% left_join(Labs_Neutrophiles %>% select(-fst_dt), by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
   ggplot(aes(rslt_nbr, ON_Palbo)) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="black") +
  xlab("\n Lab Result") + ylab("Probability / Proporiton of \n STILL being ON Palbo\n ") +
  theme_minimal() 

temp <- temp %>% left_join(Labs_Neutrophiles %>% select(-fst_dt), by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  rename("Neutrophiles"="rslt_nbr")




eGFR <- fread("Source/eGFR.txt")

eGFR[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
eGFR <- eGFR %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
eGFR <- eGFR %>% ungroup()
eGFR <- eGFR %>% filter(rslt_nbr>10&rslt_nbr<150)

eGFR <- setDT(eGFR)[Months_lookup, on = c("fst_dt" = "Month")]


eGFR %>% inner_join(tem %>% select(patient) %>% distinct(), by=c("patid"="patient")) %>%
  ggplot(aes(Exact_Month, rslt_nbr)) + geom_smooth()

# get temp model from before with the other tests, see above

temp %>% left_join(eGFR %>% select(-fst_dt), by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
   ggplot(aes(rslt_nbr, ON_Palbo)) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="black") +
  xlab("\n Lab Result") + ylab("Probability / Proporiton of \n STILL being ON Palbo\n ") +
  theme_minimal() 

temp <- temp %>% left_join(eGFR %>% select(-fst_dt), by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  rename("eGFR"="rslt_nbr")




model_eGFR <- glmer(ON_Palbo ~ 
                 Elapsed + eGFR + (eGFR|patient)  , 
                  data = temp, family = binomial)

summary(model_eGFR)

to_predict <- temp %>% filter(!is.na(eGFR)) %>% 
  select(patient, Elapsed, eGFR, ON_Palbo) %>% distinct()

to_predict %>% bind_cols(data.frame(predict(model_eGFR, data=to_predict, type="response"))) %>%
  ggplot(aes(eGFR, `predict.model_eGFR..data...to_predict..type....response..`)) +
  geom_smooth(colour="black", fill="black", method="lm") +
  theme_minimal() +
  xlab("\n eGFR level") + 
  coord_cartesian(ylim=c(0,1)) +
  ylab("Probability of \n STILL being ON Palbociclib \n")





# ---------

# Albumin by buckets ---------------

temp2 <- temp %>% filter(!is.na(Albumin)) %>% select(patient) %>% distinct() %>%
  sample_n(100) %>% left_join(temp) %>% 
  mutate(Albumin2=ifelse(Albumin<=3,"<3", 
                                ifelse(Albumin<=4,"<4",
                                       ifelse(Albumin<=5,"<5", ">5")))) %>%
  select(patient, Elapsed, ON_Palbo, Albumin2) %>% mutate(Albumin2=as.factor(Albumin2))

temp2 <- temp %>% mutate(Albumin2=round(Albumin)) %>%
  select(patient, Elapsed, ON_Palbo, Albumin2) %>% mutate(Albumin2=as.factor(Albumin2))

model_Alb2 <- glmer(ON_Palbo ~ 
                 Elapsed + Albumin2 +  (Albumin2 |patient)  , 
                  data = temp2, family = binomial)

summary(model_Alb2)


to_predict <- temp2 %>% filter(!is.na(Albumin2)) %>% 
  select(patient, Elapsed, Albumin2, ON_Palbo) %>% distinct()

to_predict %>% bind_cols(data.frame(predict(model_Alb2, data=to_predict, type="response"))) %>%
  ggplot(aes(Elapsed, `predict.model_Alb2..data...to_predict..type....response..`, 
             colour=Albumin2 , fill=Albumin2 ) ) +
  geom_smooth() +
  theme_minimal() +
  scale_colour_manual(values=c("#003366","#1A5276","#3498DB","#5DADE2","#AED6F1")) +
  scale_fill_manual(values=c("#003366","#1A5276","#3498DB","#5DADE2","#AED6F1")) +
  xlab("\n Number of Elapsed Months \n Since Palbociclib Initiation") + 
  coord_cartesian(ylim=c(0,1)) +
  ylab("Probability of \n STILL being ON Palbociclib \n")

  
plot(random.effects(model_Alb))
  

# -----------------------
# Add all markers together ---------

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )


temp <- fread("Source/AfterPalbo_Month_Over_Month_Markers.txt")


AST <- fread("Source/AST.txt")
ALT <- fread("Source/ALT.txt")
WBC <- fread("Source/WBC.txt")
Bilirub <- fread("Source/Bilirub.txt")
Lymphs <- fread("Source/Lymphs.txt")
Bicarb <- fread("Source/Bicarb.txt")
eGFR <- fread("Source/eGFR.txt")
Labs_Neutrophiles <- fread("Source/Labs_Neutrophiles.txt")
ANIONGAP <- fread("Source/ANIONGAP.txt")
POTASSIUM <- fread("Source/POTASSIUM.txt")
CALCIUM <- fread("Source/CALCIUM.txt")
PLATELETS <- fread("Source/PLATELETS.txt")
ALP <- fread("Source/ALP.txt")




ANIONGAP[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ANIONGAP <- ANIONGAP %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ANIONGAP <- ANIONGAP %>% ungroup()
ANIONGAP <- setDT(ANIONGAP)[Months_lookup, on = c("fst_dt" = "Month")]
ANIONGAP <- ANIONGAP %>% select(-fst_dt)
ANIONGAP <- ANIONGAP %>% rename("ANIONGAP"="rslt_nbr")


POTASSIUM[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
POTASSIUM <- POTASSIUM %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
POTASSIUM <- POTASSIUM %>% ungroup()
POTASSIUM <- setDT(POTASSIUM)[Months_lookup, on = c("fst_dt" = "Month")]
POTASSIUM <- POTASSIUM %>% select(-fst_dt)
POTASSIUM <- POTASSIUM %>% rename("POTASSIUM"="rslt_nbr")


CALCIUM[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
CALCIUM <- CALCIUM %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
CALCIUM <- CALCIUM %>% ungroup()
CALCIUM <- setDT(CALCIUM)[Months_lookup, on = c("fst_dt" = "Month")]
CALCIUM <- CALCIUM %>% select(-fst_dt)
CALCIUM <- CALCIUM %>% rename("CALCIUM"="rslt_nbr")


PLATELETS[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
PLATELETS <- PLATELETS %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
PLATELETS <- PLATELETS %>% ungroup()
PLATELETS <- setDT(PLATELETS)[Months_lookup, on = c("fst_dt" = "Month")]
PLATELETS <- PLATELETS %>% select(-fst_dt)
PLATELETS <- PLATELETS %>% rename("PLATELETS"="rslt_nbr")

ALP[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ALP <- ALP %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ALP <- ALP %>% ungroup()
ALP <- setDT(ALP)[Months_lookup, on = c("fst_dt" = "Month")]
ALP <- ALP %>% select(-fst_dt)
ALP <- ALP %>% rename("ALP"="rslt_nbr")


AST[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
AST <- AST %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
AST <- AST %>% ungroup()
AST <- setDT(AST)[Months_lookup, on = c("fst_dt" = "Month")]
AST <- AST %>% select(-fst_dt)
AST <- AST %>% rename("AST"="rslt_nbr")


ALT[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ALT <- ALT %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ALT <- ALT %>% ungroup()
ALT <- setDT(ALT)[Months_lookup, on = c("fst_dt" = "Month")]
ALT <- ALT %>% select(-fst_dt)
ALT <- ALT %>% rename("ALT"="rslt_nbr")


WBC[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
WBC <- WBC %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
WBC <- WBC %>% ungroup()
WBC <- setDT(WBC)[Months_lookup, on = c("fst_dt" = "Month")]
WBC <- WBC %>% select(-fst_dt)
WBC <- WBC %>% rename("WBC"="rslt_nbr")



Bilirub[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Bilirub <- Bilirub %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Bilirub <- Bilirub %>% ungroup()
Bilirub <- setDT(Bilirub)[Months_lookup, on = c("fst_dt" = "Month")]
Bilirub <- Bilirub %>% select(-fst_dt)
Bilirub <- Bilirub %>% rename("Bilirub"="rslt_nbr")



Lymphs[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Lymphs <- Lymphs %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Lymphs <- Lymphs %>% ungroup()
Lymphs <- setDT(Lymphs)[Months_lookup, on = c("fst_dt" = "Month")]
Lymphs <- Lymphs %>% select(-fst_dt)
Lymphs <- Lymphs %>% rename("Lymphs"="rslt_nbr")



Bicarb[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Bicarb <- Bicarb %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Bicarb <- Bicarb %>% ungroup()
Bicarb <- setDT(Bicarb)[Months_lookup, on = c("fst_dt" = "Month")]
Bicarb <- Bicarb %>% select(-fst_dt)
Bicarb <- Bicarb %>% rename("Bicarb"="rslt_nbr")


eGFR[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
eGFR <- eGFR %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
eGFR <- eGFR %>% ungroup()
eGFR <- eGFR %>% filter(rslt_nbr>10&rslt_nbr<150)
eGFR <- setDT(eGFR)[Months_lookup, on = c("fst_dt" = "Month")]
eGFR <- eGFR %>% select(-fst_dt)
eGFR <- eGFR %>% rename("eGFR"="rslt_nbr")


Labs_Neutrophiles[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Labs_Neutrophiles <- Labs_Neutrophiles %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Labs_Neutrophiles <- Labs_Neutrophiles %>% ungroup()
Labs_Neutrophiles <- setDT(Labs_Neutrophiles)[Months_lookup, on = c("fst_dt" = "Month")]
Labs_Neutrophiles <- Labs_Neutrophiles %>% select(-fst_dt)
Labs_Neutrophiles <- Labs_Neutrophiles %>% rename("Neutrophiles"="rslt_nbr")



temp <- temp %>% 
  left_join(AST, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(ALT, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(WBC, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Bilirub, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Lymphs, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Bicarb, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(eGFR, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Labs_Neutrophiles, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(ALP, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(CALCIUM, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(POTASSIUM, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(PLATELETS, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(ANIONGAP, by=c("patient"="patid", "Elapsed"="Exact_Month"))  

temp %>%
  gather(Test, Result, Albumin:ANIONGAP) %>%
  filter(Test!="Bicarb"&Test!="Bilirub"&Test!="WBC"&Test!="PLATELETS"&Test!="eGFR") %>%
  mutate(Test=ifelse(Test=="Albumin", "'1- Albumin",
                     ifelse(Test=="ANIONGAP", "'2- ANIONGAP",
                            ifelse(Test=="BMI", "'3- BMI",
                                   ifelse(Test=="CALCIUM", "'4- CALCIUM",
                                          ifelse(Test=="Hemoglobin", "'5- Hemoglocin",
                                                 ifelse(Test=="Lymphs", "'6- Lymphs",
                                                        ifelse(Test=="POTASSIUM", "'7- POTASSIUM",
                                                               ifelse(Test=="ALP", "'8- ALP",
                                                                      ifelse(Test=="ALT", "'9- ALT",
                                                                             ifelse(Test=="AST", "10- AST",
                                                                                    ifelse(Test=="C-Reactive Protein", "11- C-Reactive Protein",
                                                                                           ifelse(Test=="Neutrophiles", "12- Neutrophiles", NA))))))))))))) %>%
  ggplot(aes(Result, ON_Palbo, colour=Test, fill=Test))+ 
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial), colour="black", fill="black") +
  facet_wrap(~Test, scales="free_x") +
  theme_minimal() +
  xlab("\n Test result") +
  ylab("Probability of \n STILL being ON Palbociblib \n")


library(lme4)
 

model_All <- glmer(ON_Palbo ~  Elapsed  + Albumin  + BMI + Hemoglobin + AST + eGFR + Neutrophiles + (1 |patient)  , data = temp, family = binomial)

summary(model_All)

temp2 <- temp %>% filter(!is.na(Albumin) & !is.na(BMI) & !is.na(Hemoglobin) & !is.na(AST) & !is.na(eGFR) & !is.na(Neutrophiles))

temp2 <- temp2 %>% bind_cols(
  data.frame(predict(model_All, data=temp2, type="response")) %>% 
  rename("pred"="predict.model_All..data...temp2..type....response..")
)


temp2 %>% ggplot(aes(Albumin, pred)) + geom_point() + geom_smooth() +
        coord_cartesian(ylim=c(0,1))  + geom_line(aes(group=patient))


# ---------

# Therapy in bone cancer patients V2, 2+ Dxs --------------

PONS_Events <- fread("Source/PONS Events.txt")

PONS_Events <- PONS_Events %>% filter(grepl("C795", code)) %>% 
  group_by(patid) %>% count() %>% filter(n>1) %>% 
  select(patid) %>% distinct() %>% ungroup() %>% left_join(PONS_Events) %>%
  group_by(patid) %>%
  filter(grepl("C795", code)) %>% filter(claimed==min(claimed)) %>% select(patid, claimed) %>% distinct()

PONS_Events$claimed <- as.Date(PONS_Events$claimed)

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[New_Primary_Cancer_Box$Primary_Cancer!="-"]
sum(New_Primary_Cancer_Box$weight) # 22984889

PONS_Events %>% left_join(New_Primary_Cancer_Box) %>% summarise(n=sum(weight, na.rm=T)) # 1518477 or # 1285656

PONS_Events <- PONS_Events %>% left_join(New_Primary_Cancer_Box)


Zoledronic_Acid_procedures <- fread("Source/Zoledronic_Acid_procedures.txt")
Zoledronic_Acid_procedures <- Zoledronic_Acid_procedures %>% select(patid) %>% distinct()
Zoledronic_Acid_rxs <- fread("Source/Zoledronic_Acid_rxs.txt")
Zoledronic_Acid_rxs <- Zoledronic_Acid_rxs %>% select(patid) %>% distinct()
Zoledronic <- Zoledronic_Acid_procedures %>% full_join(Zoledronic_Acid_rxs)  %>% distinct()



data.frame(Zoledronic %>% inner_join(New_Primary_Cancer_Box) %>% 
  group_by(Primary_Cancer) %>% summarise(n=100*sum(weight)/614009))


data.frame(Zoledronic %>% left_join(New_Primary_Cancer_Box)) %>% summarise(n=sum(weight, na.rm=T))  #614008.9

data.frame(Zoledronic %>% inner_join(PONS_Events)) %>% summarise(n=sum(weight, na.rm=T))  #243100.8
   




CAN_Doses <- fread("Source/CAN Doses.txt")
CAN_Doses <- CAN_Doses %>% filter(drug_class=="Radiotherapy") %>% 
  select(pat_id, from_dt) %>% distinct() %>% rename("patid"="pat_id") %>%
  inner_join(PONS_Events) %>% filter(from_dt>claimed) %>% select(patid) %>% distinct()
 



PONS_Events %>% inner_join(Zoledronic) %>%  summarise(n=sum(weight, na.rm=T)) # 243101
PONS_Events %>% inner_join(CAN_Doses) %>%  summarise(n=sum(weight, na.rm=T)) # 717690
PONS_Events %>% inner_join(CAN_Doses %>% full_join(Zoledronic) %>% distinct()) %>%  summarise(n=sum(weight, na.rm=T)) # 808005


# SREs
PONS_Comorbidity_Inventories <- fread("Source/PONS Comorbidity Inventories.txt")

SREs <- PONS_Comorbidity_Inventories %>% filter(grepl("M48", diagnosis)|
                                  grepl("M49", diagnosis)|
                                  grepl("M80", diagnosis)|
                                  grepl("M84", diagnosis)|
                                  grepl("M84", diagnosis)|
                                  grepl("M90", diagnosis)|
                                  grepl("M96", diagnosis)|
                                  grepl("S02", diagnosis)|
                                  grepl("S12", diagnosis)|
                                  grepl("S22", diagnosis)|
                                  grepl("S32", diagnosis)|
                                  grepl("S42", diagnosis)|
                                  grepl("S52", diagnosis)|
                                  grepl("S62", diagnosis)|
                                  grepl("S72", diagnosis)|
                                  grepl("S82", diagnosis)|
                                  grepl("S92", diagnosis)|
                                  grepl("T02", diagnosis)|
                                  grepl("T08", diagnosis)|
                                  grepl("T10", diagnosis)|
                                  grepl("T12", diagnosis)|
                                  grepl("T14", diagnosis)) %>% select(patid) %>% distinct()

data.frame(Zoledronic %>% left_join(New_Primary_Cancer_Box)) %>% summarise(n=sum(weight, na.rm=T))  #614008.9

data.frame(Zoledronic %>% inner_join(SREs))%>% left_join(New_Primary_Cancer_Box) %>% summarise(n=sum(weight, na.rm=T))  #353701
   
data.frame(
  SREs %>% inner_join(PONS_Events) %>% inner_join(Zoledronic) %>%
    left_join(New_Primary_Cancer_Box)
  ) %>% 
    summarise(n=sum(weight, na.rm=T))  #715311


# 2 Dxs bone mets  SRE
# % Zol or Radio



data.frame(
  SREs %>% inner_join(PONS_Events) %>%
    inner_join(Zoledronic %>% full_join(CAN_Doses) %>% distinct()) %>%
    left_join(New_Primary_Cancer_Box)
  ) %>% 
    summarise(n=sum(weight, na.rm=T))  #715311 # 438261.2





# --------------------
# Fulvestrant vs Palbociclib - biomarkers over time probability of being ON it ------

temp_Fulvestrant <- fread("Source/AfterFulvestrant_Month_Over_Month_Markers.txt")
temp_Palbociclib <- fread("Source/AfterPalbo_Month_Over_Month_Markers.txt")

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )



AST <- fread("Source/AST.txt")
ALT <- fread("Source/ALT.txt")
WBC <- fread("Source/WBC.txt")
Bilirub <- fread("Source/Bilirub.txt")
Lymphs <- fread("Source/Lymphs.txt")
Bicarb <- fread("Source/Bicarb.txt")
eGFR <- fread("Source/eGFR.txt")
Labs_Neutrophiles <- fread("Source/Labs_Neutrophiles.txt")
ANIONGAP <- fread("Source/ANIONGAP.txt")
POTASSIUM <- fread("Source/POTASSIUM.txt")
CALCIUM <- fread("Source/CALCIUM.txt")
PLATELETS <- fread("Source/PLATELETS.txt")
ALP <- fread("Source/ALP.txt")




ANIONGAP[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ANIONGAP <- ANIONGAP %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ANIONGAP <- ANIONGAP %>% ungroup()
ANIONGAP <- setDT(ANIONGAP)[Months_lookup, on = c("fst_dt" = "Month")]
ANIONGAP <- ANIONGAP %>% select(-fst_dt)
ANIONGAP <- ANIONGAP %>% rename("ANIONGAP"="rslt_nbr")


POTASSIUM[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
POTASSIUM <- POTASSIUM %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
POTASSIUM <- POTASSIUM %>% ungroup()
POTASSIUM <- setDT(POTASSIUM)[Months_lookup, on = c("fst_dt" = "Month")]
POTASSIUM <- POTASSIUM %>% select(-fst_dt)
POTASSIUM <- POTASSIUM %>% rename("POTASSIUM"="rslt_nbr")


CALCIUM[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
CALCIUM <- CALCIUM %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
CALCIUM <- CALCIUM %>% ungroup()
CALCIUM <- setDT(CALCIUM)[Months_lookup, on = c("fst_dt" = "Month")]
CALCIUM <- CALCIUM %>% select(-fst_dt)
CALCIUM <- CALCIUM %>% rename("CALCIUM"="rslt_nbr")


PLATELETS[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
PLATELETS <- PLATELETS %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
PLATELETS <- PLATELETS %>% ungroup()
PLATELETS <- setDT(PLATELETS)[Months_lookup, on = c("fst_dt" = "Month")]
PLATELETS <- PLATELETS %>% select(-fst_dt)
PLATELETS <- PLATELETS %>% rename("PLATELETS"="rslt_nbr")

ALP[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ALP <- ALP %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ALP <- ALP %>% ungroup()
ALP <- setDT(ALP)[Months_lookup, on = c("fst_dt" = "Month")]
ALP <- ALP %>% select(-fst_dt)
ALP <- ALP %>% rename("ALP"="rslt_nbr")


AST[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
AST <- AST %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
AST <- AST %>% ungroup()
AST <- setDT(AST)[Months_lookup, on = c("fst_dt" = "Month")]
AST <- AST %>% select(-fst_dt)
AST <- AST %>% rename("AST"="rslt_nbr")


ALT[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ALT <- ALT %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ALT <- ALT %>% ungroup()
ALT <- setDT(ALT)[Months_lookup, on = c("fst_dt" = "Month")]
ALT <- ALT %>% select(-fst_dt)
ALT <- ALT %>% rename("ALT"="rslt_nbr")


WBC[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
WBC <- WBC %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
WBC <- WBC %>% ungroup()
WBC <- setDT(WBC)[Months_lookup, on = c("fst_dt" = "Month")]
WBC <- WBC %>% select(-fst_dt)
WBC <- WBC %>% rename("WBC"="rslt_nbr")



Bilirub[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Bilirub <- Bilirub %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Bilirub <- Bilirub %>% ungroup()
Bilirub <- setDT(Bilirub)[Months_lookup, on = c("fst_dt" = "Month")]
Bilirub <- Bilirub %>% select(-fst_dt)
Bilirub <- Bilirub %>% rename("Bilirub"="rslt_nbr")



Lymphs[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Lymphs <- Lymphs %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Lymphs <- Lymphs %>% ungroup()
Lymphs <- setDT(Lymphs)[Months_lookup, on = c("fst_dt" = "Month")]
Lymphs <- Lymphs %>% select(-fst_dt)
Lymphs <- Lymphs %>% rename("Lymphs"="rslt_nbr")



Bicarb[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Bicarb <- Bicarb %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Bicarb <- Bicarb %>% ungroup()
Bicarb <- setDT(Bicarb)[Months_lookup, on = c("fst_dt" = "Month")]
Bicarb <- Bicarb %>% select(-fst_dt)
Bicarb <- Bicarb %>% rename("Bicarb"="rslt_nbr")


eGFR[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
eGFR <- eGFR %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
eGFR <- eGFR %>% ungroup()
eGFR <- eGFR %>% filter(rslt_nbr>10&rslt_nbr<150)
eGFR <- setDT(eGFR)[Months_lookup, on = c("fst_dt" = "Month")]
eGFR <- eGFR %>% select(-fst_dt)
eGFR <- eGFR %>% rename("eGFR"="rslt_nbr")


Labs_Neutrophiles[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Labs_Neutrophiles <- Labs_Neutrophiles %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Labs_Neutrophiles <- Labs_Neutrophiles %>% ungroup()
Labs_Neutrophiles <- setDT(Labs_Neutrophiles)[Months_lookup, on = c("fst_dt" = "Month")]
Labs_Neutrophiles <- Labs_Neutrophiles %>% select(-fst_dt)
Labs_Neutrophiles <- Labs_Neutrophiles %>% rename("Neutrophiles"="rslt_nbr")


temp_Palbociclib <- temp_Palbociclib %>% 
  left_join(AST, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(ALT, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(WBC, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Bilirub, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Lymphs, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Bicarb, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(eGFR, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Labs_Neutrophiles, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(ALP, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(CALCIUM, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(POTASSIUM, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(PLATELETS, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(ANIONGAP, by=c("patient"="patid", "Elapsed"="Exact_Month"))  



temp_Fulvestrant <- temp_Fulvestrant %>% 
  left_join(AST, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(ALT, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(WBC, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Bilirub, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Lymphs, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Bicarb, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(eGFR, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Labs_Neutrophiles, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(ALP, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(CALCIUM, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(POTASSIUM, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(PLATELETS, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(ANIONGAP, by=c("patient"="patid", "Elapsed"="Exact_Month"))  



temp_Carboplatin <- temp_Carboplatin %>% 
  left_join(AST, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(ALT, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(WBC, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Bilirub, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Lymphs, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Bicarb, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(eGFR, by=c("patient"="patid", "Elapsed"="Exact_Month"))  %>%
  left_join(Labs_Neutrophiles, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(ALP, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(CALCIUM, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(POTASSIUM, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(PLATELETS, by=c("patient"="patid", "Elapsed"="Exact_Month"))    %>%
  left_join(ANIONGAP, by=c("patient"="patid", "Elapsed"="Exact_Month"))  

length(unique(temp_Palbociclib$patient))
length(unique(temp_Carboplatin$patient))


temp <- temp_Fulvestrant %>% rename("ON"="ON_Fulvestrant") %>% mutate(group="Fulvestrant") %>%
  bind_rows(temp_Palbociclib %>% rename("ON"="ON_Palbo") %>% mutate(group="Palbociclib")) 

temp %>%
  gather(Test, Result, Albumin:ANIONGAP) %>%
  filter(Test!="Bicarb"&Test!="Bilirub"&Test!="WBC"&Test!="PLATELETS"&Test!="eGFR") %>%
  ggplot(aes(Result, ON, colour=group, fill=group, group=group))+ 
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial)) +
  facet_wrap(~Test, scales="free_x") +
  theme_minimal() +
  xlab("\n Test result") +
  ylab("Probability of \n STILL being ON Drug \n") +
  scale_fill_manual(values=c("firebrick", "midnightblue")) +
  scale_colour_manual(values=c("firebrick", "midnightblue")) 


temp %>%
  ggplot(aes(Elapsed, ON, colour=group, fill=group, group=group)) +
    stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial)) +
  theme_minimal() + xlab("\n Number of Elapsed Months \n Since Palbociclib / Fulvestrant Initiation") +
  ylab("Probability of STILL being ON Drug \n") +
  scale_fill_manual(values=c("firebrick", "midnightblue")) +
  scale_colour_manual(values=c("firebrick", "midnightblue")) 


temp %>%
  ggplot(aes(Elapsed, BMI, colour=group, fill=group, group=group)) +
    geom_smooth() + ylim(5,15)



model_Elapsed <- glmer(ON ~ 
                 Elapsed  + group +  (Elapsed |patient)  , 
                  data = temp, family = binomial)

summary(model_Elapsed)

model_Alb <- glmer(ON ~ 
                 Elapsed + Albumin + group +  (Albumin |patient)  , 
                  data = temp, family = binomial)

summary(model_Alb)

# ------------
# Model palbociclib --------------------------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
Metastatic <- PONS_Demographics%>% filter(!is.na(cancer_metastasis))  %>% select(patid)
New_Primary_Cancer_Box <- Metastatic %>% inner_join(New_Primary_Cancer_Box)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

Palbociclib_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-") %>% select(-Month) %>% distinct()
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>%  filter(Treat!="179")
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Treat=as.character(Treat)) %>%
  left_join(PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class), by=c("Treat"="molecule"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-Treat) %>% distinct()
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Exp=1) %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")
names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), "/", "_")

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(Palbociclib_pats %>% mutate(Palbo=1)) %>%
  select(patient, Palbo, Alkylating_Agent:Topoisomerase_Inhibitor)

CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- PONS_Demographics %>% inner_join(CAN_Drug_Histories %>% select(patient), by=c("patid"="patient"))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% mutate(Exp=1) %>% spread(key=diagnosis, value=Exp) 
PONS_Demographics[is.na(PONS_Demographics)] <- 0
PONS_Demographics <- PONS_Demographics %>% select(-Breast)
names(PONS_Demographics)[1] <- "patient"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Demographics)
CAN_Drug_Histories %>% group_by(Palbo) %>% count()

All_Discrete <- CAN_Drug_Histories


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- PONS_Demographics %>% inner_join(CAN_Drug_Histories %>% select(patient), by=c("patid"="patient"))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
Mets_sites <- PONS_Demographics %>% group_by(patid) %>% count() %>% rename("Mets_Sites"="n") 

PONS_Events <- fread("Source/PONS Events.txt")
PONS_Events <- PONS_Events %>% inner_join(All_Discrete %>% select(patient), by=c("patid"="patient"))
Number_Cancer_Dx <- PONS_Events %>% filter(grepl("C", code)) %>% select(patid, claimed) %>% distinct() %>%
  group_by(patid) %>% count() %>% rename("Number_Cancer_Dx"="n")


All_Discrete <- All_Discrete %>% left_join(Number_Cancer_Dx, by=c("patient"="patid")) %>% left_join(Mets_sites, by=c("patient"="patid"))

setDT(All_Discrete)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[All_Discrete[, .(patient)], on = c("patient"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Treat=as.character(Treat)) %>%
  left_join(PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class), by=c("Treat"="molecule"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month, chemo_class) %>% distinct()
CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient, chemo_class) %>% count() %>%
spread(key=chemo_class, value=n)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")
names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), "/", "_")

temp2_Conts <- All_Discrete %>% select(patient, Palbo,  Number_Cancer_Dx, Mets_Sites) %>% left_join(CAN_Drug_Histories)

sum(is.na(temp2_Conts))


temp2_Conts[is.na(temp2_Conts)] <- 0



temp <- temp2_Conts %>% select(-patient) 

temp <- temp %>% group_by(Palbo) %>% sample_n(3000) %>% ungroup()

modelAll_1_rf <- randomForest::randomForest(as.factor(Palbo) ~ ., data = temp)

summary(modelAll_1_rf)

data.frame(modelAll_1_rf$importance) %>% arrange(-MeanDecreaseGini)

result <- data.frame(variable = character(), group_1_mean = numeric(), group_2_mean = numeric())


for (col_name in names(temp)[-1]) {
  
  means <- tapply(temp[[col_name]], temp$Palbo, mean)
  
  result <- rbind(result, data.frame(variable = col_name, group_1_mean = means[1], group_2_mean = means[2]))
}


result <- data.frame(variable = character(), 
                     group_1_mean = numeric(), 
                     group_2_mean = numeric(),
                     group_3_mean = numeric(),
                     group_4_mean = numeric())

for (col_name in names(temp)[-1]) {
  
  means <- tapply(temp[[col_name]], temp$group, mean)
  
  result <- rbind(result, data.frame(variable = col_name, 
                                     group_1_mean = means[1], 
                                     group_2_mean = means[2],
                                     group_3_mean = means[3],
                                     group_4_mean = means[4]))
}


# ------------------------


# Model palbociclib  V2 All groups --------------------------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
Metastatic <- PONS_Demographics%>% filter(!is.na(cancer_metastasis))  %>% select(patid)
New_Primary_Cancer_Box <- Metastatic %>% inner_join(New_Primary_Cancer_Box)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")


Palbociclib_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()
OtherTarget_pats <- CAN_Drug_Histories %>% filter(grepl(string_target, Treat)) %>% select(patient) %>% distinct()
OtherChemo_pats <- CAN_Drug_Histories %>% filter(grepl(string_OtherChemo, Treat)) %>% select(patient) %>% distinct()
Hormonal_pats <- CAN_Drug_Histories %>% filter(grepl(string_Hormonal, Treat)) %>% select(patient) %>% distinct()

Hormonal_pats %>% anti_join(OtherChemo_pats) %>% anti_join(OtherTarget_pats) %>% anti_join(Palbociclib_pats) #  7851

OtherChemo_pats %>% anti_join(OtherTarget_pats) %>% anti_join(Palbociclib_pats) # 14743

OtherTarget_pats %>% anti_join(Palbociclib_pats) # 1035

Palbociclib_pats  # 3142

length(unique(CAN_Drug_Histories$patient))

groups <- Palbociclib_pats %>% mutate(group="Palbo") %>%
  bind_rows(OtherTarget_pats %>% anti_join(Palbociclib_pats) %>% mutate(group="OtherTarget")) %>%
  bind_rows(OtherChemo_pats %>% anti_join(OtherTarget_pats) %>% anti_join(Palbociclib_pats) %>% mutate(group="OtherChemo")) %>%
  bind_rows(Hormonal_pats %>% anti_join(OtherChemo_pats) %>% anti_join(OtherTarget_pats) %>% anti_join(Palbociclib_pats) %>% mutate(group="HormonalOnly"))


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-") %>% select(-Month) %>% distinct()
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Treat=as.character(Treat)) %>%
  left_join(PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class), by=c("Treat"="molecule"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-Treat) %>% distinct()
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Exp=1) %>% spread(key=chemo_class, value=Exp)
CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), " ", "_")
names(CAN_Drug_Histories) <- str_replace_all(names(CAN_Drug_Histories), "/", "_")

# CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(Palbociclib_pats %>% mutate(Palbo=1)) %>%
#   select(patient, Palbo, Alkylating_Agent:Topoisomerase_Inhibitor)

CAN_Drug_Histories <- groups %>% inner_join(CAN_Drug_Histories) %>% select(patient, group, Alkylating_Agent:Topoisomerase_Inhibitor)

CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0

PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- PONS_Demographics %>% inner_join(CAN_Drug_Histories %>% select(patient), by=c("patid"="patient"))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% mutate(Exp=1) %>% spread(key=diagnosis, value=Exp) 
PONS_Demographics[is.na(PONS_Demographics)] <- 0
PONS_Demographics <- PONS_Demographics %>% select(-Breast)
names(PONS_Demographics)[1] <- "patient"

CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Demographics)
CAN_Drug_Histories %>% group_by(group) %>% count()

All_Discrete <- CAN_Drug_Histories


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- PONS_Demographics %>% inner_join(CAN_Drug_Histories %>% select(patient), by=c("patid"="patient"))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
Mets_sites <- PONS_Demographics %>% group_by(patid) %>% count() %>% rename("Mets_Sites"="n") 

PONS_Events <- fread("Source/PONS Events.txt")
PONS_Events <- PONS_Events %>% inner_join(All_Discrete %>% select(patient), by=c("patid"="patient"))
Number_Cancer_Dx <- PONS_Events %>% filter(grepl("C", code)) %>% select(patid, claimed) %>% distinct() %>%
  group_by(patid) %>% count() %>% rename("Number_Cancer_Dx"="n")


All_Discrete <- All_Discrete %>% left_join(Number_Cancer_Dx, by=c("patient"="patid")) %>% left_join(Mets_sites, by=c("patient"="patid"))


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, age, plan)
PONS_Demographics <- PONS_Demographics %>% mutate(plan=ifelse(plan=="M",0,1))


All_Discrete <- All_Discrete %>% left_join(PONS_Demographics, by=c("patient"="patid"))

setDT(All_Discrete)

temp <- All_Discrete %>% select(-patient)

unique(temp$group)

temp <- data.frame(temp)

sum(is.na(temp))

temp[is.na(temp)] <- 0

result <- data.frame(variable = character(), group_1_mean = numeric(), group_2_mean = numeric(), group_3_mean = numeric(), group_4_mean = numeric())

for (col_name in names(temp)[-1]) {
  
  means <- tapply(temp[[col_name]], temp$group, mean)
  
  result <- rbind(result, data.frame(variable = col_name, group_1_mean = means[1], group_2_mean = means[2], group_3_mean = means[3], group_4_mean = means[4]))
}


groups

PONS_Comorbidity_Inventories <- fread("Source/PONS Comorbidity Inventories.txt")
names(PONS_Comorbidity_Inventories)[1] <- "patient"
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-weight)

length(unique(PONS_Comorbidity_Inventories$patient))

PONS_Comorbidity_Inventories <- groups %>% left_join(PONS_Comorbidity_Inventories) 

length(unique(PONS_Comorbidity_Inventories$patient))

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% group_by(diagnosis) %>% count() %>%
  filter(n>3000) %>% select(diagnosis) %>% left_join(PONS_Comorbidity_Inventories)

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% mutate(exp=1) %>% spread(key=diagnosis, value=exp)

sum(is.na(PONS_Comorbidity_Inventories))

PONS_Comorbidity_Inventories[is.na(PONS_Comorbidity_Inventories)] <- 0

PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(-patient)




result <- list()

for (col_name in names(PONS_Comorbidity_Inventories)[-1]) {
  
  means <- tapply(PONS_Comorbidity_Inventories[[col_name]], PONS_Comorbidity_Inventories$group, mean)
  
  result[[col_name]] <- means
}

result_df <- as.data.frame(result)
result_df <- t(result_df)

row.names(result_df) <- names(PONS_Comorbidity_Inventories)[-1]

colnames(result_df) <- paste("group", 1:4, "_mean", sep = "_")

result_df <- data.frame(result_df)


fwrite(result_df, "result_df.csv")
result[1]
data.frame(row.names(result_df))



groups


PONS_MeasuresLabs <- fread("Source/PONS Measures Labs.txt")
unique(PONS_MeasuresLabs$test)
PONS_MeasuresLabs <- groups %>% inner_join(PONS_MeasuresLabs, by=c("patient"="patid"))
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% filter(test != "Cancer Stage" & test != "Body Height")
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% select(patient, test, unit, claimed, value) %>% distinct()
PONS_MeasuresLabs[, claimed := as.character(claimed)][, claimed := substr(claimed, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_MeasuresLabs <- PONS_MeasuresLabs[Months_lookup, on = c("claimed" = "Month")]
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% group_by(patient, test, Exact_Month) %>% summarise(value=min(value)) %>% ungroup()
length(unique(PONS_MeasuresLabs$patient))

PONS_MeasuresLabs <- PONS_MeasuresLabs %>% spread(key=test, value=value)


AST <- fread("Source/AST.txt")
ALT <- fread("Source/ALT.txt")
WBC <- fread("Source/WBC.txt")
Bilirub <- fread("Source/Bilirub.txt")
Lymphs <- fread("Source/Lymphs.txt")
Bicarb <- fread("Source/Bicarb.txt")
eGFR <- fread("Source/eGFR.txt")
Labs_Neutrophiles <- fread("Source/Labs_Neutrophiles.txt")
ANIONGAP <- fread("Source/ANIONGAP.txt")
POTASSIUM <- fread("Source/POTASSIUM.txt")
CALCIUM <- fread("Source/CALCIUM.txt")
PLATELETS <- fread("Source/PLATELETS.txt")
ALP <- fread("Source/ALP.txt")




ANIONGAP[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ANIONGAP <- ANIONGAP %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ANIONGAP <- ANIONGAP %>% ungroup()
ANIONGAP <- setDT(ANIONGAP)[Months_lookup, on = c("fst_dt" = "Month")]
ANIONGAP <- ANIONGAP %>% select(-fst_dt)
ANIONGAP <- ANIONGAP %>% rename("ANIONGAP"="rslt_nbr")


POTASSIUM[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
POTASSIUM <- POTASSIUM %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
POTASSIUM <- POTASSIUM %>% ungroup()
POTASSIUM <- setDT(POTASSIUM)[Months_lookup, on = c("fst_dt" = "Month")]
POTASSIUM <- POTASSIUM %>% select(-fst_dt)
POTASSIUM <- POTASSIUM %>% rename("POTASSIUM"="rslt_nbr")


CALCIUM[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
CALCIUM <- CALCIUM %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
CALCIUM <- CALCIUM %>% ungroup()
CALCIUM <- setDT(CALCIUM)[Months_lookup, on = c("fst_dt" = "Month")]
CALCIUM <- CALCIUM %>% select(-fst_dt)
CALCIUM <- CALCIUM %>% rename("CALCIUM"="rslt_nbr")


PLATELETS[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
PLATELETS <- PLATELETS %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
PLATELETS <- PLATELETS %>% ungroup()
PLATELETS <- setDT(PLATELETS)[Months_lookup, on = c("fst_dt" = "Month")]
PLATELETS <- PLATELETS %>% select(-fst_dt)
PLATELETS <- PLATELETS %>% rename("PLATELETS"="rslt_nbr")

ALP[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ALP <- ALP %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ALP <- ALP %>% ungroup()
ALP <- setDT(ALP)[Months_lookup, on = c("fst_dt" = "Month")]
ALP <- ALP %>% select(-fst_dt)
ALP <- ALP %>% rename("ALP"="rslt_nbr")


AST[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
AST <- AST %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
AST <- AST %>% ungroup()
AST <- setDT(AST)[Months_lookup, on = c("fst_dt" = "Month")]
AST <- AST %>% select(-fst_dt)
AST <- AST %>% rename("AST"="rslt_nbr")


ALT[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ALT <- ALT %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ALT <- ALT %>% ungroup()
ALT <- setDT(ALT)[Months_lookup, on = c("fst_dt" = "Month")]
ALT <- ALT %>% select(-fst_dt)
ALT <- ALT %>% rename("ALT"="rslt_nbr")


WBC[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
WBC <- WBC %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
WBC <- WBC %>% ungroup()
WBC <- setDT(WBC)[Months_lookup, on = c("fst_dt" = "Month")]
WBC <- WBC %>% select(-fst_dt)
WBC <- WBC %>% rename("WBC"="rslt_nbr")



Bilirub[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Bilirub <- Bilirub %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Bilirub <- Bilirub %>% ungroup()
Bilirub <- setDT(Bilirub)[Months_lookup, on = c("fst_dt" = "Month")]
Bilirub <- Bilirub %>% select(-fst_dt)
Bilirub <- Bilirub %>% rename("Bilirub"="rslt_nbr")



Lymphs[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Lymphs <- Lymphs %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Lymphs <- Lymphs %>% ungroup()
Lymphs <- setDT(Lymphs)[Months_lookup, on = c("fst_dt" = "Month")]
Lymphs <- Lymphs %>% select(-fst_dt)
Lymphs <- Lymphs %>% rename("Lymphs"="rslt_nbr")



Bicarb[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Bicarb <- Bicarb %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Bicarb <- Bicarb %>% ungroup()
Bicarb <- setDT(Bicarb)[Months_lookup, on = c("fst_dt" = "Month")]
Bicarb <- Bicarb %>% select(-fst_dt)
Bicarb <- Bicarb %>% rename("Bicarb"="rslt_nbr")


eGFR[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
eGFR <- eGFR %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
eGFR <- eGFR %>% ungroup()
eGFR <- eGFR %>% filter(rslt_nbr>10&rslt_nbr<150)
eGFR <- setDT(eGFR)[Months_lookup, on = c("fst_dt" = "Month")]
eGFR <- eGFR %>% select(-fst_dt)
eGFR <- eGFR %>% rename("eGFR"="rslt_nbr")


Labs_Neutrophiles[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Labs_Neutrophiles <- Labs_Neutrophiles %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Labs_Neutrophiles <- Labs_Neutrophiles %>% ungroup()
Labs_Neutrophiles <- setDT(Labs_Neutrophiles)[Months_lookup, on = c("fst_dt" = "Month")]
Labs_Neutrophiles <- Labs_Neutrophiles %>% select(-fst_dt)
Labs_Neutrophiles <- Labs_Neutrophiles %>% rename("Neutrophiles"="rslt_nbr")



temp <- PONS_MeasuresLabs %>% 
  full_join(AST, by=c("patient"="patid", "Exact_Month"="Exact_Month"))  %>%
  full_join(ALT, by=c("patient"="patid", "Exact_Month"="Exact_Month"))  %>%
  full_join(WBC, by=c("patient"="patid", "Exact_Month"="Exact_Month"))  %>%
  full_join(Bilirub, by=c("patient"="patid", "Exact_Month"="Exact_Month"))  %>%
  full_join(Lymphs, by=c("patient"="patid", "Exact_Month"="Exact_Month"))  %>%
  full_join(Bicarb, by=c("patient"="patid", "Exact_Month"="Exact_Month"))  %>%
  full_join(eGFR, by=c("patient"="patid", "Exact_Month"="Exact_Month"))  %>%
  full_join(Labs_Neutrophiles, by=c("patient"="patid", "Exact_Month"="Exact_Month"))    %>%
  full_join(ALP, by=c("patient"="patid", "Exact_Month"="Exact_Month"))    %>%
  full_join(CALCIUM, by=c("patient"="patid", "Exact_Month"="Exact_Month"))    %>%
  full_join(POTASSIUM, by=c("patient"="patid", "Exact_Month"="Exact_Month"))    %>%
  full_join(PLATELETS, by=c("patient"="patid", "Exact_Month"="Exact_Month"))    %>%
  full_join(ANIONGAP, by=c("patient"="patid", "Exact_Month"="Exact_Month"))  


temp <- groups %>% inner_join(temp)
length(unique(temp$patient))


temp2 <- temp %>% gather(test, score, Albumin:ANIONGAP) %>% drop_na()

temp2 <- temp2 %>% group_by(group, patient, test) %>% filter(score==max(score)) %>% distinct() %>% ungroup()

temp2 %>% group_by(group, test) %>% summarise(mean=mean(score)) %>% spread(key=group, value=mean) 

# ------------------
# Time Elapsed vs Albumin as a function of where patients are ---------

temp_Palbociclib <- fread("Source/AfterPalbo_Month_Over_Month_Markers.txt")


temp_Palbociclib %>%
  filter(!is.na(Albumin)) %>% select(patient) %>% distinct() %>%
  sample_n(200) %>% 
  left_join(temp_Palbociclib) %>%
  ggplot(aes(Elapsed, Albumin, 
                                colour=as.factor(ON_Palbo), 
                                fill=as.factor(ON_Palbo))) +
    geom_line(aes(group=patient),  alpha=0.3) +
    geom_smooth() +
    xlim(0,24)


CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)
CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

Palbociclib_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()

CAN_Drug_Histories <- Palbociclib_pats %>% inner_join(CAN_Drug_Histories) 
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(
  CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% 
  group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, Month) %>% rename("Min"="Month")
)  %>% mutate(Elapsed=Month-Min)
  
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Lapsed=ifelse(Treat=="-", 1,0))



string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(OtherTarget=ifelse(grepl(string_target, Treat),1,0)) %>%
  mutate(OtherChemo=ifelse(grepl(string_OtherChemo, Treat),1,0)) %>%
  mutate(Hormonal=ifelse(grepl(string_Hormonal, Treat),1,0)) 


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Month, Treat, Elapsed, ON_Palbo, OtherTarget, OtherChemo, Hormonal, Lapsed) 
CAN_Drug_Histories <- CAN_Drug_Histories %>%  filter(Elapsed>=0) %>% select(-Month)

temp_Palbociclib <- CAN_Drug_Histories %>% left_join(temp_Palbociclib)

temp_Palbociclib <- temp_Palbociclib %>% mutate(group=ifelse(ON_Palbo==1, "Palbo",
                                         ifelse(OtherTarget==1, "Target",
                                                ifelse(OtherChemo==1, "Chemo",
                                                       ifelse(Hormonal==1, "Hormone",
                                                              ifelse(Lapsed==1, "Lapsed", "ignore"))))))

temp_Palbociclib %>% filter(group=="ignore")


temp_Palbociclib <- temp_Palbociclib %>% mutate(group=ifelse(Treat=="355", "Dead", group)) %>%
  mutate(group=ifelse(group=="ignore", "SupportiveCare", group))


temp_Palbociclib %>%
   mutate(group=ifelse(group=="Palbo", group, "no")) %>%
   ggplot(aes(Elapsed, Albumin, 
                                colour=as.factor(group), 
                                fill=as.factor(group))) +
    geom_smooth() +
    xlim(0,24)




temp_Palbociclib %>% filter(ON_Palbo==1) %>% group_by(patient) %>%
  filter(!is.na(Albumin) & Elapsed==max(Elapsed)) %>%
  select(patient, Albumin) %>% mutate(cutoff=ifelse(Albumin<=3.5, "Lower", "Upper")) %>%
  left_join(temp_Palbociclib) %>%
  filter(OtherTarget==1) %>% group_by(cutoff, patient) %>% count() %>% ungroup() %>%
  group_by(cutoff) %>% summarise(mean=mean(n))


temp_Palbociclib %>%
  group_by(patient) %>% mutate(grp = rle(ON_Palbo)$lengths %>% {rep(seq(length(.)), .)}) %>%
  group_by(patient) %>% filter(grp==min(grp)) %>% filter(Elapsed==max(Elapsed)) %>%
  select(patient, Elapsed) %>% mutate(lookup=Elapsed+1) %>% select(-Elapsed) %>%
  left_join(temp_Palbociclib) %>%
  filter(Elapsed<lookup) %>%  group_by(patient) %>%
  filter(Elapsed==max(Elapsed)) %>%
  filter(!is.na(Albumin)) %>% mutate(cutoff=ifelse(Albumin<=3.5, "Lower", "Upper")) %>% 
  select(-Albumin) %>% select(patient, cutoff, lookup) %>%
  left_join(temp_Palbociclib) %>% filter(Elapsed==lookup) %>%
  group_by(cutoff, OtherTarget, OtherChemo, Hormonal, Lapsed ) %>% count() %>%
  mutate(n=ifelse(cutoff=="Lower", 100*n/129, 100*n/436))
   

  
  filter(ON_Palbo==1) %>% group_by(patient) %>%
  filter(!is.na(Albumin) & Elapsed==max(Elapsed)) %>%
  select(patient, Albumin) %>% mutate(cutoff=ifelse(Albumin<=3.5, "Lower", "Upper")) %>%
  left_join(temp_Palbociclib) %>%
  filter(OtherTarget==1) %>% group_by(cutoff, patient) %>% count() %>% ungroup() %>%
  group_by(cutoff) %>% summarise(mean=mean(n))






temp_Palbociclib %>% filter(ON_Palbo==1) %>% group_by(patient) %>%
  filter(!is.na(Albumin) & Elapsed==max(Elapsed)) %>%
  select(patient, Albumin) %>% mutate(cutoff=ifelse(Albumin<=3.5, "Lower", "Upper")) %>%
  left_join(temp_Palbociclib) %>%
  filter(cutoff=="Upper") %>%
  ggplot(aes(Albumin, colour=as.factor(group), fill=as.factor(group))) +
  geom_density(alpha=0.3) +
  ggsci::scale_colour_futurama() +
  ggsci::scale_fill_futurama() +
  theme_minimal() +
  facet_wrap(~group)


# ----------------------
# Inflows and Outflows to palbo over time source lapsed naive ---------
CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")


string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")


Palbociclib_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()

CAN_Drug_Histories <- Palbociclib_pats %>% inner_join(CAN_Drug_Histories) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")


CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% rename("patient"="patid"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% select(patient, Month)  %>% 
  group_by(patient) %>% filter(Month==min(Month)) %>% rename("First_Palbo"="Month") %>%
  left_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, First_Palbo, metastasis_onset, Month, Treat, ON_Palbo)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% 
  mutate(Drug_Exp=cumsum(grepl(string_target, Treat)|
                           grepl(string_OtherChemo, Treat)|
                           grepl(string_Hormonal, Treat))) %>%
  mutate(Drug_Exp=ifelse(Drug_Exp>=1,1,0))



data.frame(CAN_Drug_Histories %>% filter(Month==First_Palbo-1 & Treat=="-") %>%
  mutate(Elapsed=Month-metastasis_onset) %>%
  group_by(Elapsed, Drug_Exp) %>% summarise(n=sum(weight)) %>%
  spread(key=Drug_Exp, value=n))


CAN_Drug_Histories <- CAN_Drug_Histories %>%
  left_join(CAN_Drug_Histories %>% filter(ON_Palbo==0&lag(ON_Palbo)==1) %>% select(patient, Month) %>% 
  group_by(patient)  %>% filter(Month==min(Month)) %>% rename("First_DropOut"="Month"))

CAN_Drug_Histories <- CAN_Drug_Histories %>%
  left_join(
    CAN_Drug_Histories %>% filter(Month>=First_DropOut) %>%
  group_by(patient) %>% 
    mutate(PostDrop_Drug_Exp=cumsum(grepl(string_target, Treat)|
                           grepl(string_OtherChemo, Treat)|
                           grepl(string_Hormonal, Treat))) %>%
  mutate(PostDrop_Drug_Exp=ifelse(PostDrop_Drug_Exp>=1,1,0))
  ) %>% mutate(PostDrop_Drug_Exp=ifelse(is.na(PostDrop_Drug_Exp),0,PostDrop_Drug_Exp))
  

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Box=ifelse(grepl("179", Treat), "Palbo",
                                         ifelse(grepl(string_target, Treat), "OtherTarget",
                                                ifelse(grepl(string_OtherChemo, Treat), "OtherChemo",
                                                       ifelse(grepl(string_Hormonal, Treat), "Hormonal", "Lapsed")))))

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ElapsedMets=Month-metastasis_onset)


data.frame(CAN_Drug_Histories %>% filter(ON_Palbo==1) %>%
             group_by(ElapsedMets, ON_Palbo) %>% summarise(n=sum(weight)))



data.frame(CAN_Drug_Histories %>% filter(ON_Palbo==0&lead(ON_Palbo)==1) %>%
             mutate(Box=ifelse(Box=="Lapsed"&Drug_Exp ==1, "Lapsed",
                               ifelse(Box=="Lapsed"&Drug_Exp==0, "Naive", Box))) %>%
             group_by(ElapsedMets, Box) %>% summarise(n=sum(weight)) %>%
             spread(key=Box, value=n)
             )



CAN_Drug_Histories <- CAN_Drug_Histories %>%
  left_join(CAN_Drug_Histories %>% group_by(patient) %>% summarise(PostDropLap=max(PostDrop_Drug_Exp)) %>%
  mutate(groupLapse=ifelse(PostDropLap==1, "ST", "LT")) %>% ungroup())

sum(is.na(CAN_Drug_Histories))  


CAN_Drug_Histories[is.na(CAN_Drug_Histories)] <- 0



data.frame(CAN_Drug_Histories %>% filter(ON_Palbo==0&lag(ON_Palbo)==1) %>%
             mutate(Box=ifelse(Box=="Lapsed"&groupLapse=="ST", "ST_Lapsed",
                               ifelse(Box=="Lapsed"&groupLapse=="LT", "LT_Lapsed", Box))) %>%
             group_by(ElapsedMets, Box) %>% summarise(n=sum(weight)) %>%
             spread(key=Box, value=n)
             )

data.frame(CAN_Drug_Histories %>%
  mutate(Box=ifelse(Box=="Lapsed"&Drug_Exp ==1, "Lapsed",
                               ifelse(Box=="Lapsed"&Drug_Exp==0, "Naive", Box))) %>%
  group_by(ElapsedMets, Box) %>% summarise(n=sum(weight)) %>%
  spread(key=Box, value=n))
           



# -------------------
# Stock month over month All breast cancer metastatic -------------------------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")


string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")


CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% rename("patient"="patid"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, metastasis_onset, Month, Treat, ON_Palbo)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% 
  mutate(Drug_Exp=cumsum(grepl(string_target, Treat)|
                           grepl(string_OtherChemo, Treat)|
                           grepl(string_Hormonal, Treat))) %>%
  mutate(Drug_Exp=ifelse(Drug_Exp>=1,1,0))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Box=ifelse(grepl("179", Treat), "Palbo",
                                         ifelse(grepl(string_target, Treat), "OtherTarget",
                                                ifelse(grepl(string_OtherChemo, Treat), "OtherChemo",
                                                       ifelse(grepl(string_Hormonal, Treat), "Hormonal", "Lapsed")))))

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ElapsedMets=Month-metastasis_onset)


data.frame(CAN_Drug_Histories %>%
  mutate(Box=ifelse(Box=="Lapsed"&Drug_Exp ==1, "Lapsed",
                               ifelse(Box=="Lapsed"&Drug_Exp==0, "Naive", Box))) %>%
  group_by(ElapsedMets, Box) %>% summarise(n=sum(weight)) %>%
  spread(key=Box, value=n))
           



# -------------------
# Summary waterfall breast cancer patients ------------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid, weight)
sum(New_Primary_Cancer_Box$weight) # 2509553

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis), 0, 1))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)

New_Primary_Cancer_Box %>% group_by(cancer_metastasis) %>% summarise(n=sum(weight))

#   cancer_metastasis        n
# 1                 0 1456230.
# 2                 1 1053323.

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, Treat) %>% distinct()

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&(
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted")], collapse = "|"),")\\b")

string_Biologic <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&(
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic")], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

Palbo_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()
Target_pats <- CAN_Drug_Histories %>% filter(grepl(string_target, Treat)) %>% select(patient) %>% distinct()
Biologic_pats <- CAN_Drug_Histories %>% filter(grepl(string_Biologic, Treat)) %>% select(patient) %>% distinct()
OtherChemo_pats <- CAN_Drug_Histories %>% filter(grepl(string_OtherChemo, Treat)) %>% select(patient) %>% distinct()
Hormonal_pats <- CAN_Drug_Histories %>% filter(grepl(string_Hormonal, Treat)) %>% select(patient) %>% distinct()

Cancer_exp <- Palbo_pats %>% full_join(Target_pats) %>% full_join(Biologic_pats) %>% 
  full_join(OtherChemo_pats) %>% full_join(Hormonal_pats) %>% distinct()

New_Primary_Cancer_Box %>% rename("patient"="patid") %>% inner_join(Cancer_exp) %>%   group_by(cancer_metastasis) %>% summarise(n=sum(weight))


New_Primary_Cancer_Box %>% rename("patient"="patid") %>%
  inner_join(Palbo_pats) %>%
#  anti_join(Palbo_pats) %>%  anti_join(Target_pats) %>%  anti_join(Biologic_pats) %>%  anti_join(OtherChemo_pats) %>%
  group_by(cancer_metastasis) %>% summarise(n=sum(weight))



New_Primary_Cancer_Box %>% rename("patient"="patid") %>%
  inner_join(OtherChemo_pats) %>%
  anti_join(Palbo_pats) %>%  anti_join(Target_pats) %>% 
  inner_join(Hormonal_pats) %>%
  group_by(cancer_metastasis) %>% summarise(n=sum(weight))


New_Primary_Cancer_Box %>% rename("patient"="patid") %>%
  inner_join(OtherChemo_pats) %>% inner_join(Hormonal_pats) %>%
  group_by(cancer_metastasis) %>%   summarise(n=sum(weight))

temp <- New_Primary_Cancer_Box %>%  rename("patient"="patid") %>%
  left_join(Hormonal_pats %>% mutate(Hormonal="Hormonal")) %>%
  left_join(OtherChemo_pats %>% mutate(Chemo="Chemo")) %>%
  left_join(Biologic_pats %>% mutate(Biologic="Biologic")) %>%
  left_join(Target_pats %>% mutate(Target="Target")) %>%
  left_join(Palbo_pats %>% mutate(Palbo="Palbo")) 

temp[is.na(temp)] <- 0
  
temp %>%  mutate(GROUP=ifelse(Palbo=="Palbo", "Palbo",
                      ifelse(Target=="Target", "Target",
                             ifelse(Biologic=="Biologic", "Biologic",
                                    ifelse(Chemo=="Chemo", "Chemo",
                                           ifelse(Hormonal=="Hormonal","Hormonal", NA)))))) %>%
  group_by(cancer_metastasis, GROUP, Hormonal) %>%   summarise(n=sum(weight)) %>%
  filter(Hormonal!="0")

# -----------
# Stock month over month All breast cancer metastatic NAIVE vs EXP before mets -------------------------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")


string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")


CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% rename("patient"="patid"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, metastasis_onset, Month, Treat, ON_Palbo)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% 
  mutate(Drug_Exp=cumsum(grepl(string_target, Treat)|
                           grepl(string_OtherChemo, Treat)|
                           grepl(string_Hormonal, Treat))) %>%
  mutate(Drug_Exp=ifelse(Drug_Exp>=1,1,0))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Box=ifelse(grepl("179", Treat), "Palbo",
                                         ifelse(grepl(string_target, Treat), "OtherTarget",
                                                ifelse(grepl(string_OtherChemo, Treat), "OtherChemo",
                                                       ifelse(grepl(string_Hormonal, Treat), "Hormonal", "Lapsed")))))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ElapsedMets=Month-metastasis_onset)



groups <- CAN_Drug_Histories %>% filter(ElapsedMets==(-1)) %>% select(patient, Drug_Exp) %>% ungroup()


data.frame(CAN_Drug_Histories %>%
             inner_join(groups %>% filter(Drug_Exp==1) %>% select(patient)) %>%
  mutate(Box=ifelse(Box=="Lapsed"&Drug_Exp ==1, "Lapsed",
                               ifelse(Box=="Lapsed"&Drug_Exp==0, "Naive", Box))) %>%
  group_by(ElapsedMets, Box) %>% summarise(n=sum(weight)) %>%
  spread(key=Box, value=n))
           


CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% select(patient) %>% distinct() %>%
  left_join(CAN_Drug_Histories) %>% filter(ON_Palbo==1) %>% group_by(patient)  %>%
  filter(ElapsedMets==min(ElapsedMets)) %>%
  select(patient, ElapsedMets) %>%
  left_join(
    CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% group_by(patient) %>% count()
  ) %>% ungroup() %>%
  filter(ElapsedMets>=0) %>%
  ggplot(aes(ElapsedMets, n)) +
  geom_jitter(size=0.5, alpha=0.5, colour="deepskyblue4") +
  geom_smooth(colour="black", fill="black") +
  theme_minimal() +
  xlim(0,24) +
  xlab("\n Month of Palbociclib Initiation \n (Relative to Metastasis)") +
  ylab("Number of Month Spent ON Palbociclib \n")

# ----------------------


# Metastatic breast cancer patients - Box groups ~Liver Bone Lung ------------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid, weight)
sum(New_Primary_Cancer_Box$weight) # 2509553

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, cancer_metastasis) %>% mutate(cancer_metastasis=ifelse(is.na(cancer_metastasis), 0, 1))

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics)

New_Primary_Cancer_Box %>% group_by(cancer_metastasis) %>% summarise(n=sum(weight))

#   cancer_metastasis        n
# 1                 0 1456230.
# 2                 1 1053323.

New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(cancer_metastasis==1)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, Treat) %>% distinct()

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&(
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted")], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

Palbo_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()
Target_pats <- CAN_Drug_Histories %>% filter(grepl(string_target, Treat)) %>% select(patient) %>% distinct()
OtherChemo_pats <- CAN_Drug_Histories %>% filter(grepl(string_OtherChemo, Treat)) %>% select(patient) %>% distinct()
Hormonal_pats <- CAN_Drug_Histories %>% filter(grepl(string_Hormonal, Treat)) %>% select(patient) %>% distinct()

Cancer_exp <- Palbo_pats %>% full_join(Target_pats) %>% full_join(OtherChemo_pats) %>% full_join(Hormonal_pats) %>% distinct()

groups <- Palbo_pats %>% mutate(group="Palbo") %>%
  bind_rows(Target_pats %>% anti_join(Palbo_pats) %>% mutate(group="Target_pats")) %>%
  bind_rows(OtherChemo_pats %>% anti_join(Target_pats) %>% anti_join(Palbo_pats) %>% mutate(group="OtherChemo_pats")) %>%
  bind_rows(Hormonal_pats %>% anti_join(Palbo_pats)%>% anti_join(Target_pats) %>% anti_join(OtherChemo_pats) %>% mutate(group="Hormonal_pats")) 


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis)

groups %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>% group_by(group) %>% summarise(n=sum(weight))

# 1 Hormonal_pats   221091.
# 2 OtherChemo_pats 415371.
# 3 Palbo            97581.
# 4 Target_pats     308299.

groups %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>% 
  filter(grepl("Liver", diagnosis)|grepl("Lung", diagnosis)|grepl("Bone", diagnosis)) %>%
  group_by(group) %>% summarise(n=sum(weight))

# 1 Hormonal_pats    40761.
# 2 OtherChemo_pats 100419.
# 3 Palbo            91721.
# 4 Target_pats     131577.




AST <- fread("Source/AST.txt")
ALT <- fread("Source/ALT.txt")
WBC <- fread("Source/WBC.txt")
Bilirub <- fread("Source/Bilirub.txt")
Lymphs <- fread("Source/Lymphs.txt")
Bicarb <- fread("Source/Bicarb.txt")
eGFR <- fread("Source/eGFR.txt")
Labs_Neutrophiles <- fread("Source/Labs_Neutrophiles.txt")
ANIONGAP <- fread("Source/ANIONGAP.txt")
POTASSIUM <- fread("Source/POTASSIUM.txt")
CALCIUM <- fread("Source/CALCIUM.txt")
PLATELETS <- fread("Source/PLATELETS.txt")
ALP <- fread("Source/ALP.txt")



Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )


ANIONGAP[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ANIONGAP <- ANIONGAP %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ANIONGAP <- ANIONGAP %>% ungroup()
ANIONGAP <- setDT(ANIONGAP)[Months_lookup, on = c("fst_dt" = "Month")]
ANIONGAP <- ANIONGAP %>% select(-fst_dt)
ANIONGAP <- ANIONGAP %>% mutate(test="ANIONGAP")


POTASSIUM[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
POTASSIUM <- POTASSIUM %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
POTASSIUM <- POTASSIUM %>% ungroup()
POTASSIUM <- setDT(POTASSIUM)[Months_lookup, on = c("fst_dt" = "Month")]
POTASSIUM <- POTASSIUM %>% select(-fst_dt)
POTASSIUM <- POTASSIUM %>% mutate(test="POTASSIUM")


CALCIUM[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
CALCIUM <- CALCIUM %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
CALCIUM <- CALCIUM %>% ungroup()
CALCIUM <- setDT(CALCIUM)[Months_lookup, on = c("fst_dt" = "Month")]
CALCIUM <- CALCIUM %>% select(-fst_dt)
CALCIUM <- CALCIUM %>% mutate(test="CALCIUM")


PLATELETS[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
PLATELETS <- PLATELETS %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
PLATELETS <- PLATELETS %>% ungroup()
PLATELETS <- setDT(PLATELETS)[Months_lookup, on = c("fst_dt" = "Month")]
PLATELETS <- PLATELETS %>% select(-fst_dt)
PLATELETS <- PLATELETS %>% mutate(test="PLATELETS")

ALP[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ALP <- ALP %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ALP <- ALP %>% ungroup()
ALP <- setDT(ALP)[Months_lookup, on = c("fst_dt" = "Month")]
ALP <- ALP %>% select(-fst_dt)
ALP <- ALP %>% mutate(test="ALP")


AST[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
AST <- AST %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
AST <- AST %>% ungroup()
AST <- setDT(AST)[Months_lookup, on = c("fst_dt" = "Month")]
AST <- AST %>% select(-fst_dt)
AST <- AST %>% mutate(test="AST")


ALT[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
ALT <- ALT %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
ALT <- ALT %>% ungroup()
ALT <- setDT(ALT)[Months_lookup, on = c("fst_dt" = "Month")]
ALT <- ALT %>% select(-fst_dt)
ALT <- ALT %>% mutate(test="ALT")


WBC[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
WBC <- WBC %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
WBC <- WBC %>% ungroup()
WBC <- setDT(WBC)[Months_lookup, on = c("fst_dt" = "Month")]
WBC <- WBC %>% select(-fst_dt)
WBC <- WBC %>% mutate(test="WBC")



Bilirub[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Bilirub <- Bilirub %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Bilirub <- Bilirub %>% ungroup()
Bilirub <- setDT(Bilirub)[Months_lookup, on = c("fst_dt" = "Month")]
Bilirub <- Bilirub %>% select(-fst_dt)
Bilirub <- Bilirub %>% mutate(test="Bilirub")



Lymphs[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Lymphs <- Lymphs %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Lymphs <- Lymphs %>% ungroup()
Lymphs <- setDT(Lymphs)[Months_lookup, on = c("fst_dt" = "Month")]
Lymphs <- Lymphs %>% select(-fst_dt)
Lymphs <- Lymphs %>% mutate(test="Lymphs")



Bicarb[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Bicarb <- Bicarb %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Bicarb <- Bicarb %>% ungroup()
Bicarb <- setDT(Bicarb)[Months_lookup, on = c("fst_dt" = "Month")]
Bicarb <- Bicarb %>% select(-fst_dt)
Bicarb <- Bicarb %>% mutate(test="Bicarb")


eGFR[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
eGFR <- eGFR %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
eGFR <- eGFR %>% ungroup()
eGFR <- eGFR %>% filter(rslt_nbr>10&rslt_nbr<150)
eGFR <- setDT(eGFR)[Months_lookup, on = c("fst_dt" = "Month")]
eGFR <- eGFR %>% select(-fst_dt)
eGFR <- eGFR %>% mutate(test="eGFR")


Labs_Neutrophiles[, fst_dt := as.character(fst_dt)][, fst_dt := substr(fst_dt, 1L, 7L)]
Labs_Neutrophiles <- Labs_Neutrophiles %>% group_by(patid, fst_dt) %>% summarise(rslt_nbr=min(rslt_nbr))
Labs_Neutrophiles <- Labs_Neutrophiles %>% ungroup()
Labs_Neutrophiles <- setDT(Labs_Neutrophiles)[Months_lookup, on = c("fst_dt" = "Month")]
Labs_Neutrophiles <- Labs_Neutrophiles %>% select(-fst_dt)
Labs_Neutrophiles <- Labs_Neutrophiles %>% mutate(test="Labs_Neutrophiles")


Other_Labs <- ANIONGAP %>% bind_rows(POTASSIUM) %>% bind_rows(CALCIUM)  %>% 
  bind_rows(PLATELETS)  %>% bind_rows(ALP)  %>% bind_rows(AST)  %>% bind_rows(ALT)  %>%
  bind_rows(WBC)  %>% bind_rows(Bilirub)  %>% bind_rows(Lymphs)  %>% bind_rows(Bicarb) %>% 
  bind_rows(eGFR)  %>% bind_rows(Labs_Neutrophiles)



PONS_MeasuresLabs <- fread("Source/PONS Measures Labs.txt")
PONS_MeasuresLabs <- groups %>% select(patient) %>% inner_join(PONS_MeasuresLabs, by=c("patient"="patid"))
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% filter(test != "Cancer Stage" & test != "Body Height")
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% select(patient, test, unit, claimed, value) %>% distinct()
setDT(PONS_MeasuresLabs)
PONS_MeasuresLabs[, claimed := as.character(claimed)][, claimed := substr(claimed, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_MeasuresLabs <- PONS_MeasuresLabs[Months_lookup, on = c("claimed" = "Month")]
PONS_MeasuresLabs <- PONS_MeasuresLabs %>% group_by(patient, test, Exact_Month) %>% summarise(value=mean(value)) %>% ungroup()

names(Other_Labs) <- c("patient", "value", "Exact_Month", "test")

Other_Labs <- PONS_MeasuresLabs %>% select(patient, value, Exact_Month, test)  %>%
  bind_rows(Other_Labs)

Last <- Other_Labs %>% group_by(patient, test) %>% filter(Exact_Month==max(Exact_Month)) %>% select(patient, test, value) %>% ungroup()

unique(Last$test)

groups %>% inner_join(Last) %>%
  filter( (test=="Albumin"&value>1&value<5)|
            (test=="Labs_Neutrophiles"&value>1&value<14)|
            (test=="BMI"&value>15&value<60) |
            (test=="POTASSIUM"&value>2&value<6)|
            (test=="CALCIUM"&value>6.2&value<11.2)|
            (test=="Hemoglobin"&value>5&value<16)|
            (test=="AST"&value>0&value<100) | 
            (test=="ALT"&value>0&value<100) | 
            (test=="ALP"&value>0&value<250) ) %>%
  ggplot(aes(value, colour=group, fill=group)) + 
  geom_density(alpha=0.6) +
  facet_wrap(~test, scales="free") +
  theme_minimal() +
  xlab("\n LAST Lab test result observed") +
  ylab("Patient density \n") +
  scale_colour_manual(values=c("#D3D3D3","#FFEFCA", "#C86060", "#66A4B9" )) +
  scale_fill_manual(values=c("#D3D3D3","#FFEFCA", "#C86060", "#66A4B9" ))

PONS_Demographics %>% 
  filter(grepl("Liver", diagnosis)|grepl("Lung", diagnosis)|grepl("Bone", diagnosis)) %>%
  select(patid) %>% distinct() %>% mutate(Aggressive="YES")  

groups %>% inner_join(Last) %>%
  left_join(PONS_Demographics %>% 
  filter(grepl("Liver", diagnosis)|grepl("Lung", diagnosis)|grepl("Bone", diagnosis)) %>%
  select(patid) %>% distinct() %>% mutate(Aggressive="YES"), by=c("patient"="patid")) %>%
  mutate(Aggressive=ifelse(is.na(Aggressive), "NO", Aggressive)) %>%
  filter( (test=="Albumin"&value>1&value<5)|
            (test=="Labs_Neutrophiles"&value>1&value<14)|
            (test=="BMI"&value>15&value<60) |
            (test=="POTASSIUM"&value>2&value<6)|
            (test=="CALCIUM"&value>6.2&value<11.2)|
            (test=="Hemoglobin"&value>5&value<16)|
            (test=="AST"&value>0&value<100) | 
            (test=="ALT"&value>0&value<100) | 
            (test=="ALP"&value>0&value<250) ) %>%
  ggplot(aes(value, colour=Aggressive, fill=Aggressive)) + 
  geom_density(alpha=0.5) +
  facet_wrap(~test, scales="free") +
  theme_minimal() +
  xlab("\n LAST Lab test result observed") +
  ylab("Patient density \n") +
  scale_colour_manual(values=c("#66A4B9", "#C86060")) +
  scale_fill_manual(values=c("#66A4B9", "#C86060" ))




PONS_Demographics_surv <- fread("Source/PONS Demographics.txt")
PONS_Demographics_surv <- PONS_Demographics_surv %>% select(patid, cancer_onset, cancer_metastasis , death_date )

PONS_Demographics_surv$cancer_onset <- as.Date(PONS_Demographics_surv$cancer_onset)
PONS_Demographics_surv$cancer_metastasis <- as.Date(PONS_Demographics_surv$cancer_metastasis)
PONS_Demographics_surv$death_date  <- as.Date(PONS_Demographics_surv$death_date)

missingDeathDay <- ymd("2025-07-31")

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_surv <- PONS_Demographics_surv %>% inner_join(groups %>% select(patient), by=c("patid"="patient"))

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived_Mets = as.numeric(death_date)-as.numeric(cancer_metastasis)) %>%
  mutate(Survived_Mets=ifelse(Survived_Mets>1825,1825,Survived_Mets))



groups %>% 
  left_join(PONS_Demographics %>% 
              filter(grepl("Liver", diagnosis)|grepl("Lung", diagnosis)|grepl("Bone", diagnosis)) %>%
  select(patid) %>% distinct() %>% mutate(Aggressive="YES"), by=c("patient"="patid")) %>%
    mutate(Aggressive=ifelse(is.na(Aggressive), "NO", Aggressive)) %>%
  inner_join(PONS_Demographics_surv, by=c("patient"="patid")) %>%
  group_by(Aggressive) %>% summarise(n=mean(Survived/30.5))
  


groups %>% 
  left_join(PONS_Demographics %>% 
              filter(grepl("Liver", diagnosis)|grepl("Lung", diagnosis)|grepl("Bone", diagnosis)) %>%
  select(patid) %>% distinct() %>% mutate(Aggressive="YES"), by=c("patient"="patid")) %>%
    mutate(Aggressive=ifelse(is.na(Aggressive), "NO", Aggressive)) %>%
  inner_join(PONS_Demographics_surv, by=c("patient"="patid")) %>%
  group_by(Aggressive) %>% summarise(n=mean(Survived_Mets/30.5))
  


PONS_Demographics %>% 
              filter(grepl("Liver", diagnosis)|grepl("Lung", diagnosis)|grepl("Bone", diagnosis)) %>%
  select(patid) %>% distinct() %>% mutate(Aggressive="YES") %>%
    mutate(Aggressive=ifelse(is.na(Aggressive), "NO", Aggressive)) %>%
    inner_join(PONS_Demographics_surv) %>%
  group_by(Aggressive) %>% summarise(n=mean(as.numeric(cancer_onset)))


CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, Treat) %>% distinct()
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(groups %>% select(patient))


N_months_Hormonal <- CAN_Drug_Histories %>% filter(grepl(string_Hormonal, Treat)) %>%  group_by(patient) %>% count()

groups %>% left_join(N_months_Hormonal) %>%
  left_join(PONS_Demographics %>% 
              filter(grepl("Liver", diagnosis)|grepl("Lung", diagnosis)|grepl("Bone", diagnosis)) %>%
  select(patid) %>% distinct() %>% mutate(Aggressive="YES"), by=c("patient"="patid") ) %>%
  # mutate(n=ifelse(is.na(n),0,n)) %>%
  group_by(Aggressive) %>% summarise(mean=mean(n, na.rm=T))



CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
setDT(groups)
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[groups[, .(patient)], on = c("patient"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))
CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month)

stop <- CAN_Drug_Histories %>% filter(!grepl(string_OtherChemo, Treat) & grepl(string_OtherChemo, lag(Treat))) %>%
  select(patient, weight) %>% distinct()

sum(stop$weight)

temp <- CAN_Drug_Histories %>% filter(!grepl(string_OtherChemo, Treat) & grepl(string_OtherChemo, lag(Treat)) &
                              !grepl(string_target, Treat)  & !grepl("179", Treat)   ) %>%
  select(patient, Month) %>% group_by(patient) %>% filter(Month==min(Month)) %>% rename("Drop"="Month") %>%
  left_join(CAN_Drug_Histories)




CAN_Drug_Histories %>% select(patient, weight) %>% ungroup() %>% distinct() %>% summarise(n=sum(weight))
temp %>% select(patient, weight) %>% ungroup() %>% distinct() %>% summarise(n=sum(weight))
  
To_Target %>% inner_join(temp) %>% select(patient, weight) %>% ungroup() %>% distinct() %>% summarise(n=sum(weight))
Remain_no_Target %>% inner_join(temp) %>% select(patient, weight) %>% ungroup() %>% distinct() %>% summarise(n=sum(weight))

length(unique(temp$patient))

To_Target <- temp %>% filter(Month>=Drop) %>% filter(grepl("179", Treat) | grepl(string_target, Treat)) %>%
  select(patient) %>% distinct()

Remain_no_Target <- temp %>% select(patient) %>% distinct() %>% anti_join(To_Target)

plot <- To_Target %>% left_join(temp) %>% filter(Month>=Drop) %>% 
  filter(grepl("179", Treat) | grepl(string_target, Treat)) 

plot %>%
  group_by(patient, weight) %>% filter(Month==min(Month)) %>% 
  mutate(Elapsed=Month-Drop) %>% 
  ungroup() %>% 
  ggplot(aes(disease, Elapsed)) +
  geom_violin(alpha=0.5, fill="deepskyblue4") +
  geom_boxplot(alpha=0.5,  fill="deepskyblue4", notch = TRUE, size=1.5, width=0.5) +
  theme_minimal() +
  xlab("") + ylab("Number of Months From \n Chemo STOP to Target START \n")
  


To_Target %>% mutate(group="Started_Target") %>%
  bind_rows(Remain_no_Target %>% mutate(group="No")) %>%
  inner_join(Last) %>%
    filter( (test=="Albumin"&value>1&value<5)|
            (test=="Labs_Neutrophiles"&value>1&value<14)|
            (test=="BMI"&value>15&value<60) |
            (test=="POTASSIUM"&value>2&value<6)|
            (test=="CALCIUM"&value>6.2&value<11.2)|
            (test=="Hemoglobin"&value>5&value<16)|
            (test=="AST"&value>0&value<100) | 
            (test=="ALT"&value>0&value<100) | 
            (test=="ALP"&value>0&value<250) ) %>%
  ggplot(aes(value, colour=group, fill=group)) + 
  geom_density(alpha=0.5) +
  facet_wrap(~test, scales="free") +
  theme_minimal() +
  xlab("\n LAST Lab test result observed") +
  ylab("Patient density \n") +
  scale_colour_manual(values=c("#66A4B9", "#C86060")) +
  scale_fill_manual(values=c("#66A4B9", "#C86060" ))



Palbo <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()
CAN_Drug_Histories <- Palbo %>% left_join(CAN_Drug_Histories)


CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)


string_Cancer <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" )], collapse = "|"),")\\b")


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(grepl(string_Cancer, Treat))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease) %>% arrange(patient, weight, Month, Treat) %>%
  group_by(patient, weight, Month) %>% mutate(Treat=paste0(Treat, collapse=",")) %>% distinct()



PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics%>% select(patid, cancer_metastasis) 
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]
PONS_Demographics <- PONS_Demographics %>% drop_na()

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("Mets"="Exact_Month")


CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Demographics, by=c("patient"="patid")) %>%
  filter(Month>=Mets)


data.frame(CAN_Drug_Histories %>% select(patient, Treat) %>% distinct() %>% filter(Treat!="-") %>%
  group_by(patient) %>% mutate(line=row_number()) %>%
  left_join(CAN_Drug_Histories)) %>%
  left_join(
   CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>%  group_by(patient) %>% filter(Month==min(Month)) %>%
     select(patient, Month) %>% rename("Start"="Month")
  ) %>%
  left_join(
       CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>%  group_by(patient) %>% count()
  ) %>% filter(Month==Start) %>%  # summarise(meanline=mean(line), medianline=median(line)) %>% 
  ggplot(aes(line, n)) +
  #geom_jitter() +
  xlim(0,16) +
  theme_minimal() +
  geom_smooth(colour="black", fill="black",level = 0.8) +
  xlab("\n Therapy Line at which \n Palbociclib was initiated \n ") +
  ylab("Number of Months spent ON Palbo \n")



# -----------

# Stock month over month All breast cancer metastatic NAIVE vs EXP before mets GET GROUPS  -------------------------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")


string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")


CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% rename("patient"="patid"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, metastasis_onset, Month, Treat, ON_Palbo)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% 
  mutate(Drug_Exp=cumsum(grepl(string_target, Treat)|
                           grepl(string_OtherChemo, Treat)|
                           grepl(string_Hormonal, Treat))) %>%
  mutate(Drug_Exp=ifelse(Drug_Exp>=1,1,0))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Box=ifelse(grepl("179", Treat), "Palbo",
                                         ifelse(grepl(string_target, Treat), "OtherTarget",
                                                ifelse(grepl(string_OtherChemo, Treat), "OtherChemo",
                                                       ifelse(grepl(string_Hormonal, Treat), "Hormonal", "Lapsed")))))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ElapsedMets=Month-metastasis_onset)



groups <- CAN_Drug_Histories %>% filter(ElapsedMets==(-1)) %>% select(patient, Drug_Exp) %>% ungroup()

Exp_vs_naive <- groups

data.frame(CAN_Drug_Histories %>%
             inner_join(groups %>% filter(Drug_Exp==1) %>% select(patient)) %>%
  mutate(Box=ifelse(Box=="Lapsed"&Drug_Exp ==1, "Lapsed",
                               ifelse(Box=="Lapsed"&Drug_Exp==0, "Naive", Box))) %>%
  group_by(ElapsedMets, Box) %>% summarise(n=sum(weight)) %>%
  spread(key=Box, value=n))
           


CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% select(patient) %>% distinct() %>%
  left_join(CAN_Drug_Histories) %>% filter(ON_Palbo==1) %>% group_by(patient)  %>%
  filter(ElapsedMets==min(ElapsedMets)) %>%
  select(patient, ElapsedMets) %>%
  left_join(
    CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% group_by(patient) %>% count()
  ) %>% ungroup() %>%
  filter(ElapsedMets>=0) %>%
  ggplot(aes(ElapsedMets, n)) +
  geom_jitter(size=0.5, alpha=0.5, colour="deepskyblue4") +
  geom_smooth(colour="black", fill="black") +
  theme_minimal() +
  xlim(0,24) +
  xlab("\n Month of Palbociclib Initiation \n (Relative to Metastasis)") +
  ylab("Number of Month Spent ON Palbociclib \n")


# Compare the red series NAIVE vs EXT, BEFORE vs AFTER

# CAN_Drug_Histories
# 
# Exp_vs_naive %>% inner_join(CAN_Drug_Histories) %>%
#   filter(OtherTarget==1) %>%
#   filter(Period<0)
# 
# 
# ignore_df <- Exp_vs_naive %>% inner_join(CAN_Drug_Histories) %>%
#   filter(OtherTarget==1) %>%
#   select(patient, Drug_Exp, Treat)
# 
# ignore_df <- separate_rows(ignore_df, Treat, sep = ",", convert=T)
# 
# 
# ignore_df %>% select(patient, Drug_Exp) %>% distinct() %>% group_by(Drug_Exp) %>% count()
# 
# data.frame(ignore_df %>% select(patient, Drug_Exp, Treat) %>% distinct() %>%
#   group_by(Drug_Exp, Treat) %>% count() %>% distinct() %>%
#   spread(Drug_Exp, value=n))
#  
# ignore_df <- Exp_vs_naive %>% filter(Drug_Exp==1) %>%
#   inner_join(CAN_Drug_Histories) %>%
#   filter(OtherTarget==1) %>% mutate(Period=ifelse(Period<0, "Before", "After")) %>%
#   select(patient, Period, Treat) %>% distinct()
# 
# ignore_df %>% select(patient, Period) %>% distinct() %>% group_by(Period) %>% count()
# 
# ignore_df <- separate_rows(ignore_df, Treat, sep = ",", convert=T)
# 
# data.frame(ignore_df %>% select(patient, Period, Treat) %>% distinct() %>%
#   group_by(Period, Treat) %>% count() %>% distinct() %>%
#   spread(Period, value=n) %>% filter(grepl(string_target, Treat)))
 
# 

# -------------
# Flows every 12 months before after mets Therapy Lines Breast Cancer With Rank 12m VIZ  EXP vs NAIVE at METS ---------------------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
setDT(New_Primary_Cancer_Box)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer %in% c("Breast Cancer"), .(patid, Primary_Cancer)]

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics[ , .(patid, cancer_metastasis)]
PONS_Demographics <- PONS_Demographics[!is.na(cancer_metastasis)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

PONS_Demographics <- PONS_Demographics[
  Months_lookup, on = c("cancer_metastasis" = "Month")
  ][
    ,.SD, .SDcols = !c("cancer_metastasis")
    ][, {setnames(.SD, old = "Exact_Month", new = "cancer_metastasis")}]

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics, by=c("patient"="patid")) %>% 
  select(-disease) %>% filter(Month>= (cancer_metastasis-12) )


CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% mutate(Month2=Month-cancer_metastasis) %>% 
  select(-c(Month, cancer_metastasis))


CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month2>=(-12)) %>% filter(Month2<=12) %>%
  group_by(patient) %>% count() %>%
  filter(n==25) %>% select(patient) %>%
  left_join(CAN_Drug_Histories)  %>% filter(Month2>=(-12)) %>% filter(Month2<=12)

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)

unique(PONS_Ingredients_JN_ChemoClass$generic_name)

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

string_CDK <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Palbociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Ribociclib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Abemaciclib"], collapse = "|"),")\\b")


string_EveroFul <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Everolimus"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Fulvestrant"], collapse = "|"),")\\b")


string_Niche <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$generic_name=="Alpelisib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Elacestrant"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Olaparib"|
                                                                             PONS_Ingredients_JN_ChemoClass$generic_name=="Talazoparib"], collapse = "|"),")\\b")


string_Dead <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$drug_group=="Death"], collapse = "|"),")\\b")


string_OtherTarget <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[ (PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"|
                                                                                       PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic") &
                                                                                       PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"], collapse = "|"),")\\b")



string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$drug_group!="Death"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Everolimus"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Fulvestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Palbociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Ribociclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Abemaciclib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Alpelisib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Elacestrant"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Olaparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$generic_name!="Talazoparib"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"], collapse = "|"),")\\b")


range(CAN_Drug_Histories$Month2)
# 
# CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Period=
#                                                       ifelse(Month2<0,1,
#                                                       ifelse(Month2<=6,2, 
#                                                       ifelse(Month2<=12,3,
#                                                       ifelse(Month2<=18,4,NA)))))


CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Period=Month2)

CAN_Drug_Histories <- CAN_Drug_Histories %>% # filter(Treat != "-") %>%
  mutate(Dead=ifelse(grepl(string_Dead, Treat), 1, 0)) %>%
  mutate(Hormone=ifelse(grepl(string_Hormonal, Treat), 1, 0)) %>%
  mutate(CDK=ifelse(grepl(string_CDK, Treat), 1, 0)) %>%
  mutate(OtherTarget=ifelse(grepl(string_OtherTarget, Treat), 1, 0)) %>%
  mutate(OtherCCh=ifelse(grepl(string_OtherChemo, Treat), 1, 0)) %>%
  mutate(EveroFul=ifelse(grepl(string_EveroFul, Treat), 1, 0)) %>%
  mutate(Niche=ifelse(grepl(string_Niche, Treat), 1, 0)) 

# CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Dead==1|Hormone==1|CDK==1|OtherTarget==1|OtherCCh==1|EveroFul==1|Niche==1)

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(rank=
                                                      ifelse(Dead==1, 1,
                                                      ifelse(Niche==1, 2,
                                                      ifelse(EveroFul==1, 3,
                                                      ifelse(CDK==1,4, 
                                                      ifelse(OtherTarget==1,5,
                                                      ifelse(Hormone==1,6,
                                                      ifelse(OtherCCh==1,7,8))))))))

temp <- CAN_Drug_Histories %>% select(patient, weight, Month2, Period, rank)

temp <- temp %>% group_by(patient, Period) %>% mutate(cumrank=cummin(rank))

groups <- fread("Source/Groups_HR_HER2_status.txt")
groups <- groups %>% filter(group=="HRPosHER2Neg") %>% select(patid) %>% rename("patient"="patid") 
#groups <- groups %>% filter(group=="HRNegHER2Pos") %>% select(patid) %>% rename("patient"="patid") 
#groups <- groups %>% filter(group=="HRPosHER2Pos") %>% select(patid) %>% rename("patient"="patid") 

temp2 <- temp %>% mutate(cumrank=paste0("S", cumrank)) %>% 
  inner_join(groups) %>%
  select(patient, Period, cumrank) %>% mutate(Period=paste0("M_", Period)) %>% ungroup() %>%
  spread(key=Period, value=cumrank) %>% 
  select(patient, `M_-12`, `M_-11`, `M_-10`, `M_-9`, `M_-8`, `M_-7`, `M_-6`, `M_-5`, `M_-4`, `M_-3`,`M_-2`,`M_-1`,`M_0`,
                                               `M_1`,`M_2`,`M_3`,`M_4`,`M_5`,`M_6`,`M_7`,`M_8`,`M_9`,`M_10`,`M_11`,`M_12`)

temp_exp <- temp2 %>% inner_join(Exp_vs_naive %>% filter(Drug_Exp==1) %>% select(patient))
temp_naive <- temp2 %>% inner_join(Exp_vs_naive %>% filter(Drug_Exp==0) %>% select(patient))

fwrite(temp_exp, "temp_for_Sankey_HRPosHER2Neg_ExpMets_12m.csv", sep=",")
fwrite(temp_naive, "temp_for_Sankey_HRPosHER2Neg_NaiveMets_12m.csv", sep=",")

# ----------

# Compare Naive at Mets vs Exp at Mets -------------
Exp_vs_naive
Exp_vs_naive %>% group_by(Drug_Exp) %>% count()

#   Drug_Exp     n
# 1        0 17149
# 2        1 16145


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics <- PONS_Demographics %>% inner_join(Exp_vs_naive %>% select(patient), by=c("patid"="patient"))

PONS_Demographics <-  PONS_Demographics %>%
  group_by(patid) %>% count() %>% filter(n>=2) %>% select(patid) %>%
  left_join(PONS_Demographics) %>% filter(diagnosis != "Breast")

data.frame(PONS_Demographics %>% inner_join(Exp_vs_naive, by=c("patid"="patient")) %>%
  group_by(diagnosis, Drug_Exp) %>% count() %>% spread(key=Drug_Exp, value=n) %>%
  mutate(`0`=ifelse(is.na(`0`),0,`0`)) %>% mutate(`1`=ifelse(is.na(`1`),0,`1`))) 
  


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, died, cancer_onset, death_date)

PONS_Demographics$cancer_onset <- as.Date(PONS_Demographics$cancer_onset)
PONS_Demographics$death_date  <- as.Date(PONS_Demographics$death_date)

missingDeathDay <- ymd("2025-07-31")

PONS_Demographics <- PONS_Demographics %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics <- PONS_Demographics %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))

Exp_vs_naive %>% inner_join(PONS_Demographics, by=c("patient"="patid")) %>% group_by(Drug_Exp) %>% summarise(mean=mean(Survived))

#   Drug_Exp  mean
# 1        0 1689.
# 2        1 1702.

groups_to_compare <- groups_to_compare %>% inner_join(PONS_Demographics, by=c("patient"="patid"))

Exp_vs_naive %>% inner_join(PONS_Demographics, by=c("patient"="patid")) %>%
  mutate(status=ifelse(died=="Y",1,0)) %>%  
  group_by(Drug_Exp, died) %>% count()

#   Drug_Exp died      n
# 1        0 N     15031
# 2        0 Y      2118
# 3        1 N     13897
# 4        1 Y      2248



CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(Exp_vs_naive)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, Treat, Drug_Exp)])
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
CAN_Drug_Histories <- CAN_Drug_Histories %>% distinct() %>% filter(Treat!="-")
PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class)
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule")) %>% select(-Treat) %>% distinct()

data.frame(CAN_Drug_Histories %>% group_by(Drug_Exp, chemo_class) %>% count() %>%
  spread(key=Drug_Exp, value=n)) %>% mutate(X0=X0/17149, X1=X1/16145)


PONS_Comorbidity_Inventories <- fread("Source/PONS Comorbidity Inventories.txt")
PONS_Comorbidity_Inventories <- PONS_Comorbidity_Inventories %>% select(patid, diagnosis) %>% 
  inner_join(Exp_vs_naive, by=c("patid"="patient"))

ignore <- data.frame(PONS_Comorbidity_Inventories %>% group_by(Drug_Exp, diagnosis) %>% count() %>%
  spread(key=Drug_Exp, value=n))

ignore[is.na(ignore)] <- 0

ignore %>% filter(X1>1000) %>% mutate(X0=round(X0/17149,2), X1=round(X1/16145, 2))

# --------

# Inflows and Outflows to palbo over time - How many drugs of each class? ---------


CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

# 
# string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
#   PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")
# 
# 
# string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
#                                                                                      PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
#                                                                                PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
#                                                                                PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
#                                                                                  PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")
# 
# string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
#   PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")


Palbociclib_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()

CAN_Drug_Histories <- Palbociclib_pats %>% inner_join(CAN_Drug_Histories) 

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(ON_Palbo==1) %>% group_by(patient) %>% filter(Month==min(Month)) %>%
  select(patient, Month) %>% distinct() %>% rename("First_Palbo"="Month") %>%
  left_join(CAN_Drug_Histories)

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month<=First_Palbo)
CAN_Drug_Histories <- CAN_Drug_Histories %>% arrange(patient, Month)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% rename("patient"="patid"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Month=Month-metastasis_onset) %>% select(-metastasis_onset)

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-First_Palbo)

CAN_Drug_Histories %>% filter(ON_Palbo==1) %>%
  group_by(Month) %>% summarise(starts=sum(weight)) %>%
  ggplot(aes(Month, starts)) + geom_col() + xlim(0,36)


working_df <- CAN_Drug_Histories %>% select(patient, Month, Treat) 
working_df <- separate_rows(working_df, Treat, sep = ",", convert=T)
working_df <- working_df %>% mutate(Exp=1) %>% spread(key=Treat, value=Exp)
working_df[is.na(working_df)] <- 0
working_df <- working_df %>% select(-`-`)
range(working_df$Month)

working_df_duplicate <- working_df[, 3:118]

names(working_df_duplicate) <- paste0("CUM_", names(working_df_duplicate))

working_df_duplicate[] <- 0

working_df <- cbind(working_df, working_df_duplicate)

cumulative_cols <-  working_df %>%
  group_by(patient) %>%
  mutate(across(3:118, cumsum)) %>%
  select(patient, 3:118) %>% ungroup() %>% select(-patient)

cumulative_cols <- ifelse(cumulative_cols > 0, 1, 0)

working_df[, 119:234] <- cumulative_cols


data.frame(
  working_df %>% filter(`131`==1) %>% select(patient) %>% distinct() %>%
  left_join(working_df)  %>% select(patient, Month, `131`, `CUM_131`)
  )

working_df <- working_df[, c(1, 2, 119:234)]

data.frame(names(working_df)[3:118]) %>%
  mutate(`names.working_df..3.118.`=parse_number(names.working_df..3.118.)) %>%
  left_join(
    PONS_Ingredients_JN_ChemoClass %>% select(molecule, chemo_class) %>% distinct() %>% mutate(molecule=as.numeric(molecule))
  , by=c("names.working_df..3.118."="molecule")
  ) %>% arrange(chemo_class)  

working_df$Alkylating <- working_df$CUM_133 + working_df$CUM_314

working_df$Androgen <- working_df$CUM_326 + working_df$CUM_332

working_df$Antidepressant <- working_df$CUM_339 + working_df$CUM_340 + working_df$CUM_341 + working_df$CUM_342

working_df$Antiemetic <- working_df$CUM_1 + working_df$CUM_10 + working_df$CUM_11 + working_df$CUM_12 +
   working_df$CUM_14  + working_df$CUM_15 + working_df$CUM_16 + working_df$CUM_18 + working_df$CUM_19 +
   working_df$CUM_5 + working_df$CUM_6 + working_df$CUM_7 + working_df$CUM_9

working_df$Antimetabolites <- working_df$CUM_124 + working_df$CUM_128 + working_df$CUM_160 + working_df$CUM_64 +
   working_df$CUM_81  

working_df$Antimicrotubule <- working_df$CUM_105 + working_df$CUM_115 + working_df$CUM_143 + working_df$CUM_177 +
   working_df$CUM_241 

working_df$Antipsychotic <- working_df$CUM_343 + working_df$CUM_344 + working_df$CUM_346 + working_df$CUM_347 +
   working_df$CUM_349 + working_df$CUM_350 + working_df$CUM_351

working_df$Appetite <- working_df$CUM_319 + working_df$CUM_321 

working_df$Biologic <- working_df$CUM_246 + working_df$CUM_255  + working_df$CUM_265  + working_df$CUM_293 +
    working_df$CUM_297  + working_df$CUM_298  + working_df$CUM_308

working_df$Cannabinoid <- working_df$CUM_322 + working_df$CUM_323  

working_df$Chemoprotective <- working_df$CUM_22 + working_df$CUM_23  + working_df$CUM_24  + working_df$CUM_28 +
   working_df$CUM_30 + working_df$CUM_32 + working_df$CUM_34 + working_df$CUM_37 + working_df$CUM_38 +  
   working_df$CUM_39 + working_df$CUM_40 + working_df$CUM_42 

working_df$Corticosteroid <- working_df$CUM_335 + working_df$CUM_336  + working_df$CUM_337  + working_df$CUM_338

working_df$Growth  <- working_df$CUM_334 

working_df$Hormonal <- working_df$CUM_103 + working_df$CUM_112  + working_df$CUM_120  + working_df$CUM_126 +
  working_df$CUM_131 + working_df$CUM_145 + working_df$CUM_151 + working_df$CUM_153 + working_df$CUM_173 + 
  working_df$CUM_212 + working_df$CUM_226 + working_df$CUM_231 + working_df$CUM_305 + working_df$CUM_58 +  
  working_df$CUM_70 + working_df$CUM_79 

working_df$Hospital <- working_df$CUM_353 + working_df$CUM_352

working_df$Immuno <- working_df$CUM_119 + working_df$CUM_138  + working_df$CUM_139 + working_df$CUM_147 +
   working_df$CUM_168  + working_df$CUM_174 + working_df$CUM_179 + working_df$CUM_197 + working_df$CUM_216 + 
   working_df$CUM_229 + working_df$CUM_46 + working_df$CUM_49 + working_df$CUM_54 + working_df$CUM_99

working_df$Nutrition <- working_df$CUM_317

working_df$Other <- working_df$CUM_152 + working_df$CUM_245 + working_df$CUM_60

working_df$PD1 <- working_df$CUM_251 + working_df$CUM_292 

working_df$Platinum <- working_df$CUM_312 + working_df$CUM_313

working_df$Progestin <- working_df$CUM_324 + working_df$CUM_325

working_df$Radio <- working_df$CUM_309 + working_df$CUM_310 + working_df$CUM_311

working_df$Surgery <- working_df$CUM_354 

working_df$Topoisomerase <- working_df$CUM_106 + working_df$CUM_113 + working_df$CUM_118  + working_df$CUM_225



working_df <- working_df[, c(1, 2, 119:142)]


CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(working_df)


temp <- data.frame(
  data.frame(CAN_Drug_Histories %>% filter(ON_Palbo==1) %>%
  group_by(Month) %>% summarise(starts=sum(weight))) %>%
  left_join(
  CAN_Drug_Histories %>%
  filter(ON_Palbo == 1) %>%
  group_by(Month) %>%
  summarise(across(Alkylating:Topoisomerase, \(x) mean(x, na.rm = TRUE)))
  )
)

fwrite(temp, "temp.csv")

names(CAN_Drug_Histories)

data.frame(CAN_Drug_Histories %>% mutate(Target_tot=Biologic+Immuno) %>% filter(ON_Palbo==1) %>%
  mutate(Target_tot=ifelse(Target_tot>=3,3,Target_tot)) %>%
  group_by(Month, Target_tot) %>% summarise(n=sum(weight)) %>% spread(key=Target_tot, value=n)) 


data.frame(CAN_Drug_Histories %>% mutate(Chemo_tot=Alkylating+Antimetabolites+Antimicrotubule+Other+PD1+Platinum+Topoisomerase) %>% filter(ON_Palbo==1) %>%
  mutate(Chemo_tot=ifelse(Chemo_tot>=3,3,Chemo_tot)) %>%
  group_by(Month, Chemo_tot) %>% summarise(n=sum(weight)) %>% spread(key=Chemo_tot, value=n)) 



data.frame(CAN_Drug_Histories %>% filter(ON_Palbo==1) %>%
  mutate(Hormonal=ifelse(Hormonal>=3,3,Hormonal)) %>%
  group_by(Month, Hormonal) %>% summarise(n=sum(weight)) %>% spread(key=Hormonal, value=n)) 





#  --------------------------------
# Number of metastasis vs metastasis site - All Breast Mets vs palbociclib --------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)



PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, cancer_metastasis) %>% inner_join(New_Primary_Cancer_Box)
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis)

PONS_Demographics <- PONS_Demographics %>% group_by(patid) %>% count() %>%
  left_join(PONS_Demographics)

data.frame(PONS_Demographics %>% mutate(n=n-1) %>%
  mutate(n=ifelse(n>=8,8,n)) %>% group_by(diagnosis, n) %>% summarise(tot=sum(weight)) %>%
  spread(key=n, value=tot))

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% select(patid), by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Treat) %>% distinct() %>% filter(Treat!="-")
Palbo_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct() %>% rename("patid"="patient")

data.frame(PONS_Demographics %>% inner_join(Palbo_pats) %>% mutate(n=n-1) %>%
  mutate(n=ifelse(n>=8,8,n)) %>% group_by(diagnosis, n) %>% summarise(tot=sum(weight)) %>%
  spread(key=n, value=tot))


# --------
# Procedures ----------------------------

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics) 

length(unique(PONS_Demographics$patid)) # 49333

Breast_Cancer_Pts_Procd_events <- fread("Source/Breast_Cancer_Pts_Procd_events.txt", sep=",")

library("readxl")
Procedures_lookup_table_flags <- read_excel("Source/Procedures_lookup_table_flags.xlsx")
Procedures_lookup_table_flags <- Procedures_lookup_table_flags %>% select(proc_cd, category_dtl_code_desc, proc_desc)

Breast_Cancer_Pts_Procd_events <- Procedures_lookup_table_flags %>% 
  inner_join(Breast_Cancer_Pts_Procd_events) %>% inner_join(PONS_Demographics)

Breast_Cancer_Pts_Procd_events <- Breast_Cancer_Pts_Procd_events %>% select(4,6,5,1,2,3)

length(unique(Breast_Cancer_Pts_Procd_events$patid)) # 48723

category_dtl_code_desc_nums <- data.frame(Breast_Cancer_Pts_Procd_events %>% select(patid, category_dtl_code_desc) %>% distinct() %>%
  group_by(category_dtl_code_desc) %>% count() %>% arrange(-n) %>% mutate(perc=round(n/49333,2)))

proc_desc_nums <- data.frame(Breast_Cancer_Pts_Procd_events %>% select(patid, proc_desc  ) %>% distinct() %>%
  group_by(proc_desc  ) %>% count() %>% arrange(-n) %>% mutate(perc=round(n/49333,2)))



CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% select(patid), by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Treat) %>% distinct() %>% filter(Treat!="-")
Palbo_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct() %>% rename("patid"="patient")
length(unique(Palbo_pats$patid)) # 3151

data.frame(Breast_Cancer_Pts_Procd_events %>% inner_join(Palbo_pats) %>% select(patid, category_dtl_code_desc) %>% distinct() %>%
  group_by(category_dtl_code_desc) %>% count() %>% arrange(-n) %>% mutate(perc=round(n/3151,2)) %>% rename("perc_palbo"="perc") %>% select(-n)) %>%
full_join(
  data.frame(Breast_Cancer_Pts_Procd_events  %>% select(patid, category_dtl_code_desc) %>% distinct() %>%
  group_by(category_dtl_code_desc) %>% count() %>% arrange(-n) %>% mutate(perc=round(n/49333,2)) %>% rename("perc_all"="perc") %>% select(-n))
)


#           category_dtl_code_desc perc_palbo perc_all
# 1                   CT SCAN CHEST       0.90     0.57
# 2                 CT SCAN ABDOMEN       0.88     0.60
# 3     OTHER DIAGNOSTIC ULTRASOUND       0.83     0.83
# 4      MAGNETIC RESONANCE IMAGING       0.78     0.68
# 5   NUCLEAR MEDICINE IMAGING BONE       0.63     0.25
# 6  DIAGNOSTIC ULTRASOUND OF HEART       0.61     0.60
# 7                     MAMMOGRAPHY       0.61     0.79
# 8                    NOT ASSIGNED       0.59     0.57
# 9                   OTHER CT SCAN       0.57     0.35
# 10          CT SCAN HEAD AND NECK       0.45     0.36
# 11     DIAG ULTRASOUND OF ABDOMEN       0.39     0.36
# 12 BREAST BIOPSY AND OTHER DIAGNO       0.33     0.45
# 13         COLONOSCOPY AND BIOPSY       0.28     0.44
# 14   DIAG ULTRASOUND OF HEAD/NECK       0.25     0.32
# 15                BIOPSY OF LIVER       0.15     0.04
# 16             BONE MARROW BIOPSY       0.05     0.03
# 17  NUCLEAR MED IMAGING PULMONARY       0.04     0.02
# 18          LAPAROSCOPY (GI ONLY)       0.01     0.01
# 19 OTHER NUCLEAR MEDICINE IMAGING       0.01     0.01
# 20    DIAG ULTRASOUND OF GI TRACT       0.00     0.00
# 21  DIAG ULTRASOUND URINARY TRACT       0.00     0.00
# 22         EXPLORATORY LAPAROTOMY       0.00     0.00



# ---------------
# Procedures over time relative to metastasis/Palbociclib ----------------------------

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)

PONS_Demographics <- New_Primary_Cancer_Box %>% inner_join(PONS_Demographics) 

length(unique(PONS_Demographics$patid)) # 49333

Breast_Cancer_Pts_Procd_events <- fread("Source/Breast_Cancer_Pts_Procd_events.txt", sep=",")

library("readxl")
Procedures_lookup_table_flags <- read_excel("Source/Procedures_lookup_table_flags.xlsx")
Procedures_lookup_table_flags <- Procedures_lookup_table_flags %>% select(proc_cd, category_dtl_code_desc, proc_desc)

Breast_Cancer_Pts_Procd_events <- Procedures_lookup_table_flags %>% 
  inner_join(Breast_Cancer_Pts_Procd_events) %>% inner_join(PONS_Demographics)

Breast_Cancer_Pts_Procd_events <- Breast_Cancer_Pts_Procd_events %>% select(4,6,5,1,2,3)

length(unique(Breast_Cancer_Pts_Procd_events$patid)) # 48723

unique(Breast_Cancer_Pts_Procd_events$category_dtl_code_desc )

Breast_Cancer_Pts_Procd_events %>% mutate(date=as.Date(date), cancer_metastasis=as.Date(cancer_metastasis)) %>%
  mutate(Elapsed=as.numeric(date-cancer_metastasis)/30.5 ) %>% 
  mutate(Elapsed=round(Elapsed)) %>%
  filter(category_dtl_code_desc == "OTHER CT SCAN")  %>%
  group_by(Elapsed) %>% count() %>%
  ggplot(aes(Elapsed, n)) +
  geom_col(fill="midnightblue") +
  theme_minimal() +
  xlab("\n Number of Months since Metastasis Onset") + ylab("Mammography \n")



CAN_Doses <- fread("Source/CAN Doses.txt")

CAN_Doses <- CAN_Doses%>% filter(generic_name=="Palbociclib") %>% select(pat_id, from_dt) 
CAN_Doses <- CAN_Doses %>% group_by(pat_id) %>% summarise(from_dt=min(from_dt))
CAN_Doses$from_dt <- as.Date(CAN_Doses)


Breast_Cancer_Pts_Procd_events %>%
  inner_join(CAN_Doses, by=c("patid"="pat_id")) %>%
  mutate(from_dt=as.Date(from_dt), cancer_metastasis=as.Date(cancer_metastasis)) %>%
  mutate(Elapsed=as.numeric(from_dt-cancer_metastasis)/30.5 ) %>% 
  mutate(Elapsed=round(Elapsed)) %>%
  filter(category_dtl_code_desc == "OTHER CT SCAN")  %>%
  group_by(Elapsed) %>% count() %>%
  ggplot(aes(Elapsed, n)) +
  geom_col(fill="firebrick") +
  theme_minimal() +
  xlab("\n Number of Months since First Palbociclib") + ylab("Mammography \n")

# --------
# Dendogram -----------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, cancer_metastasis) %>% inner_join(New_Primary_Cancer_Box)
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis)
PONS_Demographics <- PONS_Demographics %>% select(-weight)
setDT(PONS_Demographics)


CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat)])
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
setDT(unique(CAN_Drug_Histories))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
PONS_Ingredients_JN_ChemoClass <- setDT(PONS_Ingredients_JN_ChemoClass)[indication == "Cancer", .(molecule, chemo_class)]
PONS_Ingredients_JN_ChemoClass$molecule <- as.numeric(PONS_Ingredients_JN_ChemoClass$molecule)
CAN_Drug_Histories <- CAN_Drug_Histories %>% left_join(PONS_Ingredients_JN_ChemoClass, by=c("Treat"="molecule"))
CAN_Drug_Histories <- CAN_Drug_Histories %>% drop_na() %>% select(-Treat) %>% distinct()


#names(PONS_Demographics)[2] <- "class"
#names(PONS_Demographics)[1] <- "patient"

names(CAN_Drug_Histories)[2] <- "class"

#df_work<- PONS_Demographics %>% bind_rows(CAN_Drug_Histories)

df_work<- CAN_Drug_Histories

df_work$exp <- 1

df_work <- df_work %>% spread(key=class, value=exp)
df_work[is.na(df_work)] <- 0
names(df_work)
#df_work <- df_work %>% select(-Breast)
#df_work <- df_work %>% select(-Prostate)
df_work <- df_work %>% select(-c(Death, `Hospital Inpatient`))

dim(df_work)
data_matrix <- as.matrix(df_work[1:10000, -1]) 

dist_matrix <- dist(data_matrix, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "ward.D2")

plot(hclust_result, hang = -1, cex = 0.8, main = "Hierarchical Clustering Dendrogram", labels = FALSE)

class_order <- hclust_result$order
class_labels <- colnames(data_matrix)[class_order]

heatmap(data_matrix[class_order, ], Rowv = NULL, Colv = NULL, col = c("#fff9f0", "firebrick"),
        labRow = class_labels, labCol = NULL, main = "Heatmap of Classes")


# k <- 8  
# kmeans_result <- kmeans(data_matrix, centers = k)
# 
# library(RColorBrewer)
# colors <- brewer.pal(k, "Set1")
# plot(data_matrix, col = colors[kmeans_result$cluster], pch = 19, main = "K-Means Clustering")
# 
# heatmap(data_matrix, Rowv = as.dendrogram(hclust_result), Colv = NA, col = heat.colors(256))

# ------
# Number of mets vs sites vs palbo exp vs survived summary --------
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, cancer_metastasis) %>% inner_join(New_Primary_Cancer_Box)
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis)
setDT(PONS_Demographics)

PONS_Demographics <- PONS_Demographics %>% group_by(patid)  %>% count() %>% left_join(PONS_Demographics)
PONS_Demographics <- PONS_Demographics %>% mutate(exp=1) %>% spread(key=diagnosis, value=exp)
PONS_Demographics[is.na(PONS_Demographics)] <- 0

PONS_Demographics <- PONS_Demographics %>% mutate(n=n-1) %>% ungroup() %>% filter(n!=0) %>%
  mutate(n=ifelse(n==1,1,2))

PONS_Demographics <- PONS_Demographics %>% select(-Breast)

PONS_Demographics %>% mutate(group=ifelse(n==1&Lung==1, "LLB",
                                          ifelse(n==1&Liver==1, "LLB",
                                                 ifelse(n==1&Bone==1, "LLB",
                                                        ifelse(n==1&Lymphoma==1, "Lymphoma",
                                                               ifelse(n==1&Skin==1, "Skin",
                                                                      ifelse(n==1, "Other",
                                                                             ifelse(n==2&Lung==1, "LLB",
                                                                                    ifelse(n==2&Liver==1, "LLB",
                                                                                           ifelse(n==2&Bone==1, "LLB",
                                                                                                  ifelse(n==2,"Other", NA))))))))))) %>%
  group_by(n, group) %>% summarise(pop=sum(weight))



CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, Treat) %>% distinct()
Palbo_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()


PONS_Demographics %>% inner_join(Palbo_pats, by=c("patid"="patient")) %>% mutate(group=ifelse(n==1&Lung==1, "LLB",
                                          ifelse(n==1&Liver==1, "LLB",
                                                 ifelse(n==1&Bone==1, "LLB",
                                                        ifelse(n==1&Lymphoma==1, "Lymphoma",
                                                               ifelse(n==1&Skin==1, "Skin",
                                                                      ifelse(n==1, "Other",
                                                                             ifelse(n==2&Lung==1, "LLB",
                                                                                    ifelse(n==2&Liver==1, "LLB",
                                                                                           ifelse(n==2&Bone==1, "LLB",
                                                                                                  ifelse(n==2,"Other", NA))))))))))) %>%
  group_by(n, group) %>% summarise(pop=sum(weight))



PONS_Demographics_surv <- fread("Source/PONS Demographics.txt")
PONS_Demographics_surv <- PONS_Demographics_surv %>% select(patid, cancer_onset, cancer_metastasis , death_date )

PONS_Demographics_surv$cancer_onset <- as.Date(PONS_Demographics_surv$cancer_onset)
PONS_Demographics_surv$cancer_metastasis <- as.Date(PONS_Demographics_surv$cancer_metastasis)
PONS_Demographics_surv$death_date  <- as.Date(PONS_Demographics_surv$death_date)

missingDeathDay <- ymd("2025-07-31")

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_surv <- PONS_Demographics_surv %>% inner_join(groups %>% select(patient), by=c("patid"="patient"))

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived_Mets = as.numeric(death_date)-as.numeric(cancer_metastasis)) %>%
  mutate(Survived_Mets=ifelse(Survived_Mets>1825,1825,Survived_Mets))

PONS_Demographics_surv <- PONS_Demographics_surv %>% select(patid, Survived, Survived_Mets) %>% drop_na()



PONS_Demographics %>% inner_join(PONS_Demographics_surv) %>% mutate(group=ifelse(n==1&Lung==1, "LLB",
                                          ifelse(n==1&Liver==1, "LLB",
                                                 ifelse(n==1&Bone==1, "LLB",
                                                        ifelse(n==1&Lymphoma==1, "Lymphoma",
                                                               ifelse(n==1&Skin==1, "Skin",
                                                                      ifelse(n==1, "Other",
                                                                             ifelse(n==2&Lung==1, "LLB",
                                                                                    ifelse(n==2&Liver==1, "LLB",
                                                                                           ifelse(n==2&Bone==1, "LLB",
                                                                                                  ifelse(n==2,"Other", NA))))))))))) %>%
  group_by(n, group) %>% summarise(Suvived=mean(Survived_Mets/30.5))


# -----------


# Month over month relative to mets, durg exp/naive, number of mets, location ----------

# Exp vs naive at first mets 

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")


string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"|PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")


string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")


CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% rename("patient"="patid"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, metastasis_onset, Month, Treat, ON_Palbo)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% 
  mutate(Drug_Exp=cumsum(grepl(string_target, Treat)|
                           grepl(string_OtherChemo, Treat)|
                           grepl(string_Hormonal, Treat))) %>%
  mutate(Drug_Exp=ifelse(Drug_Exp>=1,1,0))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Box=ifelse(grepl("179", Treat), "Palbo",
                                         ifelse(grepl(string_target, Treat), "OtherTarget",
                                                ifelse(grepl(string_OtherChemo, Treat), "OtherChemo",
                                                       ifelse(grepl(string_Hormonal, Treat), "Hormonal", "Lapsed")))))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ElapsedMets=Month-metastasis_onset)



groups <- CAN_Drug_Histories %>% filter(ElapsedMets==(-1)) %>% select(patient, Drug_Exp) %>% ungroup()

Exp_vs_naive <- groups


# Number of mets vs location
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, cancer_metastasis) %>% inner_join(New_Primary_Cancer_Box)
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis)
setDT(PONS_Demographics)

PONS_Demographics <- PONS_Demographics %>% group_by(patid)  %>% count() %>% left_join(PONS_Demographics)
PONS_Demographics <- PONS_Demographics %>% mutate(exp=1) %>% spread(key=diagnosis, value=exp)
PONS_Demographics[is.na(PONS_Demographics)] <- 0

PONS_Demographics <- PONS_Demographics %>% mutate(n=n-1) %>% ungroup() %>% filter(n!=0) %>%
  mutate(n=ifelse(n==1,1,2))

PONS_Demographics <- PONS_Demographics %>% select(-Breast)

# 
# groups_to_track <- PONS_Demographics %>% inner_join(Exp_vs_naive, by=c("patid"="patient")) %>% mutate(group=ifelse(n==1&Lung==1, "LLB",
#                                           ifelse(n==1&Liver==1, "LLB",
#                                                  ifelse(n==1&Bone==1, "LLB",
#                                                         ifelse(n==1&Lymphoma==1, "Lymphoma",
#                                                                ifelse(n==1&Brain==1, "BLIR",
#                                                                       ifelse(n==1&Leukemia==1, "BLIR",
#                                                                              ifelse(n==1&Intestinal==1, "BLIR",
#                                                                                     ifelse(n==1&Reproductive==1, "BLIR",
#                                                                                            ifelse(n==1, "Other",
#                                                                                                   ifelse(n==2&Lung==1&Lymphoma==0, "LLB",
#                                                                                                          ifelse(n==2&Liver==1&Lymphoma==0, "LLB",
#                                                                                                                 ifelse(n==2&Bone==1&Lymphoma==0, "LLB",
#                                                                                                                        ifelse(n==2&Lung==1&Lymphoma==1, "LLB_lymph",
#                                                                                                                               ifelse(n==2&Liver==1&Lymphoma==1, "LLB_lymph",
#                                                                                                                                      ifelse(n==2&Bone==1&Lymphoma==1, "LLB_lymph",
#                                                                                                                                             ifelse(n==2&Lymphoma==1, "Lymphoma",
#                                                                                                                                                    ifelse(n==2&Brain==1,"BLIR",
#                                                                                                                                                           ifelse(n==2&Leukemia==1, "BLIR",
#                                                                                                                                                                  ifelse(n==2&Intestinal==1,"BLIR",
#                                                                                                                                                                         ifelse(n==2&Reproductive==1,"BLIR",
#                                                                                                                                                                                ifelse(n==2,"Other", NA)))))))))))))))))))))) %>%
#   select(patid, weight, Drug_Exp, n, group)


groups_to_track <- PONS_Demographics %>% inner_join(Exp_vs_naive, by=c("patid"="patient")) %>% mutate(group=ifelse(n==1&Lung==1, "LLB",
                                          ifelse(n==1&Liver==1, "LLB",
                                                 ifelse(n==1&Bone==1, "LLB",
                                                        ifelse(n==1&Lymphoma==1, "Lymphoma",
                                                               ifelse(n==1, "Other",
                                                                      ifelse(n==2&Lung==1, "LLB",
                                                                             ifelse(n==2&Liver==1, "LLB",
                                                                                    ifelse(n==2&Bone==1, "LLB",
                                                                                           ifelse(n==2&Lymphoma==1, "Lymphoma",
                                                                                                  ifelse(n==2,"Other", NA))))))))))) %>%
  select(patid, weight, Drug_Exp, n, group)


groups_to_track %>% group_by(Drug_Exp, n, group) %>% summarise(pop=sum(weight))


# SURVIVAL


PONS_Demographics_surv <- fread("Source/PONS Demographics.txt")
PONS_Demographics_surv <- PONS_Demographics_surv %>% select(patid, cancer_onset, cancer_metastasis , death_date )

PONS_Demographics_surv$cancer_onset <- as.Date(PONS_Demographics_surv$cancer_onset)
PONS_Demographics_surv$cancer_metastasis <- as.Date(PONS_Demographics_surv$cancer_metastasis)
PONS_Demographics_surv$death_date  <- as.Date(PONS_Demographics_surv$death_date)

missingDeathDay <- ymd("2021-07-31")

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_surv <- PONS_Demographics_surv %>% inner_join(groups %>% select(patient), by=c("patid"="patient"))

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
  mutate(Survived=ifelse(Survived>1825,1825,Survived))

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived_Mets = as.numeric(death_date)-as.numeric(cancer_metastasis)) %>%
  mutate(Survived_Mets=ifelse(Survived_Mets>1825,1825,Survived_Mets))

PONS_Demographics_surv <- PONS_Demographics_surv %>% select(patid, Survived, Survived_Mets) %>% drop_na()

groups_to_track %>% inner_join(PONS_Demographics_surv) %>% group_by(Drug_Exp,n,group) %>% summarise(Survived=mean(Survived/30.5)) 
groups_to_track %>% inner_join(PONS_Demographics_surv) %>% group_by(Drug_Exp,n,group) %>% summarise(Survived=mean(Survived_Mets/30.5)) 



CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="-")
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, Treat) %>% distinct() %>% 
  inner_join(groups_to_track %>% select(patid), by=c("patient"="patid"))

Palbo_pats <- CAN_Drug_Histories %>% filter(grepl("179",Treat)) %>% select(patient) %>% distinct()

Palbo_pats %>% inner_join(groups_to_track, by=c("patient"="patid")) %>% 
  group_by(Drug_Exp, n, group) %>% summarise(pop=sum(weight))


# Stock Month over month 

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")


string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")

string_Biologic <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"], collapse = "|"),")\\b")

string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ON_Palbo=ifelse(grepl("179", Treat), 1,0))

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")


CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% rename("patient"="patid"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, metastasis_onset, Month, Treat, ON_Palbo)

CAN_Drug_Histories <- CAN_Drug_Histories %>% group_by(patient) %>% 
  mutate(Drug_Exp=cumsum(grepl(string_target, Treat)|
                           grepl(string_Biologic, Treat)|
                           grepl(string_OtherChemo, Treat)|
                           grepl(string_Hormonal, Treat))) %>%
  mutate(Drug_Exp=ifelse(Drug_Exp>=1,1,0))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Box=ifelse(grepl("179", Treat), "Palbo",
                                         ifelse(grepl(string_target, Treat), "OtherTarget",
                                                ifelse(grepl(string_Biologic, Treat), "Biologic",
                                                   ifelse(grepl(string_OtherChemo, Treat), "OtherChemo",
                                                       ifelse(grepl(string_Hormonal, Treat), "Hormonal", "Lapsed"))))))



CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(ElapsedMets=Month-metastasis_onset)

names(groups_to_track)[3] <- "experienced_at_mets"

CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(groups_to_track %>% select(-weight), by=c("patient"="patid"))

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Box=ifelse(Box=="Lapsed"&Drug_Exp ==1, "Lapsed",
                               ifelse(Box=="Lapsed"&Drug_Exp==0, "Naive", Box))) 


ignore <- CAN_Drug_Histories %>% group_by(experienced_at_mets, n, group, ElapsedMets, Box) %>% 
  summarise(pop=sum(weight)) %>% spread(key=Box, value=pop)



groups <- fread("Source/Groups_HR_HER2_status.txt") 
groups <- groups %>% filter(group=="HRPosHER2Neg") %>% select(patid) %>% rename("patient"="patid") 


CAN_Drug_Histories %>% filter(ElapsedMets>=(-12) & ElapsedMets<=12) %>%
  group_by(patient) %>% count() %>% filter(n>=25) %>% select(patient) %>%
  distinct() %>% ungroup() %>% inner_join(groups) %>%
  left_join(CAN_Drug_Histories) %>% 
  mutate(experienced_at_mets=ifelse(experienced_at_mets==0, "Naive_", "Exp_")) %>%
  mutate(n=ifelse(n==1, "1_", "+2_")) %>%
 group_by(experienced_at_mets, n, group, ElapsedMets, Box) %>% 
  summarise(pop=sum(weight)) %>% mutate(CLASS=paste0(experienced_at_mets, paste0(n, group))) %>%
  mutate(Box=factor(Box, levels=c("Palbo", "OtherTarget", "Biologic", "OtherChemo", "Hormonal", "Lapsed", "Naive"))) %>%
  ungroup() %>%
  group_by(CLASS, ElapsedMets) %>% mutate(tot=sum(pop)) %>%
  mutate(pop=100*pop/tot) %>% ungroup() %>%
  ggplot(aes(ElapsedMets , pop, colour=Box, fill=Box)) +
   geom_line(size=1, alpha=0.8) +
   xlim(-24,24) +
   facet_wrap(~CLASS, scales="free_y") +
   theme_classic() +
   ylab("Population \n") + xlab("\n Number of Months to/from 1st Metastasis") +
  scale_colour_manual(values=c("#D63131", "#1761B8", "#29B8DC","#56B43A","#EB7BE5", "#7F7F7F", "#D3D3D3")) +
  scale_fill_manual(values=c("#D63131", "#1761B8", "#29B8DC","#56B43A","#EB7BE5", "#7F7F7F", "#D3D3D3"))
 
 
 
groups_to_track 



# 
# CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
# CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
# CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
# CAN_Drug_Histories <- CAN_Drug_Histories %>% select(patient, weight, Treat) %>% distinct()
# Palbo_pats <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% select(patient) %>% distinct()
# 
# groups_to_track %>% group_by(experienced_at_mets,n,group) %>% summarise(pop=sum(weight))
# groups_to_track %>% inner_join(Palbo_pats, by=c("patid"="patient")) %>%
#   group_by(experienced_at_mets,n,group) %>% summarise(pop=sum(weight))
# 
# 
# 
# PONS_Demographics_surv <- fread("Source/PONS Demographics.txt")
# PONS_Demographics_surv <- PONS_Demographics_surv %>% select(patid, cancer_onset, cancer_metastasis , death_date )
# 
# PONS_Demographics_surv$cancer_onset <- as.Date(PONS_Demographics_surv$cancer_onset)
# PONS_Demographics_surv$cancer_metastasis <- as.Date(PONS_Demographics_surv$cancer_metastasis)
# PONS_Demographics_surv$death_date  <- as.Date(PONS_Demographics_surv$death_date)
# 
# missingDeathDay <- ymd("2025-07-31")
# 
# PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))
# 
# PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset)) %>%
#   mutate(Survived=ifelse(Survived>1825,1825,Survived))
# 
# PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived_Mets = as.numeric(death_date)-as.numeric(cancer_metastasis)) %>%
#   mutate(Survived_Mets=ifelse(Survived_Mets>1825,1825,Survived_Mets))
# 
# PONS_Demographics_surv <- PONS_Demographics_surv %>% select(patid, Survived, Survived_Mets) %>% drop_na()
# 
# 
# groups_to_track %>% inner_join(PONS_Demographics_surv)  %>% group_by() %>% 
#  group_by(experienced_at_mets,n,group) %>% summarise(mean=mean(Survived_Mets/30.5))
#  
# 
# 
# 
# 
# 




 
 # Anchor on first mets higher level (LLB, or Lmyphoma or SKin)

# PONS_Events <- fread("Source/PONS Events.txt")
# library("readxl")
# PONS_Diagnosis_Codes <- read_excel("Source/PONS Diagnosis Codes 1.0.xlsx")
# unique(PONS_Diagnosis_Codes$condition)
# 
# PONS_Diagnosis_Codes <-PONS_Diagnosis_Codes %>% filter(condition %in% c("Lymphoma Cancer", "Skin Cancer", "Lung Cancer", "Liver Cancer", "Bone Cancer"))
# PONS_Diagnosis_Codes <- PONS_Diagnosis_Codes %>% select(code, condition)
# 
# PONS_Events <- PONS_Diagnosis_Codes %>% inner_join(PONS_Events %>% select(-c(prov, weight)))
# PONS_Events <- PONS_Events %>% select(patid, condition, claimed) %>% distinct() %>% mutate(claimed=as.Date(claimed))
# PONS_Events <- PONS_Events %>% inner_join(CAN_Drug_Histories %>% select(patient) %>% distinct(), by=c("patid"="patient")) %>% 
#   group_by(patid, condition) %>% summarise(claimed=min(claimed))
# PONS_Events <- PONS_Events %>% ungroup()
# 
# setDT(PONS_Events)
# PONS_Events[, claimed := as.character(claimed)][, claimed := substr(claimed, 1L, 7L)]
# 
# Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)
# 
# Months_lookup$Month <- as.character(
#   format(
#     as.Date(
#       paste0(Months_lookup$Month,"-1")
#       ), "%Y-%m"
#     )
#   )
# 
# PONS_Events <- PONS_Events[Months_lookup, on = c("claimed" = "Month")]
# PONS_Events <- PONS_Events %>% select(-claimed)
# 
# PONS_Events <- PONS_Events %>% mutate(condition=ifelse(condition %in% c("Lung Cancer", "Liver Cancer", "Bone Cancer"), "LLB", condition )) 
# 
# PONS_Events <- PONS_Events %>% mutate(rank=ifelse(condition=="LLB",1,
#                                    ifelse(condition=="Lymphoma Cancer",2,3))) %>%
#   group_by(patid) %>% filter(rank==min(rank)) %>% ungroup() %>% select(-rank)
# 
# 
# PONS_Events <- PONS_Events %>% group_by(patid, Exact_Month) %>% summarise(Exact_Month=min(Exact_Month)) %>% 
#   ungroup() %>% rename("First_Higher"="Exact_Month")
# 
# length(unique(PONS_Events$patid))
# 
# PONS_Events <- PONS_Events %>% group_by(patid) %>% filter(First_Higher==min(First_Higher))
# PONS_Events <- PONS_Events %>% ungroup()
# 
# 
# 
# 
#  CAN_Drug_Histories %>% 
#    left_join(PONS_Events, by=c("patient"="patid")) %>%
#    mutate(First_Higher=ifelse(is.na(First_Higher), metastasis_onset, First_Higher)) %>%
#    mutate(ElapsedMets=Month-First_Higher)  %>%
#    filter(experienced_at_mets==0 & group=="Skin" & ElapsedMets<6) %>%
#    arrange(Box)
# 
#  CAN_Drug_Histories %>% 
#    left_join(PONS_Events, by=c("patient"="patid")) %>%
#    mutate(First_Higher=ifelse(is.na(First_Higher), metastasis_onset, First_Higher)) %>%
#    mutate(ElapsedMets=Month-First_Higher) %>%
#    group_by(experienced_at_mets, n, group, ElapsedMets, Box) %>% 
#   summarise(pop=sum(weight)) %>% mutate(CLASS=paste0(experienced_at_mets, paste0(n, group))) %>%
#    ggplot(aes(ElapsedMets , pop, colour=Box, fill=Box)) +
#    geom_line(size=1, alpha=0.8) +
#    xlim(-24,24) +
#    facet_wrap(~CLASS, scales="free_y") +
#    theme_minimal() +
#    ylab("Population \n") + xlab("\n Number of Months to/from 1st Metastasis") +
#   scale_colour_manual(values=c("#9B55A9", "#7F7F7F", "#D3D3D3", "#29D562", "#66A4B9", "#C86060" )) +
#   scale_fill_manual(values=c("#9B55A9", "#7F7F7F", "#D3D3D3","#29D562", "#66A4B9", "#C86060" ))
#  
#  
 


# ----------
# Average treatment months on each class per metastasis site ----------

New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, cancer_metastasis) %>% inner_join(New_Primary_Cancer_Box)
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))

PONS_Demographics <- PONS_Demographics %>% select(patid, weight)

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
unique(PONS_Ingredients_JN_ChemoClass$chemo_class)

string_Platinum        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Platinum agent"], collapse = "|"),")\\b")
string_PD1PDL1        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "PD1/PDL1"], collapse = "|"),")\\b")
string_TargetImmuno        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Immuno/Targeted"], collapse = "|"),")\\b")
string_Biologic        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Biologic"], collapse = "|"),")\\b")
string_Hormonal        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Hormonal Therapy"], collapse = "|"),")\\b")
string_Alkylating        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Alkylating Agent"], collapse = "|"),")\\b")
string_OtherAntineoplastics        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Other Antineoplastics"], collapse = "|"),")\\b")
string_Antimetabolites        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimetabolites"], collapse = "|"),")\\b")
string_Antimicrotubule        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimicrotubule Agent"], collapse = "|"),")\\b")
string_Topoisomerase        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Topoisomerase Inhibitor"], collapse = "|"),")\\b")
string_Radio        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class == "Radio"], collapse = "|"),")\\b")
string_AnyChemo        <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class == "Platinum agent"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "PD1/PDL1"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Alkylating Agent"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Other Antineoplastics"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimetabolites"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Antimicrotubule Agent"|
    PONS_Ingredients_JN_ChemoClass$chemo_class == "Topoisomerase Inhibitor"], collapse = "|"),")\\b")

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(PONS_Demographics %>% select(patid), by=c("patient"="patid"))
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-disease)
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Platinum=ifelse(grepl(string_Platinum,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(PD1PDL1=ifelse(grepl(string_PD1PDL1,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(TargetImmuno=ifelse(grepl(string_TargetImmuno,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Biologic=ifelse(grepl(string_Biologic,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Hormonal=ifelse(grepl(string_Hormonal,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Alkylating=ifelse(grepl(string_Alkylating,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(OtherAntineoplastics=ifelse(grepl(string_OtherAntineoplastics,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Antimetabolites=ifelse(grepl(string_Antimetabolites,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Antimicrotubule=ifelse(grepl(string_Antimicrotubule,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Topoisomerase=ifelse(grepl(string_Topoisomerase,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Radio=ifelse(grepl(string_Radio,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(AnyChemo=ifelse(grepl(string_AnyChemo,Treat), 1, 0))
CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Treated=ifelse(grepl("-", Treat),1,0))


New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis, cancer_metastasis) %>% inner_join(New_Primary_Cancer_Box)
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis)
names(PONS_Demographics)[1] <- "patient"

CAN_Drug_Histories <- PONS_Demographics %>% inner_join(CAN_Drug_Histories)

sums_df <- CAN_Drug_Histories %>%
  select(-c(Month, Treat)) %>%
  group_by(diagnosis, patient, weight) %>%
  summarize(across(everything(), sum))

average_persistency_diag_classes <- sums_df %>% ungroup() %>%
  select(-c(patient, weight)) %>%
  group_by(diagnosis) %>%
  summarize(across(everything(), mean))

fwrite(average_persistency_diag_classes, "average_persistency_diag_classes.csv")



PONS_Demographics_surv <- fread("Source/PONS Demographics.txt")
PONS_Demographics_surv <- PONS_Demographics_surv %>% select(patid, cancer_onset , death_date )

PONS_Demographics_surv$cancer_onset <- as.Date(PONS_Demographics_surv$cancer_onset)
PONS_Demographics_surv$death_date  <- as.Date(PONS_Demographics_surv$death_date)

max(PONS_Demographics_surv$cancer_onset, na.rm=T)

missingDeathDay <- ymd("2021-07-31")

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(death_date = case_when(is.na(death_date) ~ missingDeathDay, TRUE ~ death_date))

PONS_Demographics_surv <- PONS_Demographics_surv %>% mutate(Survived = as.numeric(death_date)-as.numeric(cancer_onset))

data.frame(PONS_Demographics %>% left_join(PONS_Demographics_surv, by=c("patient"="patid")) %>%
  group_by(diagnosis) %>% summarise(mean=round(mean(Survived/30.5),1)))




# ---------


# Persistency ON Palbociclib as a function of Metastasis site ----------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
Metastatic <- PONS_Demographics%>% filter(!is.na(cancer_metastasis))  %>% select(patid)
New_Primary_Cancer_Box <- Metastatic %>% inner_join(New_Primary_Cancer_Box)

CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[New_Primary_Cancer_Box[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))
Months_ON_Palbo <- CAN_Drug_Histories %>% filter(grepl("179", Treat)) %>% group_by(patient) %>% count()

mean(Months_ON_Palbo$n) # 11.84

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, diagnosis)
PONS_Demographics <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box %>% select(patid))
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% mutate(Exp=1) %>% spread(key=diagnosis, value=Exp) 
PONS_Demographics[is.na(PONS_Demographics)] <- 0

Months_ON_Palbo <- Months_ON_Palbo %>% inner_join(PONS_Demographics, by=c("patient"="patid"))

Months_ON_Palbo %>% group_by(Head) %>% summarise(mean=mean(n))
Months_ON_Palbo %>% group_by(Skin) %>% summarise(mean=mean(n))


Months_ON_Palbo %>% ggplot(aes(n)) +
  geom_density(colour="#0072BB", fill="#0072BB", alpha=0.7) +
    theme_minimal() +
  xlab("\n Exact Number of Months ON Palbociclib") +
  ylab("Patient density \n")
  

Months_ON_Palbo_bin <- Months_ON_Palbo %>% mutate(n=ifelse(n<=3,0,
                                                           ifelse(n>=24,1,9))) %>%
  filter(n!=9) %>% mutate(n=as.factor(n)) 

Months_ON_Palbo_bin <- Months_ON_Palbo_bin %>% ungroup() %>% select(-patient)

Months_ON_Palbo_bin %>%
  group_by(n) %>%
  summarise_all(.funs = mean) 


Months_ON_Palbo_bin <- Months_ON_Palbo_bin %>% select(-Breast)
Months_ON_Palbo_bin <- Months_ON_Palbo_bin %>% select(-Prostate)

#Months_ON_Palbo_bin <- Months_ON_Palbo_bin %>% select(-Salivary)

model1 <- glm(n ~ . , data=Months_ON_Palbo_bin, family = "binomial")
summary(model1)


df <- data.frame(exp(cbind(coef(model1), confint(model1))))
df <- df[2:20,]
df$var <- row.names(df)  
names(df) <- c("OR", "Lower", "Upper", "Var") 
  
library(jtools)


df <- df %>% arrange(-OR)
df <- df %>% mutate(group=ifelse(OR>1, 1, 2))

df$group <- as.factor(df$group)


dotCOLS = c("#D45769","#0072BB")
barCOLS = c("#D45769","#0072BB")

df %>%
  mutate(Var=fct_reorder(Var, OR)) %>%
  ggplot(aes(x=Var, y=OR, ymin=Lower, ymax=Upper,col=group,fill=group)) + 
  geom_linerange(size=5,position=position_dodge(width = 0.5), alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="(Post)operative outcomes") +
  scale_y_continuous(name="Odds ratio", limits = c(0, 2.5)) +
  coord_flip() +
  theme_minimal()


df %>%
  mutate(Var=fct_reorder(Var, OR)) %>%
  ggplot(aes(x=Var, y=OR, col=group,fill=group)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_point(show.legend = FALSE, size=5, shape=1, stroke = 3, alpha=0.9, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Metastasis Site (non-MECE)") +
  scale_y_continuous(name="Odds ratio", limits = c(0, 2)) +
  coord_flip() +
  theme_minimal()


df <- Months_ON_Palbo %>% select(-c(Breast, Prostate)) 
df <- df %>% ungroup() %>% select(-patient)

model1 <- lm(n ~ . , data=df)
summary(model1)


df <- data.frame(cbind(coef(model1), confint(model1)))
df <- df[2:20,]
df$var <- row.names(df)  
names(df) <- c("Coef", "Lower", "Upper", "Var") 
  
library(jtools)

df <- df %>% arrange(-Coef)
df <- df %>% mutate(group=ifelse(Coef>0, 1, 2))

df$group <- as.factor(df$group)


dotCOLS = c("#D45769","#0072BB")
barCOLS = c("#D45769","#0072BB")


df %>%
  mutate(Var=fct_reorder(Var, Coef)) %>%
  ggplot(aes(x=Var, y=Coef, col=group,fill=group)) + 
  geom_hline(yintercept=0, lty=2) +
  geom_point(show.legend = FALSE, size=5, shape=1, stroke = 3, alpha=0.9, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Metastasis Site (non-MECE)") +
  scale_y_continuous(name="β", limits = c(-3.1, 2)) +
  coord_flip() +
  theme_minimal()

# --------------

# marimmeko LLB Lymphoma Other patient months on each class ----------

# Stock Month over month 

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box)
names(New_Primary_Cancer_Box)[1] <- "patient"


CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat, metastasis_onset)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=metastasis_onset) %>% select(-metastasis_onset)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="355")

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")

string_Biologic <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"], collapse = "|"),")\\b")

string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Stock=ifelse(grepl("179", Treat), "Palbo",
                                                                 ifelse(grepl(string_target, Treat), "Target",
                                                                        ifelse(grepl(string_Biologic, Treat), "Biologic",
                                                                               ifelse(grepl(string_OtherChemo, Treat), "Chemo",
                                                                                      ifelse(grepl(string_Hormonal, Treat), "Hormone", "Lapsed"))))))


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-Treat)



PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis) %>% rename("patient"="patid") %>%
  inner_join(CAN_Drug_Histories %>% select(patient) %>% distinct())
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% mutate(exp=1) %>% spread(key=diagnosis, value=exp)
PONS_Demographics[is.na(PONS_Demographics)] <- 0

PONS_Demographics <- PONS_Demographics %>% mutate(Group=ifelse(Liver==1|Lung==1|Bone==1, "LLB",
                                          ifelse(Lymphoma==1, "Lymphoma", "Other"))) %>% 
  # group_by(Group) %>% summarise(n=sum(weight))
  select(patient, Group, weight)

PONS_Demographics %>% group_by(Group) %>% summarise(n=sum(weight))

# 1 LLB      368414.
# 2 Lymphoma 362198.
# 3 Other    322711.


CAN_Drug_Histories %>% group_by(patient, Stock) %>% summarise(Months_ON_Class=sum(weight)) %>%
  inner_join(PONS_Demographics %>% select(-weight)) %>% ungroup() %>%
  group_by(Group, Stock) %>% summarise(Months_ON_Class=sum(Months_ON_Class))

#    Group    Stock    Months_ON_Class
#  1 LLB      Biologic        2390700.
#  2 LLB      Chemo           1366168.
#  3 LLB      Hormone         2602414.
#  4 LLB      Lapsed          4010651.
#  5 LLB      Palbo           1005430.
#  6 LLB      Target           493739.
#  7 Lymphoma Biologic        1211605.
#  8 Lymphoma Chemo           1195309.
#  9 Lymphoma Hormone         5168649.
# 10 Lymphoma Lapsed          4692553.
# 11 Lymphoma Palbo             45103.
# 12 Lymphoma Target            72358.
# 13 Other    Biologic         761497.
# 14 Other    Chemo            432174.
# 15 Other    Hormone         3694792.
# 16 Other    Lapsed          5547355.
# 17 Other    Palbo             16403.
# 18 Other    Target           100728

# ---------

# Stocks and flows relative to metastasis --------------------

CancerDrug_Experienced <- fread("Source/CancerDrug_Experienced.txt",  integer64 = "character", stringsAsFactors = F)
New_Primary_Cancer_Box <- fread("Source/New_Primary_Cancer_Box.txt", sep="\t")
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% inner_join(CancerDrug_Experienced)
New_Primary_Cancer_Box <- New_Primary_Cancer_Box %>% filter(Primary_Cancer=="Breast Cancer") %>% select(patid)

PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% filter(!is.na(cancer_metastasis))  %>% select(patid, cancer_metastasis)
setDT(PONS_Demographics)
PONS_Demographics[, cancer_metastasis := as.character(cancer_metastasis)][, cancer_metastasis := substr(cancer_metastasis, 1L, 7L)]

Months_lookup <- fread("Source/Months_lookup.txt",  integer64 = "character", stringsAsFactors = F)

Months_lookup$Month <- as.character(
  format(
    as.Date(
      paste0(Months_lookup$Month,"-1")
      ), "%Y-%m"
    )
  )

PONS_Demographics <- PONS_Demographics[Months_lookup, on = c("cancer_metastasis" = "Month")]
PONS_Demographics <- PONS_Demographics %>% select(-cancer_metastasis) %>% rename("metastasis_onset"="Exact_Month")

New_Primary_Cancer_Box <- PONS_Demographics %>% inner_join(New_Primary_Cancer_Box)
names(New_Primary_Cancer_Box)[1] <- "patient"


CAN_Drug_Histories <- fread("Source/CAN Drug Histories.txt")
CAN_Drug_Histories <- CAN_Drug_Histories %>% inner_join(New_Primary_Cancer_Box)
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[, .(patient, weight, Month, Treat, metastasis_onset)])
CAN_Drug_Histories$Month <- parse_number(as.character(CAN_Drug_Histories$Month))

# CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Month>=metastasis_onset)
CAN_Drug_Histories <- CAN_Drug_Histories %>% filter(Treat!="355")

PONS_Ingredients_JN_ChemoClass <- fread("Source/PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")

string_target <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Immuno/Targeted"], collapse = "|"),")\\b")

string_Biologic <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[PONS_Ingredients_JN_ChemoClass$chemo_class=="Biologic"], collapse = "|"),")\\b")

string_OtherChemo <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[(PONS_Ingredients_JN_ChemoClass$drug_group=="GDF15"|
                                                                                     PONS_Ingredients_JN_ChemoClass$drug_group=="Anticancer" ) &
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Immuno/Targeted"&
                                                                               PONS_Ingredients_JN_ChemoClass$chemo_class!="Biologic"&
                                                                                 PONS_Ingredients_JN_ChemoClass$chemo_class!="Hormonal Therapy"], collapse = "|"),")\\b")

string_Hormonal <- paste0("\\b(",paste0(PONS_Ingredients_JN_ChemoClass$molecule[
  PONS_Ingredients_JN_ChemoClass$chemo_class=="Hormonal Therapy"], collapse = "|"),")\\b")

CAN_Drug_Histories <- CAN_Drug_Histories %>% mutate(Stock=ifelse(grepl("179", Treat), "Palbo",
                                                                 ifelse(grepl(string_target, Treat), "Target",
                                                                        ifelse(grepl(string_Biologic, Treat), "Biologic",
                                                                               ifelse(grepl(string_OtherChemo, Treat), "Chemo",
                                                                                      ifelse(grepl(string_Hormonal, Treat), "Hormone", "Lapsed"))))))


CAN_Drug_Histories <- CAN_Drug_Histories %>% select(-Treat)

CAN_Drug_Histories %>% mutate(Month=Month-metastasis_onset) %>%
  arrange(patient, Month) %>% group_by(patient) %>%
  filter(Stock=="Palbo"&lag(Stock)!="Palbo")


PONS_Demographics <- fread("Source/PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics %>% select(patid, weight, diagnosis) %>% rename("patient"="patid") %>%
  inner_join(CAN_Drug_Histories %>% select(patient) %>% distinct())
PONS_Demographics <- separate_rows(PONS_Demographics, diagnosis, sep = ",", convert=T)
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " Cancer", "")
PONS_Demographics$diagnosis <- str_replace_all(PONS_Demographics$diagnosis, " ", "")
PONS_Demographics <- PONS_Demographics %>% mutate(exp=1) %>% spread(key=diagnosis, value=exp)
PONS_Demographics[is.na(PONS_Demographics)] <- 0

PONS_Demographics <- PONS_Demographics %>% mutate(Group=ifelse(Liver==1|Lung==1|Bone==1, "LLB",
                                          ifelse(Lymphoma==1, "Lymphoma", "Other"))) %>% 
  # group_by(Group) %>% summarise(n=sum(weight))
  select(patient, Group, weight)

PONS_Demographics %>% group_by(Group) %>% summarise(n=sum(weight))

CAN_Drug_Histories %>% mutate(Month=Month-metastasis_onset) %>%
  arrange(patient, Month) %>% group_by(patient) %>%
  filter( (Stock=="Palbo"&lag(Stock)!="Palbo") | (Stock=="Palbo"&Month==0) ) %>%
  ungroup() %>%
  filter(Month<=12) %>%
  inner_join(PONS_Demographics) %>%
  group_by(Group) %>% summarise(tot=sum(weight))

#   Group       tot
# 1 LLB      73202.
# 2 Lymphoma  3089.
# 3 Other     1018.
#   


CAN_Drug_Histories %>% mutate(Month=Month-metastasis_onset) %>%
  arrange(patient, Month) %>% group_by(patient) %>%
  filter(Stock=="Palbo"&lag(Stock)!="Palbo") %>%
  ungroup() %>%
  filter(Month<=12) %>% 
  group_by(Month) %>% summarise(tot=sum(weight))


CAN_Drug_Histories %>% mutate(Month=Month-metastasis_onset) %>%
  arrange(patient, Month) %>% 
  filter(Stock=="Palbo") %>%
  group_by(patient) %>% filter(Month==min(Month)) %>%
  ungroup() %>%
  filter(Month<=12) %>% 
  group_by(Month) %>% summarise(tot=sum(weight))



CAN_Drug_Histories %>% mutate(Month=Month-metastasis_onset) %>%
  filter(Month<=12&Stock=="Palbo" & Month>=0) %>% select(patient, weight) %>%
  distinct() %>% summarise(tot=sum(weight))



CAN_Drug_Histories %>% mutate(Month=Month-metastasis_onset) %>%
  filter( (Stock!="Palbo"&lead(Stock)=="Palbo") ) %>%
  filter(Month<=11 & Month>=(-1)) %>%
  group_by(Month, Stock) %>% summarise(n=sum(weight)) %>%
  spread(key=Stock, value=n)


CAN_Drug_Histories %>% mutate(Month=Month-metastasis_onset) %>%
  filter( (Stock!="Palbo"&lag(Stock)=="Palbo") ) %>%
  filter(Month<=13 & Month>=(1)) %>%
  group_by(Month, Stock) %>% summarise(n=sum(weight)) %>%
  spread(key=Stock, value=n)




# ---------------
