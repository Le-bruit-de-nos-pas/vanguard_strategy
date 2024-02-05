
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


data.frame(First_Neutro_Dx %>% filter(!is.na(date)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<cancer_onset) %>% select(patid)) %>%
  anti_join(First_Neutro_Dx %>% filter(date<from_dt) %>% select(patid)) %>%
  group_by(Primary_Cancer) %>%
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
