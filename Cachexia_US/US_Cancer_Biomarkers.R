
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
