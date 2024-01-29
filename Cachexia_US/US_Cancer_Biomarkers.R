
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
initial_colors[21] <- "#1C80D2"
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