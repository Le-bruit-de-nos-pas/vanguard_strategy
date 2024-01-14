
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
New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")

setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[
  Primary_Cancer %in% c("Breast Cancer", "Lung Cancer", "Intestinal Cancer", "Prostate Cancer"),
  .(patid, Primary_Cancer)
]

biomarkers <- fread("biomarkers/biomarkers.txt", sep=",")

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


PONS_Demographics <- fread("PONS Demographics.txt")
PONS_Demographics <- PONS_Demographics%>% select(patid, died) 

target %>% left_join(PONS_Demographics) %>% group_by(EGFR, died) %>% count()


# -------------
# Drug usage by Breast Cancer Metastasis and genetic Background status ------------------

New_Primary_Cancer_Box <- fread("New_Primary_Cancer_Box.txt", sep="\t")

setDT(New_Primary_Cancer_Box)

New_Primary_Cancer_Box <- New_Primary_Cancer_Box[Primary_Cancer %in% c("Breast Cancer"), .(patid, Primary_Cancer)]

biomarkers <- fread("biomarkers.txt", sep=",")

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

Breast <- Breast[, Positive := ifelse(positive == 1, 1, 0)][, .(patid, Primary_Cancer, BIOMARKER, Positive)]

PONS_Demographics <- fread("PONS Demographics.txt")

PONS_Demographics <- PONS_Demographics[ , .(patid, diagnosis)]

setDT(PONS_Demographics)

PONS_Demographics <- unique(PONS_Demographics[Breast[, .(patid)], on = "patid", nomatch = 0])

PONS_Demographics <- Breast[PONS_Demographics, on = "patid", nomatch = 0]

PONS_Demographics[, mets := ifelse(grepl(",", diagnosis), 1, 0)]

PONS_Demographics


# CLASSES

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat)])
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
setDT(unique(CAN_Drug_Histories))

PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
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

fwrite(result_final , "result_final.csv")




# MOLECULES

CAN_Drug_Histories <- fread("CAN Drug Histories.txt")
CAN_Drug_Histories <- setDT(CAN_Drug_Histories)[PONS_Demographics[, .(patid)], on = c("patient"="patid"), nomatch = 0]
CAN_Drug_Histories <- gather(CAN_Drug_Histories, Month, Treat, month1:month60, factor_key=TRUE)
setDT(CAN_Drug_Histories)
CAN_Drug_Histories <- unique(CAN_Drug_Histories[Treat != "-", .(patient, Treat)])
CAN_Drug_Histories <- separate_rows(CAN_Drug_Histories, Treat, sep = ",", convert=T)
setDT(unique(CAN_Drug_Histories))

PONS_Ingredients_JN_ChemoClass <- fread("PONS_Ingredients_JN_ChemoClass.csv", colClasses = "character")
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

fwrite(result_final , "result_final2.csv")






# ---------