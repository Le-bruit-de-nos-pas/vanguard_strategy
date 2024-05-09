
library(data.table)
library(tidyverse)
library(lubridate)
library(openxlsx)

ingrd  <- fread("JMDC data/Version 1.2/DANU Japan Ingredients.txt", integer64 = "character", stringsAsFactors = F)
dosDIA  <- fread("JMDC data/Version 1.2/DIA Japan Doses.txt", integer64 = "character", stringsAsFactors = F)
dosOBE  <- fread("JMDC data/Version 1.2/OBE Japan Doses.txt", integer64 = "character", stringsAsFactors = F)
drgDIA  <- fread("JMDC data/Version 1.2/DIA Japan Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
drgOBE  <- fread("JMDC data/Version 1.2/OBE Japan Drug Histories.txt", integer64 = "character", stringsAsFactors = F)
boxDIA  <- fread("JMDC data/Version 1.2/DIA Japan Box Histories.txt", integer64 = "character", stringsAsFactors = F)
boxOBE  <- fread("JMDC data/Version 1.2/OBE Japan Box Histories.txt", integer64 = "character", stringsAsFactors = F)


# column with drug ids as they show in DIA and OBE drug tables
drgLkup <- ingrd
drgLkup$drg_id <- unlist(lapply(drgLkup$drug_id, function(x) as.numeric(unlist(str_extract_all(x,"[:digit:]+$")))))

# bring stocks in
boxSpec <- read.xlsx("JMDC data/Version 1.2/DANU Japan Box Specifications 1.2.xlsx")
drgLkup <- left_join(drgLkup, boxSpec[boxSpec$drug_group != "Antiobesity",c("box_code","drug_group")], by = "drug_group")
drgLkup <- left_join(drgLkup, boxSpec[boxSpec$drug_group == "Antiobesity",c("box_code","box_name")], by = c("drug_class" = "box_name"))
drgLkup$box_code.x[is.na(drgLkup$box_code.x)] <- drgLkup$box_code.y[is.na(drgLkup$box_code.x)]
drgLkup$box_code.y <- NULL
names(drgLkup)[names(drgLkup) == "box_code.x"] <- "box_code" 
drgLkup <- left_join(drgLkup, boxSpec[,c("box_code", "box_name")], by = "box_code")

#write.xlsx(drgLkup,"R data/drgLkup_v1.xlsx")




# Reshape of T2D drug Histories************************************************* 
drgDIA2 <- drgDIA

# Ever treated (60m window) - including Obesity drugs (used by DIA pts)
DIAeverRx <- data.frame(drgDIA2[,c(1:3)], DIAeverRx = apply(drgDIA2[,c(4:63)], 1, function(x) (str_c(unique(x),collapse = ",") != "-")*1), stringsAsFactors = F)
sum(DIAeverRx$DIAeverRx) # 100,392

# Excluding obesity drugs from DIA drugs table
string_OBE <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$drug_group == "Antiobesity" | drgLkup$drug_group == "Surgery"], collapse = "|"),")\\b")
drgDIA2 <- data.frame(drgDIA2[,c(1:3)], lapply(drgDIA2[,c(4:63)], function(x) str_remove_all(x, string_OBE)), stringsAsFactors = F) # remove OBE drugs
drgDIA2 <- data.frame(drgDIA2[,c(1:3)], lapply(drgDIA2[,c(4:63)], function(x) str_remove_all(x, ",+$")), stringsAsFactors = F) # remove "," at end of string
drgDIA2 <- data.frame(drgDIA2[,c(1:3)], lapply(drgDIA2[,c(4:63)], function(x) str_remove_all(x, "^,+")), stringsAsFactors = F) # remove "," at start of string
drgDIA2 <- data.frame(drgDIA2[,c(1:3)], lapply(drgDIA2[,c(4:63)], function(x) str_remove_all(x, ",+(?=,)")), stringsAsFactors = F) # remove "," followed by ","

for (i in 1:60){
  cat(i)
  sel <- drgDIA2[,i+3] == ""
  drgDIA2[sel,i+3] <- "-"
}

# Ever treated (60m window) - excluding Obesity drugs (used bt DIA pts)
drgDIA2$everRx <- apply(drgDIA2[,c(4:63)], 1, function(x) (str_c(unique(x),collapse = ",") != "-")*1)
sum(drgDIA2$everRx) # 96,910 => 100,392 - 96,910 = 3,482 T2D dx pts using only Obesity drugs

# Only patients Ever treated with T2D drugs
drgDIA2 <- drgDIA2[drgDIA2$everRx == 1,]
drgDIA2$everRx <- NULL
#fwrite(drgDIA2,"JMDC data/Version 1.2/DIA Japan Drug Histories_v2.txt")

# Treated DIA pts
DIARxpts <- data.frame(pat_id = drgDIA2$patient, stringsAsFactors = F)
#fwrite(DIARxpts,"R Data/DIA_JP_T2DRx_pts.csv")


# Reshape of OBE drug Histories*************************************************
drgOBE2 <- drgOBE

# Identify patients who were part of the original DIA drugs table and further removed due to Obesity drug use only,  and add their drug history to the Obesity drug history table 
ptsAdd <- DIAeverRx$patient[DIAeverRx$DIAeverRx == 1] 
ptsAdd <- ptsAdd[!(ptsAdd %in% DIARxpts$pat_id)] # T2D diagnosed pts with Obesity drugs only

drgOBE2 <- data.frame(rbind(drgOBE2, drgDIA[drgDIA$patient %in% ptsAdd,]), stringsAsFactors = F) 
#fwrite(drgOBE2,"JMDC data/Version 1.2/OBE Japan Drug Histories_v2.txt")





# Reshape of T2D stocks table*************************************************** 

# DIA drug table
drgDIA2 <- data.frame(fread("JMDC data/Version 1.2/DIA Japan Drug Histories_v2.txt", integer64 = "character", stringsAsFactors = F))

# Box therapy drug strings
string_Big       <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Biguanide"], collapse = "|"),")\\b")
string_Kampo     <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Kampo"], collapse = "|"),")\\b")
string_Anor      <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Anorectic"], collapse = "|"),")\\b")
string_AntiDIA   <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4      <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "DPP4"], collapse = "|"),")\\b")
string_SGLT2     <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "SGLT2"], collapse = "|"),")\\b")
string_GLP1Oral  <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "GLP1 Oral"], collapse = "|"),")\\b")
string_GLP1Inj   <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "GLP1 Injectable"], collapse = "|"),")\\b")
string_Ins       <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Insulin"], collapse = "|"),")\\b")
string_Surg      <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Surgery"], collapse = "|"),")\\b")

# Stock therapy hierarchy
# Surgery(H) > Insulin(I) > GLP1 Injectable(G) > GLP1 Oral(g) > SGLT2(S) > DPP4(D) > Antidiabetic(d) > Anorectic(o) > Kampo(k) > Biguanide(b) > Lapsed(x)
strings <- data.frame(string = c(string_Big, string_Kampo, string_Anor, string_AntiDIA, string_DPP4, string_SGLT2, string_GLP1Oral, string_GLP1Inj, string_Ins, 
                                 string_Surg),
                        name = c("Biguanide", "Kampo", "Anorectic","Antidiabetic", "DPP4", "SGLT2", "GLP1 Oral", "GLP1 Injectable", "Insulin","Surgery"),
                       stock = c("b","k","o","d","D","S","g","G","I","H"), stringsAsFactors = F)

# Stocks table
boxT2D <- data.frame(drgDIA2[,c(1:3)], stringsAsFactors = F)

for(i in 1:60){
  cat(i)
  for(k in 1:nrow(strings)){
    sel              <- str_detect(drgDIA2[,i+3], strings$string[k])
    boxT2D[sel, i+3] <- strings$stock[k]
  }
  names(boxT2D)[i+3] <- paste0("month",i)
}

boxT2D[is.na(boxT2D)] <- "x"  
boxDIA2 <- boxT2D
#fwrite(boxDIA2,"JMDC data/Version 1.2/DIA Japan Box Histories_v2.1.txt")

# Reshape of Obesity stocks table***********************************************
boxOBE2 <- boxOBE

ptsAdd  <- drgOBE2$patient[!(drgOBE2$patient %in% drgOBE$patient)]
boxOBE2 <- data.frame(rbind(boxOBE2, boxDIA[(boxDIA$patient %in% ptsAdd)]), stringsAsFactors = F) 
#fwrite(boxOBE2,"JMDC data/Version 1.2/OBE Japan Box Histories_v2.txt")




# Reshape of T2D doses table**************************************************** 
dosDIA2 <- dosDIA

# T2D dx patients using only Obesity drugs 
unique(drgLkup$drug_group[drgLkup$indication == "Obesity Nonspecific" | drgLkup$indication == "Obesity"]) # "Antiobesity", "Surgery" 

T2DRxPts <- dosDIA2[drug_group != "Antiobesity" & drug_group != "Surgery"]
OBERxPts <- dosDIA2[drug_group == "Antiobesity" | drug_group == "Surgery"]

OBERxPts$OBEonly <- (!(OBERxPts$pat_id %in% T2DRxPts$pat_id))*1 
OBERxPts <- unique(OBERxPts[OBERxPts$OBEonly == 1,.(pat_id,weight)])

dosDIA2 <- dosDIA2[!(pat_id %in% OBERxPts$pat_id)]

#fwrite(dosDIA2,"JMDC data/Version 1.2/DIA Japan Doses_v2.txt")

# Not worth to perform the reshapping of the Obesity doses table, as in the meantime the call was to use only the T2D reshapped tables and use the original Obesity tables (doses, drugs, stocks)





library(data.table)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(scales)
library(splitstackshape)
library(ggridges)

weights     <- data.frame(fread("JMDC data/Version 1.2/DANU Japan Weights.txt", integer64 = "character", stringsAsFactors = F))
demo        <- data.frame(fread("JMDC data/Version 1.2/DANU Japan Demographics.txt", integer64 = "character", stringsAsFactors = F))
dossiers    <- fread("JMDC data/Version 1.2/DANU Japan Dossiers.txt", integer64 = "character", stringsAsFactors = F)
dxprofiles  <- fread("JMDC data/Version 1.2/DANU Japan Diagnosis Profiles.txt", integer64 = "character", stringsAsFactors = F)
drgLkup     <- data.frame(read.xlsx("R data/drgLkup_v1.xlsx"), stringsAsFactors = F)

dosDIA2     <- fread("JMDC data/Version 1.2/DIA Japan Doses_v2.txt", integer64 = "character", stringsAsFactors = F)
dosOBE      <- fread("JMDC data/Version 1.2/OBE Japan Doses.txt", integer64 = "character", stringsAsFactors = F)

drgDIA2     <- fread("JMDC data/Version 1.2/DIA Japan Drug Histories_v2.txt", integer64 = "character", stringsAsFactors = F)
drgOBE      <- fread("JMDC data/Version 1.2/OBE Japan Drug Histories.txt", integer64 = "character", stringsAsFactors = F)

boxDIA2     <- fread("JMDC data/Version 1.2/DIA Japan Box Histories_v2.1.txt", integer64 = "character", stringsAsFactors = F)
boxOBE      <- fread("JMDC data/Version 1.2/OBE Japan Box Histories.txt", integer64 = "character", stringsAsFactors = F)

boxAggDIA   <- fread("JMDC data/Version 1.2/DIA Japan Box Stocks.txt", integer64 = "character", stringsAsFactors = F)
boxAggOBE   <- fread("JMDC data/Version 1.2/OBE Japan Box Stocks.txt", integer64 = "character", stringsAsFactors = F)

flDIALong   <- fread("R data/DIA_Flows_Aux._Long_v2.1.txt", integer64 = "character", stringsAsFactors = F)
flOBELong   <- fread("R data/OBE_Flows_Aux._Long.txt", integer64 = "character", stringsAsFactors = F)

cmbdtDIA    <- fread("R data/DIAcomorbDxs_MarksDBPts.txt", integer64 = "character", stringsAsFactors = F)
cmbdtOBE    <- fread("R data/OBEcomorbDxs_MarksDBPts.txt", integer64 = "character", stringsAsFactors = F)

medDIA      <- fread("JMDC data/Version 1.2/DIA Japan Medication Surveys.txt", integer64 = "character", stringsAsFactors = F)







# Box therapy drug strings
string_Big       <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Biguanide"], collapse = "|"),")\\b")
string_Kampo     <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Kampo"], collapse = "|"),")\\b")
string_Anor      <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Anorectic"], collapse = "|"),")\\b")
string_AntiDIA   <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Antidiabetic"], collapse = "|"),")\\b")
string_DPP4      <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "DPP4"], collapse = "|"),")\\b")
string_SGLT2     <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "SGLT2"], collapse = "|"),")\\b")
string_GLP1Oral  <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "GLP1 Oral"], collapse = "|"),")\\b")
string_GLP1Inj   <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "GLP1 Injectable"], collapse = "|"),")\\b")
string_Ins       <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Insulin"], collapse = "|"),")\\b")
string_Surg      <- paste0("\\b(",paste0(drgLkup$drg_id[drgLkup$box_name == "Surgery"], collapse = "|"),")\\b")

# timeline - convert time 74 months window of analysis (continuous enrollment) into months
convertMonths           <- data.frame(min = seq(ymd("2015-04-16"), by = "month", length = 74), 
                                    max = seq(ymd("2015-05-15"), by = "month", length = 74), id = 1:74)
convertMonths$month     <- seq(1,nrow(convertMonths),1)
convertMonths$daysInM   <- convertMonths$max - convertMonths$min + 1






sum(weights$weight * weights$available_sample) # 107,622,659 -> Japanese population 18+
sum(weights$available_sample) # 2,108,894 -> Continuous Enrolled patients in JMDC DB 18+

#write.xlsx(weights,"R Data/weights.xlsx")




T2DAG <- setDT(demo)[diagnosis == "Diabetes" | diagnosis == "Diabetes + Obesity", .(weight = sum(weight)), by = .(age, gender)]
OBEAG <- setDT(demo)[diagnosis == "Obesity", .(weight = sum(weight)), by = .(age, gender)]

dataset <- list(T2D = T2DAG,Obesity = OBEAG)
#write.xlsx(dataset,"R Data/T2D_OBE_Age&Gender.xlsx")




# figures to feed waterfall underlying data in presentation*********************

# projection figures************************************************************

# number of enrolled members in the JMDC DB
enrollMemb <- fread("D:/JMDC_T2DOb/Analytics/Patients/EnrollPats.txt", integer64 = "character", stringsAsFactors = F)
fig1 <- length(unique(enrollMemb$member_id)) # 12,421,109

# number of continuous enrolled 18+ patient samples
fig2 <- sum(weights$available_sample) # 2,108,894

# Total insured Japanese resident population
fig3 <- sum(weights$weight * weights$available_sample) # 107,622,659

# diagnosed population figures (in 74m window)**********************************

# Total diagnosed (T2D or Obesity) projected population
length(unique(demo$patid)) - nrow(demo) # 0 -> unique patients in demo table
fig4 <- sum(demo$weight[demo$diagnosis != "-"]) # 36,459,538
sum(demo$diagnosis != "-") # 688,875

# Total T2D-only diagnosed projected population
fig5 <- sum(demo$weight[demo$diagnosis == "Diabetes"]) # 10,969,947

# Total T2D + Obesity diagnosed projected population
fig6 <- sum(demo$weight[demo$diagnosis == "Diabetes + Obesity"]) # 6,925,533

# Total Obesity-only diagnosed projected population
fig7 <- sum(demo$weight[demo$diagnosis == "Obesity"]) # 18,564,058

# treated population figures (in 60m window)************************************

# Total treated (T2D or Obesity) projected population
pts <- data.frame(pat_id = rbind(unique(dosDIA[,.(pat_id)]), unique(dosOBE[,.(pat_id)])), stringsAsFactors = F)
length(unique(pts$pat_id)) - nrow(pts) # 0 => No common pts between dosDIA and dosOBE tables => pts in dosDIA are T2D dx | T2D + Obesity dx, and pts in dosOBE are obesity only dx

# Total T2D only dx treated population
pts <- demo$patid[demo$diagnosis == "Diabetes"]
sel <- drgDIA2$patient[drgDIA2$patient %in% pts]
fig8 <- sum(demo$weight[demo$patid %in% sel]) # 4,295,422

# Total T2D + Obesity dx treated population
pts <- demo$patid[demo$diagnosis == "Diabetes + Obesity"]
sel <- drgDIA2$patient[drgDIA2$patient %in% pts]
fig9 <- sum(demo$weight[demo$patid %in% sel]) # 3,672,063

# Total Obesity only dx treated population
OBErx <- data.frame(drgOBE[,.(patient,weight)], rx = apply(drgOBE[,c(4:63)], 1, function(x) (str_c(unique(x),collapse = ",") != "-")*1), stringsAsFactors = F)
OBErx <- OBErx[OBErx$rx == 1,]
pts <- demo$patid[demo$diagnosis == "Obesity"]
sel <- OBErx$patient[OBErx$patient %in% pts]
fig10 <- sum(demo$weight[demo$patid %in% sel]) # 441,120

fig <- data.frame(cbind(name = c("n_Enroll_pts","n_ce18_pts","N_JP_pop","N_T2D_or_OBE_dx","N_T2D_dx_only",
                                 "N_T2D_&_OBE_dx","N_OBE_dx_only","N_T2D_rx","N_T2D_&_OBE_rx","N_OBE_rx"),
                        val  = c(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10)), stringsAsFactors = F)
fig$val <- as.numeric(fig$val)

rm(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10)
#write.xlsx(fig, "R data/fig_PopDxRx_waterfalll.xlsx")

# Intersection between T2D diagnosed patients and Obesity diagnosed patients (74m detection window) - Venn Diagram*************
dataT2D <- data.frame(demo[demo$diagnosis == "Diabetes" | demo$diagnosis == "Diabetes + Obesity", c("patid","weight")], stringsAsFactors = F)
dataOBE <- data.frame(demo[demo$diagnosis == "Obesity" | demo$diagnosis == "Diabetes + Obesity", c("patid","weight")], stringsAsFactors = F)

sum(dataT2D$weight) # 17,895,480
sum(dataOBE$weight) # 25,489,592
sum(demo$weight[demo$diagnosis == "Diabetes + Obesity"]) # 6,925,533

data <- list(T2D = dataT2D$patid, Obesity = dataOBE$patid)

p <- ggvenn(data, fill_color = c("lightblue","steelblue4")) 
p

# Intersection between T2D dx treated patients and Obesity dx treated patients (60m analysis window) - Venn Diagram*************
dataRxT2D <- data.frame(demo[demo$diagnosis == "Diabetes" | demo$diagnosis == "Diabetes + Obesity", c("patid","weight")], stringsAsFactors = F)
dataRxT2D$rx <- (dataRxT2D$patid %in% drgDIA2$patient | dataRxT2D$patid %in% OBErx$patient)*1
dataRxT2D <- dataRxT2D[dataRxT2D$rx == 1,]

dataRxOBE <- data.frame(demo[demo$diagnosis == "Obesity" | demo$diagnosis == "Diabetes + Obesity", c("patid","weight")], stringsAsFactors = F)
dataRxOBE$rx <- (dataRxOBE$patid %in% drgDIA2$patient | dataRxOBE$patid %in% OBErx$patient)*1
dataRxOBE <- dataRxOBE[dataRxOBE$rx == 1,]

sum(dataRxT2D$weight) # 7,967,485
sum(dataRxOBE$weight) # 4,113,183
dataRxT2D <- left_join(dataRxT2D,demo[,c("patid","diagnosis")], by = "patid")
sum(dataRxT2D$weight[dataRxT2D$diagnosis == "Diabetes + Obesity"]) # 3,672,063

dataRx <- list(T2D = dataRxT2D$patid, Obesity = dataRxOBE$patid)

p <- ggvenn(dataRx, fill_color = c("lightblue","steelblue4")) 
p 



# Figure out the meaning of the letter codes in the Dossiers code field, using for that the DANU Japan Diagnosis Profiles table
dxProf <- dxprofiles[, code_initial := str_sub(code, start = 1L, end = 1L)]
unique(dxProf$code_initial) #  "D" "H" "M" "P" "R"

unique(dxProf$source[dxProf$code_initial == "D"]) # D = "Diagnosis"
unique(dxProf$source[dxProf$code_initial == "H"]) # H = "Health Checkup"
unique(dxProf$source[dxProf$code_initial == "M"]) # M = "Drug"
unique(dxProf$source[dxProf$code_initial == "P"]) # P = "Procedure"
unique(dxProf$source[dxProf$code_initial == "R"]) # R = "Material"

unique(dxProf$description[dxProf$code_initial == "R"]) # "Insulin Therapy: Disposable Syringe For Insulin Preparation Etc. Injection (Standard Type)"
unique(dxProf$description[dxProf$code_initial == "H"]) # "BMI measurement..." & "Diabetes Therapy..."

# Conclusion: M, P, R and H for Diabetes condition => Source <=> Drug Treatment; H for Obesity condition = BMI measurement; D => Source = Diagnosis;

# T2D---------------------------------------------------------------------------
# diagnose pop. source type: Dx, Rx
# rx source type takes precedence (if a pat has both Dx and Rx diagnose sources, it's classified under Rx source type)

dossT2D <- dossiers
dossT2D$code_letter <- str_sub(dossT2D$code, start = 1L, end = 1L) 

temp <- unique(dossT2D[,.(patid,diagnosis)])
length(unique(temp$patid)) - nrow(temp) # -124,431 # In this dossiers table a patient may have both diagnosis: Diabetes and obesity
unique(dossT2D$diagnosis) # "Diabetes", "Obesity" 

dossT2D <- dossT2D[diagnosis == "Diabetes", .(patid,weight,diagnosis,condition,code,code_letter)]
dossT2D <- dossT2D[,unq_codes := str_c(unique(code_letter), collapse = ","), by = .(patid)]
dossT2D <- dossT2D[, source := ifelse(str_detect(unq_codes,paste0(c("M","P","R","H"), collapse = "|")),"Rx","Dx")]
dossT2D <- unique(dossT2D[,.(patid,weight,source)])
length(unique(dossT2D$patid)) - nrow(dossT2D) # 0

dossT2D <- dossT2D[patid %in% demo$patid[demo$diagnosis == "Diabetes" | demo$diagnosis == "Diabetes + Obesity"]] # only pts present in demographics dxed as T2D pts 
dossT2Dgr <- dossT2D[, .(n = .N, N = sum(weight)), by = .(source)]


# Obesity-----------------------------------------------------------------------
# diagnose pop. source type: Dx, Rx, BMI
# rx source type takes precedence over Dx and BMI, and Dx takes precedence over BMI
# Obesity diagnosed only pts

dossOBE <- dossiers
dossOBE <- dossOBE[patid %in% demo$patid[demo$diagnosis == "Obesity"]] # only pts present in demographics dxed as Obesity pts 
dossOBE <- dossOBE[!(patid %in% dossT2D$patid)] # dossiers obesity only pts

dossOBE$code_letter <- str_sub(dossOBE$code, start = 1L, end = 1L) 
unique(dossOBE$diagnosis) # "Diabetes", "Obesity" 

dossOBE <- dossOBE[diagnosis == "Obesity", .(patid,weight,diagnosis,condition,code,code_letter)]
dossOBE <- dossOBE[,unq_codes := str_c(unique(code_letter), collapse = ","), by = .(patid)]
dossOBE <- dossOBE[,source := ifelse(str_detect(unq_codes,paste0(c("M","P"), collapse = "|")),"Rx", ifelse(str_detect(unq_codes,"D"),"Dx","BMI"))]
dossOBE <- unique(dossOBE[,.(patid,weight,source)])
length(unique(dossOBE$patid)) - nrow(dossOBE) # 0

dossOBEgr <- dossOBE[, .(n = .N, N = sum(weight)), by = .(source)]

dataset <- list(DIApts_by_source = dossT2Dgr, OBEpts_by_source = dossOBEgr)
#write.xlsx(dataset,"R data/DIA_OBE_Pts_by_source_type.xlsx")




# T2D---------------------------------------------------------------------------

# distribution of nr of lines of therapy over time (60m window) - Experimental 3 variables contour chart
data <- boxAggDIA[,.(n = sum(n)), by = .(period,line_of_therapy)]

p <- ggplot(data[line_of_therapy <= 6], aes(x = period, y = line_of_therapy, z = n)) + geom_contour_filled()
p <- p + scale_y_continuous(expand = c(0,0))
p <- p + scale_x_continuous(expand = c(0,0))
p <- p + theme_classic()
p

# frequency of nr of lines of therapy in the 60m window (000 pts)
data <- boxAggDIA[,.(N = sum(pats)/1000), by = .(line_of_therapy)]

p <- ggplot(data[line_of_therapy <= 12,], aes(x = factor(line_of_therapy), y = N)) + geom_col(width = 0.7, fill = "lightsteelblue4", alpha = 0.4)
p <- p + scale_x_discrete(expand = c(0.03,0))
p <- p + scale_y_continuous(expand = c(0.0,0), labels = comma)
p <- p + theme_classic() + theme(axis.title = element_blank(), legend.position = "none")
p

#ggsave("R Data/T2D_nrLinesThp.png",device = "png", plot = p, height = 3.5, width = 5.5)


# Obesity-----------------------------------------------------------------------

# frequency of nr of lines of therapy in the 60m window (000 pts)
data <- boxAggOBE[,.(N = sum(pats)/1000), by = .(line_of_therapy)]

p <- ggplot(data[line_of_therapy <= 3,], aes(x = factor(line_of_therapy), y = N)) + geom_col(width = 0.7, fill = "lightsteelblue4", alpha = 0.4)
p <- p + scale_x_discrete(expand = c(0.03,0))
p <- p + scale_y_continuous(expand = c(0.0,0), labels = comma, limits = c(0,15000))
p <- p + theme_classic() + theme(axis.title = element_blank(), legend.position = "none")
p

#ggsave("R Data/OBE_nrLinesThp.png",device = "png", plot = p, height = 3.5, width = 5.5)




# T2D***************************************************************************
# Diabetes drug table only contains ever rx pts in the 60m window analysis
T2DRx <- drgDIA2[,.(patient, weight)]
T2DRx <- setDT(demo[,c("patid","gender","age")])[T2DRx, on = .(patid = patient)]
T2DRx <- T2DRx[,.(patient = patid, weight, age, gender)]
T2DRx <- T2DRx[,.(N = sum(weight)), by = .(age, gender)]

# Obesity***********************************************************************
# Obesity drug table contains all obesity pts, treated + not treated in the 60m window analysis
OBErx <- data.frame(drgOBE[,.(patient,weight)], rx = apply(drgOBE[,c(4:63)], 1, function(x) (str_c(unique(x),collapse = ",") != "-")*1), stringsAsFactors = F)
OBErx <- OBErx[OBErx$rx == 1,]
OBErx <- setDT(OBErx[,c(1:2)])
OBErx <- setDT(demo[,c("patid","gender","age")])[OBErx, on = .(patid = patient)]
OBErx <- OBErx[,.(patient = patid, weight, age, gender)]
OBErx <- OBErx[,.(N = sum(weight)), by = .(age, gender)]

dataset <- list(DIA_Rx_Pts = T2DRx, OBE_Rx_Pts = OBErx)
#write.xlsx(dataset,"R Data/T2D_OBE_Treated_Age&Gender.xlsx")




DIApts <- data.frame(patient = drgDIA2$patient, stringsAsFactors = F)
OBEpts <- data.frame(patient = drgOBE$patient, stringsAsFactors = F)

#fwrite(DIApts, "R data/Marks_DB_DIApts.txt")
#fwrite(OBEpts, "R data/Marks_DB_OBEpts.txt")



# T2D---------------------------------------------------------------------------
flDIAL <- drgDIA2
flDIAL <- flDIAL[,disease := NULL]

# Flows table in long format
flDIAL <- melt(flDIAL, id = c("patient","weight"))
names(flDIAL)[c(3,4)] <- c("p1","v1")
flDIAL <- flDIAL[, p1 := str_extract(p1,"[:digit:]+")]
flDIAL$p1 <- as.numeric(flDIAL$p1)
flDIAL <- data.frame(cbind(flDIAL[p1 < 60], flDIAL[p1 > 1,.(p2 = p1, v2 = v1)]), stringsAsFactors = F)
flDIAL <- flDIAL[,c(1:3,5,4,6)]

# Any flow flag and stops flag**************************************************
flDIAL <- setDT(flDIAL)[, flow := (v1 != v2)*1]
flDIAL <- flDIAL[, stops := (flow == 1 & v2 == "-")*1]

# Treatment experience**********************************************************
RxExp <- data.frame(drgDIA2, stringsAsFactors = F)
RxExp$month1 <- (RxExp$month1 != "-")*1

for(i in 2:60){
  cat(i)
  RxExp[,i+2] <- (((RxExp[,i+2] != "-")*1 + RxExp[,i+2-1]) > 0)*1
}

RxExp <- setDT(RxExp)
RxExp <- melt(RxExp, id = c("patient","weight"))
RxExp <- RxExp[, month := str_extract(variable,"[:digit:]+")]
RxExp$month <- as.numeric(RxExp$month)
names(RxExp)[4] <- "T2D_RxExp"

flDIAL <- RxExp[,.(patient,month,T2D_RxExp)][flDIAL, on = .(patient, month = p1)]
flDIAL <- flDIAL[,.(patient, weight, p1 = month, p2, v1, v2, p1_RxExp = T2D_RxExp, flow, stops)]

# Starts and re-starts flag*****************************************************
flDIAL <- flDIAL[, starts := (flow == 1 & v1 == "-" & p1_RxExp == 0)*1]
flDIAL <- flDIAL[, re_starts := (flow == 1 & v1 == "-" & p1_RxExp == 1)*1]
flDIAL <- flDIAL[, disease := "DIA Japan"]
flDIAL <- flDIAL[,c(12,1:11)]

# Bring Therapy classes (Stocks) to the table***********************************
boxDIAL <- boxDIA2 
boxDIAL <- boxDIAL[,disease := NULL]
boxDIAL <- data.frame(boxDIAL, stringsAsFactors = F)

for(i in 1:60){
  cat(i)
  boxDIAL[,i+2] <- unlist(lapply(boxDIAL[,i+2],function(x) str_extract_all(x,"[:alpha:]+")))
}

setDT(boxDIAL) 
boxDIAL <- melt(boxDIAL, id = c("patient","weight"))
names(boxDIAL)[c(3,4)] <- c("p","s")
boxDIAL <- boxDIAL[, p := str_extract(p,"[:digit:]+")]
boxDIAL$p <- as.numeric(boxDIAL$p)

flDIAL <- boxDIAL[,.(patient,p,s)][flDIAL, on = .(patient, p = p1)]
names(flDIAL)[c(2,3)] <- c("p1","s1")
flDIAL <- boxDIAL[,.(patient,p,s)][flDIAL, on = .(patient, p = p2)]
names(flDIAL)[c(2,3)] <- c("p2","s2")

flDIAL <- flDIAL[,.(disease, patient, weight, p1, p2, v1, v2, s1, s2, p1_RxExp, flow, stops, starts, re_starts)]
names(flDIAL)[c(6,7)] <- c("d1","d2")

# Distinguishing between Naive (N) and Lapsed (x) patient stocks
flDIAL <- flDIAL[p1_RxExp == 0, s1 := "N"]
flDIAL <- flDIAL[p1_RxExp == 0 & s2 == "x", s2 := "N"]

#fwrite(flDIAL,"R data/DIA_Flows_Aux._Long_v2.1.txt")

# total DIA flows - Last 12 months
sum(flDIAL$flow[flDIAL$p1 >= 48]) # 93,276 -> Total flows (nr of flows - n)
sum(flDIAL$weight[flDIAL$p1 >= 48 & flDIAL$flow == 1]) # 6,987,630 -> Total flows (weights - N)

sum(flDIAL$flow[flDIAL$p1 >= 48 & flDIAL$stops == 0]) # 79,735 -> Total flows (nr of flows - n)
sum(flDIAL$weight[flDIAL$p1 >= 48 & flDIAL$flow == 1 & flDIAL$stops == 0]) # 6,043,281 -> Total flows (weights - N)

# Obesity-----------------------------------------------------------------------
flOBEL <- drgOBE
flOBEL <- flOBEL[,disease := NULL]

# Flows table in long format
flOBEL <- melt(flOBEL, id = c("patient","weight"))
names(flOBEL)[c(3,4)] <- c("p1","v1")
flOBEL <- flOBEL[, p1 := str_extract(p1,"[:digit:]+")]
flOBEL$p1 <- as.numeric(flOBEL$p1)
flOBEL <- data.frame(cbind(flOBEL[p1 < 60], flOBEL[p1 > 1,.(p2 = p1, v2 = v1)]), stringsAsFactors = F)
flOBEL <- flOBEL[,c(1:3,5,4,6)]

# Any flow flag and stops flag
flOBEL <- setDT(flOBEL)[, flow := (v1 != v2)*1]
flOBEL <- flOBEL[, stops := (flow == 1 & v2 == "-")*1]

# Treatment experience
RxExp <- data.frame(drgOBE, stringsAsFactors = F)
RxExp$month1 <- (RxExp$month1 != "-")*1

for(i in 2:60){
  cat(i)
  RxExp[,i+2] <- (((RxExp[,i+2] != "-")*1 + RxExp[,i+2-1]) > 0)*1
}

RxExp <- setDT(RxExp)
RxExp <- melt(RxExp, id = c("patient","weight"))
RxExp <- RxExp[, month := str_extract(variable,"[:digit:]+")]
RxExp$month <- as.numeric(RxExp$month)
names(RxExp)[4] <- "OBE_RxExp"

flOBEL <- RxExp[,.(patient,month,OBE_RxExp)][flOBEL, on = .(patient, month = p1)]
flOBEL <- flOBEL[,.(patient, weight, p1 = month, p2, v1, v2, p1_RxExp = OBE_RxExp, flow, stops)]

# Starts and re-starts flag
flOBEL <- flOBEL[, starts := (flow == 1 & v1 == "-" & p1_RxExp == 0)*1]
flOBEL <- flOBEL[, re_starts := (flow == 1 & v1 == "-" & p1_RxExp == 1)*1]
flOBEL <- flOBEL[, disease := "OBE Japan"]
flOBEL <- flOBEL[,c(12,1:11)]

# Bring Therapy classes (Stocks) to the table***********************************
boxOBEL <- boxOBE
boxOBEL <- boxOBEL[,disease := NULL]
boxOBEL <- data.frame(boxOBEL, stringsAsFactors = F)

for(i in 1:60){
  cat(i)
  boxOBEL[,i+2] <- unlist(lapply(boxOBEL[,i+2],function(x) str_extract_all(x,"[:alpha:]+")))
}

setDT(boxOBEL) 
boxOBEL <- melt(boxOBEL, id = c("patient","weight"))
names(boxOBEL)[c(3,4)] <- c("p","s")
boxOBEL <- boxOBEL[, p := str_extract(p,"[:digit:]+")]
boxOBEL$p <- as.numeric(boxOBEL$p)

flOBEL <- boxOBEL[,.(patient,p,s)][flOBEL, on = .(patient, p = p1)]
names(flOBEL)[c(2,3)] <- c("p1","s1")
flOBEL <- boxOBEL[,.(patient,p,s)][flOBEL, on = .(patient, p = p2)]
names(flOBEL)[c(2,3)] <- c("p2","s2")

flOBEL <- flOBEL[,.(disease, patient, weight, p1, p2, v1, v2, s1, s2, p1_RxExp, flow, stops, starts, re_starts)]
names(flOBEL)[c(6,7)] <- c("d1","d2")

# Distinguishing between Naive (N) and Lapsed (x) patient stocks
flOBEL <- flOBEL[p1_RxExp == 0, s1 := "N"]
flOBEL <- flOBEL[p1_RxExp == 0 & s2 == "x", s2 := "N"]

#fwrite(flOBEL,"R data/OBE_Flows_Aux._Long.txt")

# total OBE flows - Last 12 months
sum(flOBEL$flow[flOBEL$p1 >= 48]) # 5,386 -> Total flows (nr of flows - n)
sum(flOBEL$weight[flOBEL$p1 >= 48 & flOBEL$flow == 1]) # 268,754 -> Total flows (weights - N)

sum(flOBEL$flow[flOBEL$p1 >= 48 & flOBEL$stops == 0]) # 2,971 -> Total flows (nr of flows - n)
sum(flOBEL$weight[flOBEL$p1 >= 48 & flOBEL$flow == 1 & flOBEL$stops == 0]) # 148,728 -> Total flows (weights - N)


# period: last 12 months
# without flows to lapsed

#T2D----------------------------------------------------------------------------
data <- flDIALong
nrfl <- data[p1 >= 48 & stops == 0, .(nrFlows = sum(flow)), by = .(patient,weight)]
nrfl <- nrfl[,.(N = sum(weight)), by = .(nrFlows)]

p <- ggplot(nrfl, aes(x = factor(nrFlows), y = N)) + geom_col(width = 0.8, fill = "lightsteelblue4", alpha = 0.4)
p <- p + scale_x_discrete(expand = c(0.03,0))
p <- p + scale_y_continuous(expand = c(0.0,0), labels = comma, limits = c(0,5000000))
p <- p + theme_classic() + theme(axis.title = element_blank(), legend.position = "none")
p

#fwrite(nrfl,"R data/DIA_nrFlows_Last12m.csv")

#Obesity------------------------------------------------------------------------
data <- flOBELong
nrfl <- data[p1 >= 48 & stops == 0, .(nrFlows = sum(flow)), by = .(patient,weight)]
nrfl <- nrfl[,.(N = sum(weight)), by = .(nrFlows)]

p <- ggplot(nrfl, aes(x = factor(nrFlows), y = N)) + geom_col(width = 0.8, fill = "lightsteelblue4", alpha = 0.4)
p <- p + scale_x_discrete(expand = c(0.03,0))
p <- p + scale_y_continuous(expand = c(0.0,0), labels = comma, limits = c(0,20000000))
p <- p + theme_classic() + theme(axis.title = element_blank(), legend.position = "none")
p

#fwrite(nrfl,"R data/OBE_nrFlows_Last12m.csv")



# period: last 12 months
# including flows to lapsed

#T2D----------------------------------------------------------------------------
data <- flDIALong

outfl   <- data[p1 >= 48 & flow == 1 & s1 != s2, .(N = sum(weight)), by = .(s1)]
infl    <- data[p1 >= 48 & flow == 1 & s1 != s2, .(N = sum(weight)), by = .(s2)]
intrafl <- data[p1 >= 48 & flow == 1 & s1 == s2, .(N = sum(weight)), by = .(s1)]

names(outfl)[1]   <- "stock"
names(infl)[1]    <- "stock"
names(intrafl)[1] <- "stock"

flw <- rbind(cbind(outfl, flow = "outflow"), cbind(infl, flow = "inflow"), cbind(intrafl, flow = "intraflow"))
flw <- flw[order(flow,stock)]

#fwrite(flw,"R data/DIA_Flows_by_Thp_Lst12m.csv")

#OBE----------------------------------------------------------------------------
data <- flOBELong

outfl   <- data[p1 >= 48 & flow == 1 & s1 != s2, .(N = sum(weight)), by = .(s1)]
infl    <- data[p1 >= 48 & flow == 1 & s1 != s2, .(N = sum(weight)), by = .(s2)]
intrafl <- data[p1 >= 48 & flow == 1 & s1 == s2, .(N = sum(weight)), by = .(s1)]

names(outfl)[1]   <- "stock"
names(infl)[1]    <- "stock"
names(intrafl)[1] <- "stock"

flw <- rbind(cbind(outfl, flow = "outflow"), cbind(infl, flow = "inflow"), cbind(intrafl, flow = "intraflow"))
flw <- flw[order(flow,stock)]

#fwrite(flw,"R data/OBE_Flows_by_Thp_Lst12m.csv")



# period: last 12 months

#T2D----------------------------------------------------------------------------
data <- flDIALong

inflGLP1 <- data[p1 >= 48 & flow == 1 & (s2 == "g" | s2 == "G"), .(N = sum(weight)), by = .(s1,s2)]

#fwrite(inflGLP1,"R data/T2D_GLP1_Inflows_Lst12m.csv")

#OBE----------------------------------------------------------------------------
data <- flOBELong

inflGLP1 <- data[p1 >= 48 & flow == 1 & (s2 == "g" | s2 == "G"), .(N = sum(weight)), by = .(s1,s2)]

#fwrite(inflGLP1,"R data/OBE_GLP1_Inflows_Lst12m.csv")




#T2D---------------------------------------------------------------------------------------------------------------------
# Therapy classes to analyse Intraflows: SGLT2, DPP4, Insulins 
# Identify changes by therapy class

data <- flDIALong[flow == 1 & s1 == s2,.(patient,weight,p1,p2,d1,d2,s1,s2,flow)]

# Determine changes in class and number of drugs, for s1 & s2, for each therapy class************************************

# Biguanide Therapy class (b) - flags
data <- data[, big_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Big), str_c(unlist(str_extract_all(x, string_Big)), collapse = ","),"")))]
data <- data[, big_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Big), str_c(unlist(str_extract_all(x, string_Big)), collapse = ","),"")))]
data <- data[, nr_big_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Big)*1), str_split(x,","))))]
data <- data[, nr_big_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Big)*1), str_split(x,","))))]
data <- data[, nr_bigUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("big_d1","big_d2")] 
data <- data[, big_flow_type := ifelse(nr_big_d2 < nr_big_d1 & nr_bigUnq_d1d2 > nr_big_d1, "D+S", 
                                ifelse(nr_big_d2 > nr_big_d1 & nr_bigUnq_d1d2 > nr_big_d2, "A+S",
                                ifelse(nr_big_d2 < nr_big_d1, "D", 
                                ifelse(nr_big_d2 > nr_big_d1, "A", 
                                ifelse(nr_big_d2 == nr_big_d1 & big_d2 != big_d1, "S","-")))))] 

# Antidiabetic Therapy class (d) - flags
data <- data[, antiD_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_AntiDIA),str_c(unlist(str_extract_all(x, string_AntiDIA)),collapse = ","),"")))]
data <- data[, antiD_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_AntiDIA),str_c(unlist(str_extract_all(x, string_AntiDIA)),collapse = ","),"")))]
data <- data[, nr_antiD_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_AntiDIA)*1), str_split(x,","))))]
data <- data[, nr_antiD_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_AntiDIA)*1), str_split(x,","))))]
data <- data[, nr_antiDUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("antiD_d1","antiD_d2")] 
data <- data[, antiD_flow_type := ifelse(nr_antiD_d2 < nr_antiD_d1 & nr_antiDUnq_d1d2 > nr_antiD_d1, "D+S", 
                                ifelse(nr_antiD_d2 > nr_antiD_d1 & nr_antiDUnq_d1d2 > nr_antiD_d2, "A+S",
                                ifelse(nr_antiD_d2 < nr_antiD_d1, "D", 
                                ifelse(nr_antiD_d2 > nr_antiD_d1, "A", 
                                ifelse(nr_antiD_d2 == nr_antiD_d1 & antiD_d2 != antiD_d1, "S","-")))))] 

# DPP4 Therapy class (D) - flags
data <- data[, DPP4_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_DPP4), str_c(unlist(str_extract_all(x, string_DPP4)), collapse = ","),"")))]
data <- data[, DPP4_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_DPP4), str_c(unlist(str_extract_all(x, string_DPP4)), collapse = ","),"")))]
data <- data[, nr_DPP4_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_DPP4)*1), str_split(x,","))))]
data <- data[, nr_DPP4_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_DPP4)*1), str_split(x,","))))]
data <- data[, nr_DPP4Unq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("DPP4_d1","DPP4_d2")] 
data <- data[, DPP4_flow_type := ifelse(nr_DPP4_d2 < nr_DPP4_d1 & nr_DPP4Unq_d1d2 > nr_DPP4_d1, "D+S", 
                                ifelse(nr_DPP4_d2 > nr_DPP4_d1 & nr_DPP4Unq_d1d2 > nr_DPP4_d2, "A+S",
                                ifelse(nr_DPP4_d2 < nr_DPP4_d1, "D", 
                                ifelse(nr_DPP4_d2 > nr_DPP4_d1, "A", 
                                ifelse(nr_DPP4_d2 == nr_DPP4_d1 & DPP4_d2 != DPP4_d1, "S","-")))))] 

# SGLT2 Therapy class (S) - flags
data <- data[, SGLT2_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_SGLT2), str_c(unlist(str_extract_all(x, string_SGLT2)), collapse = ","),"")))]
data <- data[, SGLT2_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_SGLT2), str_c(unlist(str_extract_all(x, string_SGLT2)), collapse = ","),"")))]
data <- data[, nr_SGLT2_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_SGLT2)*1), str_split(x,","))))]
data <- data[, nr_SGLT2_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_SGLT2)*1), str_split(x,","))))]
data <- data[, nr_SGLT2Unq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("SGLT2_d1","SGLT2_d2")] 
data <- data[, SGLT2_flow_type := ifelse(nr_SGLT2_d2 < nr_SGLT2_d1 & nr_SGLT2Unq_d1d2 > nr_SGLT2_d1, "D+S", 
                                ifelse(nr_SGLT2_d2 > nr_SGLT2_d1 & nr_SGLT2Unq_d1d2 > nr_SGLT2_d2, "A+S",
                                ifelse(nr_SGLT2_d2 < nr_SGLT2_d1, "D", 
                                ifelse(nr_SGLT2_d2 > nr_SGLT2_d1, "A", 
                                ifelse(nr_SGLT2_d2 == nr_SGLT2_d1 & SGLT2_d2 != SGLT2_d1, "S","-")))))] 

# GLP1 Oral Therapy class (g) - flags
data <- data[, GLP1O_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x,string_GLP1Oral),str_c(unlist(str_extract_all(x,string_GLP1Oral)),collapse = ","),"")))]
data <- data[, GLP1O_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x,string_GLP1Oral),str_c(unlist(str_extract_all(x,string_GLP1Oral)),collapse = ","),"")))]
data <- data[, nr_GLP1O_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_GLP1Oral)*1), str_split(x,","))))]
data <- data[, nr_GLP1O_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_GLP1Oral)*1), str_split(x,","))))]
data <- data[, nr_GLP1OUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("GLP1O_d1","GLP1O_d2")] 
data <- data[, GLP1O_flow_type := ifelse(nr_GLP1O_d2 < nr_GLP1O_d1 & nr_GLP1OUnq_d1d2 > nr_GLP1O_d1, "D+S", 
                                ifelse(nr_GLP1O_d2 > nr_GLP1O_d1 & nr_GLP1OUnq_d1d2 > nr_GLP1O_d2, "A+S",
                                ifelse(nr_GLP1O_d2 < nr_GLP1O_d1, "D", 
                                ifelse(nr_GLP1O_d2 > nr_GLP1O_d1, "A", 
                                ifelse(nr_GLP1O_d2 == nr_GLP1O_d1 & GLP1O_d2 != GLP1O_d1, "S","-")))))] 

# GLP1 Injectable Therapy class (G) - flags
data <- data[, GLP1I_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x,string_GLP1Inj),str_c(unlist(str_extract_all(x,string_GLP1Inj)),collapse = ","),"")))]
data <- data[, GLP1I_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x,string_GLP1Inj),str_c(unlist(str_extract_all(x,string_GLP1Inj)),collapse = ","),"")))]
data <- data[, nr_GLP1I_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_GLP1Inj)*1), str_split(x,","))))]
data <- data[, nr_GLP1I_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_GLP1Inj)*1), str_split(x,","))))]
data <- data[, nr_GLP1IUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("GLP1I_d1","GLP1I_d2")] 
data <- data[, GLP1I_flow_type := ifelse(nr_GLP1I_d2 < nr_GLP1I_d1 & nr_GLP1IUnq_d1d2 > nr_GLP1I_d1, "D+S", 
                                ifelse(nr_GLP1I_d2 > nr_GLP1I_d1 & nr_GLP1IUnq_d1d2 > nr_GLP1I_d2, "A+S",
                                ifelse(nr_GLP1I_d2 < nr_GLP1I_d1, "D", 
                                ifelse(nr_GLP1I_d2 > nr_GLP1I_d1, "A", 
                                ifelse(nr_GLP1I_d2 == nr_GLP1I_d1 & GLP1I_d2 != GLP1I_d1, "S","-")))))]

# Insulins Therapy class (I) - flags
data <- data[, Ins_d1 := unlist(lapply(d1, function(x) ifelse(str_detect(x, string_Ins), str_c(unlist(str_extract_all(x, string_Ins)), collapse = ","),"")))]
data <- data[, Ins_d2 := unlist(lapply(d2, function(x) ifelse(str_detect(x, string_Ins), str_c(unlist(str_extract_all(x, string_Ins)), collapse = ","),"")))]
data <- data[, nr_Ins_d1 := unlist(lapply(d1, function(x) mapply(function (x) sum(str_detect(x, string_Ins)*1), str_split(x,","))))]
data <- data[, nr_Ins_d2 := unlist(lapply(d2, function(x) mapply(function (x) sum(str_detect(x, string_Ins)*1), str_split(x,","))))]
data <- data[, nr_InsUnq_d1d2 := .(apply(.SD, 1, function(x) sum((unique(unlist(str_split(str_c(x,","),","))) != "")*1))), ,.SDcols = c("Ins_d1","Ins_d2")] 
data <- data[, Ins_flow_type := ifelse(nr_Ins_d2 < nr_Ins_d1 & nr_InsUnq_d1d2 > nr_Ins_d1, "D+S", 
                                ifelse(nr_Ins_d2 > nr_Ins_d1 & nr_InsUnq_d1d2 > nr_Ins_d2, "A+S",
                                ifelse(nr_Ins_d2 < nr_Ins_d1, "D", 
                                ifelse(nr_Ins_d2 > nr_Ins_d1, "A", 
                                ifelse(nr_Ins_d2 == nr_Ins_d1 & Ins_d2 != Ins_d1, "S","-")))))] 


# Intraflows summary table*********************************************************************************************
# A - adding drugs; D - Dropping drugs; S - Switch of drugs; A + S - Adding and switching drugs; D + S - Dropping and switching drugs; 

intrFlw <- data[,.(patient, weight, p1, p2, d1, d2, s1, s2, flow, 
                   big_flow_type, antiD_flow_type, DPP4_flow_type, SGLT2_flow_type, GLP1O_flow_type, GLP1I_flow_type, Ins_flow_type)]
intrFlw <- intrFlw[, year := ifelse(p2 <= 12, 1, ifelse(p2 <= 24, 2, ifelse(p2 <= 36, 3, ifelse(p2 <= 48, 4, 5))))]

#fwrite(intrFlw,"R data/Intraflows_HighGranularity_table.csv")

# Looking at a single Primary Stock Therapy at a time, & grouping by any therapy intraflow type combination************
# Single Primary Stocks Therapies to look at: Insulins, SGLT2, DPP4

# Insulins - last 12 months+++++++++++++++++++++++++++++++++++++++++++++++++++++
intrIns <- intrFlw[s1 == "I" & year == 5, .(N = sum(weight)), 
                   by = .(big_flow_type,antiD_flow_type,DPP4_flow_type,SGLT2_flow_type,GLP1O_flow_type,GLP1I_flow_type,Ins_flow_type)]
intrIns$perc <- intrIns$N / sum(intrIns$N)
intrIns <- intrIns[order(-N)]
intrIns$cum_perc <- cumsum(intrIns$perc) 

# Applying hierarchy to each intraflows combination (just one classification for each intraflows combination) and Grouping Intraflows according % share level;
# Groups: Insulins A+S | D+S => Insulin Combo; Other (all together) Any combination of A | D | S | A+S | D+S => Other Combo; Other (all together) A => Other A; Other (all together) D => Other D; Other (all together) S => Other S;
# Hierarchy: Insulins A | D | S | Combo > Other

intrIns <- intrIns[Ins_flow_type != "-" & (Ins_flow_type == "D+S" | Ins_flow_type == "A+S"), Intraflow := "Ins combo"]
intrIns <- intrIns[Ins_flow_type != "-" & is.na(Intraflow), Intraflow := paste("Ins", Ins_flow_type)]

intrIns <- intrIns[, OtherThp_flw_type := .(mapply(function(x) ifelse(sum((str_split(x,",") != "")*1) > 0, str_c(unlist(x), collapse = ","),""),
                                                   apply(.SD,1,function(x) paste0(unique(x[x!="-"]))))), ,
                   .SDcols = c("big_flow_type","antiD_flow_type","DPP4_flow_type","SGLT2_flow_type","GLP1O_flow_type","GLP1I_flow_type")]
intrIns <- intrIns[, OtherThp_flw_type_aux := lapply(unlist(OtherThp_flw_type), function(x) (str_count(x,"[:alpha:]") > 1)*1)]
intrIns <- intrIns[Ins_flow_type == "-" & OtherThp_flw_type_aux == 1, Intraflow := "Other combo"]
intrIns <- intrIns[Ins_flow_type == "-" & OtherThp_flw_type_aux == 0, Intraflow := paste("Other", OtherThp_flw_type)]

intrInsGr <- intrIns[,.(N = sum(N)), by = .(Intraflow)]
intrInsGr <- intrInsGr[, perc := N/sum(N)]


# SGLT2 - last 12 months++++++++++++++++++++++++++++++++++++++++++++++++++++++++
intrSGLT2 <- intrFlw[s1 == "S" & year == 5, .(N = sum(weight)), by = .(big_flow_type,antiD_flow_type,DPP4_flow_type,SGLT2_flow_type)]
intrSGLT2$perc <- round((intrSGLT2$N / sum(intrSGLT2$N))*100,3)
intrSGLT2 <- intrSGLT2[order(-perc)]
intrSGLT2$cum_perc <- cumsum(intrSGLT2$perc) 

# Applying Hierarchy to each intraflows combination and Grouping intraflows according to % share level;
# Groups: SGLT2 A; SGLT2 D; SGLT2 S; SGLT2 Combo (A+S | D+S); antiD A; Big A; DPP4 A; antiD D; DPP4 D; Other;  
# Hierarchy (based on: Primary switch & biggest share): SGLT2 A | D | S | Combo > antiD A > Big A > DPP4 A > antiD D > DPP4 D > Other

intrSGLT2 <- intrSGLT2[, intraflow := "Other"]
intrSGLT2 <- intrSGLT2[DPP4_flow_type == "D", intraflow := "DPP4 D"]
intrSGLT2 <- intrSGLT2[antiD_flow_type == "D", intraflow := "antiD D"]
intrSGLT2 <- intrSGLT2[DPP4_flow_type == "A", intraflow := "DPP4 A"]
intrSGLT2 <- intrSGLT2[big_flow_type == "A", intraflow := "Big A"]
intrSGLT2 <- intrSGLT2[antiD_flow_type == "A", intraflow := "antiD A"]
intrSGLT2 <- intrSGLT2[SGLT2_flow_type == "A+S" | SGLT2_flow_type == "D+S", intraflow := "SGLT2 Combo"]
intrSGLT2 <- intrSGLT2[SGLT2_flow_type == "S", intraflow := "SGLT2 S"]
intrSGLT2 <- intrSGLT2[SGLT2_flow_type == "D", intraflow := "SGLT2 D"]
intrSGLT2 <- intrSGLT2[SGLT2_flow_type == "A", intraflow := "SGLT2 A"]

intrSGLT2Gr <- intrSGLT2[,.(N = sum(N)), by = .(intraflow)]
intrSGLT2Gr <- intrSGLT2Gr[, perc := N/sum(N)]


# DPP4 - last 12 months+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
intrDPP4 <- intrFlw[s1 == "D" & year == 5, .(N = sum(weight)), by = .(big_flow_type,antiD_flow_type,DPP4_flow_type)]
intrDPP4$perc <- round((intrDPP4$N / sum(intrDPP4$N))*100,3)
intrDPP4 <- intrDPP4[order(-perc)]
intrDPP4$cum_perc <- cumsum(intrDPP4$perc) 

# Applying Hierarchy to each intraflows combination and Grouping intraflows according to % share level;
# Groups: DPP4 A; DPP4 D; DPP4 S; DPP4 Combo (A+S | D+S); antiD A; Big A; antiD D; Big D; Other;
# Hierarchy (based on: Primary switch & biggest share): DPP4 A | D | S | Combo > antiD A > Big A > antiD D > Big D > other

intrDPP4 <- intrDPP4[, intraflow := "Other"]
intrDPP4 <- intrDPP4[big_flow_type == "D", intraflow := "Big D"]
intrDPP4 <- intrDPP4[antiD_flow_type == "D", intraflow := "antiD D"]
intrDPP4 <- intrDPP4[big_flow_type == "A", intraflow := "Big A"]
intrDPP4 <- intrDPP4[antiD_flow_type == "A", intraflow := "antiD A"]
intrDPP4 <- intrDPP4[DPP4_flow_type == "A+S" | DPP4_flow_type == "D+S", intraflow := "DPP4 Combo"]
intrDPP4 <- intrDPP4[DPP4_flow_type == "S", intraflow := "DPP4 S"]
intrDPP4 <- intrDPP4[DPP4_flow_type == "D", intraflow := "DPP4 D"]
intrDPP4 <- intrDPP4[DPP4_flow_type == "A", intraflow := "DPP4 A"]

intrDPP4Gr <- intrDPP4[,.(N = sum(N)), by = .(intraflow)]
intrDPP4Gr <- intrDPP4Gr[, perc := N/sum(N)]


length(unique(unlist(intrFlw[s1 == "I" & year == 5, .(patient)]))) # n = 4,342
length(unique(unlist(intrFlw[s1 == "S" & year == 5, .(patient)]))) # n = 7,623
length(unique(unlist(intrFlw[s1 == "D" & year == 5, .(patient)]))) # n = 5,106

unlist(unique(intrFlw[s1 == "I" & year == 5, .(patient,weight)])[,.(sum(weight))]) # N = 351,768
unlist(unique(intrFlw[s1 == "S" & year == 5, .(patient,weight)])[,.(sum(weight))]) # N = 460,656
unlist(unique(intrFlw[s1 == "D" & year == 5, .(patient,weight)])[,.(sum(weight))]) # N = 488,724

dataset <- list(Insulins = intrInsGr, SGLT2 = intrSGLT2Gr, DPP4 = intrDPP4Gr)
#write.xlsx(dataset,"R data/Intraflows_Insulins_SGLT2_DPP4_by_groups-last 12m.xlsx")



# T2D---------------------------------------------------------------------------
data <- flDIALong[p1 >= 48 & flow == 1]

sMx <- data[, .(N = round(sum(weight),0)), by = .(s1, s2)]

rank <- data.frame(cbind(stock = c("N","x","b","d","D","S","g","G","I"), rank = c(seq(1,9,1))), stringsAsFactors = F)

sMx <- dcast(sMx, s1 ~ s2, value.var = "N")
sMx <- setDT(rank)[sMx, on = .(stock = s1)]
sMx <- sMx[order(rank)]
sMx <- sMx[,.(s1 = stock, N = 0, x, b, d, D, S, g, G, I)]
sMx[is.na(sMx)] <- 0

#write.xlsx(sMx,"R data/DIA_SMatrix_L12m.xlsx")


# Last 12 months
# Using observed drug history
# Treated patients

#T2D----------------------------------------------------------------------------
data1                <- flDIALong[p1 >= 49, .(patient,weight,p1,d1)]
names(data1)[c(3,4)] <- c("month","drugs")  

data2                <- flDIALong[p2 == 60, .(patient,weight,p2,d2)]
names(data2)[c(3,4)] <- c("month","drugs") 

data <- setDT(data.frame(rbind(data1,data2)))

# nr of lines & nr of months rx by patient
data$Rx <- (data$drugs != "-")*1 
data <- data[Rx == 1,  ':=' (nr_lines = length(unique(drugs)), months_rx = sum(Rx)), by = .(patient)]
data <- data[Rx == 1]

# nr of unique molecules by patient
data       <- data[, nrUnqMols := lapply(.(drugs), function(x) length(unique(unlist(str_split(x,","))))), by = .(patient)]

# Grouping data:
# nr of lines vs nr of months Rx
# % of months rx by 'nr of lines vs nr of months Rx' groups 
# avg nr of unique molecules by nr of lines of therapy

data <- unique(data[,.(patient, weight, nr_lines, months_rx, nrUnqMols)])
length(unique(data$patient)) - nrow(data) # 0 -> Okay, each patient have just one nr_lines and months_rx record

length(unique(data$patient)) # n = 86,126
sum(data$weight) # N = 7,164,112

data$nr_lines_groups    <- ifelse(data$nr_lines == 1, "1 line", ifelse(data$nr_lines == 2, "2 lines", ifelse(data$nr_lines <= 4, "3-4 lines", "5+ lines")))
data$nr_mRx_groups <- ifelse(data$months_rx <= 3, "1-3 months", ifelse(data$months_rx <= 8, "4-8 months", ifelse(data$months_rx <= 11, "9-11 months", "12 months")))

data1 <- data[, .(N = sum(weight)), by = .(nr_lines_groups, nr_mRx_groups)][order(nr_lines_groups,nr_mRx_groups)] # aggregation by nr lines and nr months Rx
data2 <- data[, .(Tot_nr_months = sum(months_rx*weight)), by = .(nr_lines_groups, nr_mRx_groups)]
data2 <- data2[, Perc := Tot_nr_months/sum(Tot_nr_months)]
data3 <- data[, .(avgNr_UnqMols = round(sum(nrUnqMols*weight)/sum(weight),1)), by = .(nr_lines_groups)]

dataset <- list(Pts_by_nrLines_mRx_groups = data1, mRx_by_nrLines_mRx_groups = data2, avg_nr_Unq_Mols = data3)
#write.xlsx(dataset,"R data/T2D_RxPts_nrLines_vs_monthsRx_last12m.xlsx")


#Obesity------------------------------------------------------------------------
data1                <- flOBELong[p1 >= 49, .(patient,weight,p1,d1)]
names(data1)[c(3,4)] <- c("month","drugs")  

data2                <- flOBELong[p2 == 60, .(patient,weight,p2,d2)]
names(data2)[c(3,4)] <- c("month","drugs") 

data <- setDT(data.frame(rbind(data1,data2)))

# nr of lines & nr of months rx by patient
data$Rx <- (data$drugs != "-")*1 
data <- data[Rx == 1,  ':=' (nr_lines = length(unique(drugs)), months_rx = sum(Rx)), by = .(patient)]
data <- data[Rx == 1]

# nr of unique molecules by patient
data <- data[, nrUnqMols := lapply(.(drugs), function(x) length(unique(unlist(str_split(x,","))))), by = .(patient)]

# Grouping data:
# nr of lines vs nr of months Rx
# % of months rx by 'nr of lines vs nr of months Rx' groups 
# avg nr of unique molecules by nr of lines of therapy

data <- unique(data[,.(patient, weight, nr_lines, months_rx, nrUnqMols)])
length(unique(data$patient)) - nrow(data) # 0 -> Okay, each patient have just one nr_lines and months_rx record

length(unique(data$patient)) # n = 3,785
sum(data$weight) # N = 191,812

max(data$nr_lines) # 4
max(data$months_rx) # 12

data$nr_lines_groups    <- ifelse(data$nr_lines == 1, "1 line", "2+ lines")
data$nr_mRx_groups <- ifelse(data$months_rx <= 3, "1-3 months", ifelse(data$months_rx <= 8, "4-8 months", ifelse(data$months_rx <= 11, "9-11 months", "12 months")))

data1 <- data[, .(N = sum(weight)), by = .(nr_lines_groups, nr_mRx_groups)][order(nr_lines_groups,nr_mRx_groups)]
data2 <- data[, .(Tot_nr_months = sum(months_rx*weight)), by = .(nr_lines_groups, nr_mRx_groups)]
data2 <- data2[, Perc := Tot_nr_months/sum(Tot_nr_months)]
data3 <- data[, .(avgNr_UnqMols = round(sum(nrUnqMols*weight)/sum(weight),1)), by = .(nr_lines_groups)]

dataset <- list(Pts_by_nrLines_mRx_groups = data1, mRx_by_nrLines_mRx_groups = data2, avg_nr_Unq_Mols = data3)
#write.xlsx(dataset,"R data/Obesity_RxPts_nrLines_vs_monthsRx_last12m.xlsx")




# T2D---------------------------------------------------------------------------

data <- data.frame(drgDIA2, stringsAsFactors = F)

nrLines <- data[,c(1:3)] 
nrLines$month1 <- (data$month1 != "-")*1

for(i in 2:60){
  cat(i)
  nrLines[,i+3] <- apply(data[,(4:(i+3))], 1, function(x) length(unique(x[x!="-"])))
  names(nrLines)[i+3] <- paste0("month",i)
}

#fwrite(nrLines,"R data/DIA_nrLines_Histories.txt")

# Distribution of pts by stock and nr of lines by month
stk    <- melt(boxDIA2, id = c("disease","patient","weight")) # Stocks long format
names(stk)[c(4,5)] <- c("month","stock")
nrLThp <- gather(nrLines, key = "month", value = "nr_lines", -c("disease","patient","weight")) # nr of lines of therapy long format

stkNrL <- setDT(nrLThp[,c("patient","month","nr_lines")])[stk, on = .(patient, month)]
stkNrL <- stkNrL[,.(disease, patient, weight, month, stock, nr_lines)]
stkNrL <- stkNrL[,.(N = sum(weight)), by = .(month, stock, nr_lines)]
stkNrL <- stkNrL[, month_nr := str_extract_all(month, "[:digit:]+")]

stkNrL$month_nr <- as.numeric(stkNrL$month_nr)
stkNrL <- dcast(stkNrL, formula = stock + nr_lines ~ month_nr, value.var = "N")
stkNrL[is.na(stkNrL)] <- 0




#T2D----------------------------------------------------------------------------
# month 60

# nr lines
data <- fread("R data/DIA_nrLines_Histories.txt", integer64 = "character", stringsAsFactors = F)
nrlm60 <- data[,.(patient, weight, month60)]
nrlm60 <- setDT(demo[, c("patid","age")])[nrlm60, on = .(patid = patient)]

nrlnsmpl <- nrlm60[,.N, by = .(month60)][order(month60)]
names(nrlnsmpl)[1] <- "nr_lines_m60"
nrlnsmpl <- nrlnsmpl[, perc := (N/sum(N))*100] 

nrlm60  <- nrlm60[month60 >= 12, month60 := 12] # grouping in 12+ lines 

# bring stocks in month60
nrlm60 <- boxDIA2[,.(patient, month60)][nrlm60, on = .(patient = patid)]
nrlm60 <- nrlm60[,.(patient, weight, age, stock_m60 = month60, nr_lines_m60 = i.month60)]
nrlm60smpl <- nrlm60[, .N, by = .(stock_m60, nr_lines_m60)][order(stock_m60,nr_lines_m60)]
nrlm60smpl <- nrlm60smpl[, perc := (N/sum(N))*100, by = .(stock_m60)]
nrlm60smpl <- nrlm60smpl[, cumPerc := cumsum(perc), by = .(stock_m60)]

nrlm60 <- nrlm60[, nr_lines_m60_v2 := ifelse(nr_lines_m60 < 6, nr_lines_m60, 6)]  #grouping in 6+ lines alternative version
nrlm60smplv2 <- nrlm60[,.(n = .N), by = .(nr_lines_m60_v2)] 

nrlm60Any <- nrlm60
nrlm60Any <- nrlm60Any[, .(patient, weight, age, stock_m60 = "Any", nr_lines_m60, nr_lines_m60_v2)]
nrlm60 <- rbind(nrlm60, nrlm60Any) # adding all stock series (replicated DIA pts with generic stock) 

nrlm60Exp <- nrlm60[, ':=' (weight2 = round(weight,0), weight = NULL)]
nrlm60Exp <- expandRows(nrlm60Exp, count = "weight2")

nrlm60Exp$nr_lines_m60 <- factor(nrlm60Exp$nr_lines_m60, levels = c("1","2","3","4","5","6","7","8","9","10","11","12")) 
levels(nrlm60Exp$nr_lines_m60) <- c("1","2","3","4","5","6","7","8","9","10","11","12+")

nrlm60Exp$nr_lines_m60_v2 <- factor(nrlm60Exp$nr_lines_m60_v2, levels = c("1","2","3","4","5","6"))
levels(nrlm60Exp$nr_lines_m60_v2) <- c("1","2","3","4","5","6+")

nrlm60Exp$stock_m60 <- factor(nrlm60Exp$stock_m60, levels = c("Any", "b", "d", "D", "S", "g", "G","I","x"))
levels(nrlm60Exp$stock_m60) <- c("Any Therapy","Biguanide","Antidiabetic","DDP-4","SGLT2","GLP1 Oral","GLP1 Injectable","Insulin","Lapsed")

medians <- nrlm60Exp[, .(median = median(age)), by = .(stock_m60)]


# box_plot (uses weights - expanded samples by their weight)
p <- ggplot(nrlm60Exp, aes(x = nr_lines_m60_v2, y = age)) + geom_boxplot(outlier.shape = NA)
p <- p + scale_x_discrete(expand = c(0.08,0))
p <- p + scale_y_continuous(expand = c(0.01,0), breaks = seq(30,74,4), limits = c(30,74))
p <- p + coord_flip()
p <- p + theme_classic() + theme(axis.title = element_blank(), panel.spacing.x = unit(1, "lines"), panel.spacing.y = unit(1.5, "lines"))
p <- p + facet_wrap(~stock_m60)
p

#ggsave("R Data/T2D_nrLinesAgeDist.png",device = "png", plot = p, height = 4.5, width = 7.5)

# ridgeplot (using weights - expanded samples by their weight)
p <- ggplot(nrlm60Exp, aes(x = age, fill = nr_lines_m60_v2)) + geom_density_ridges(aes(y = nr_lines_m60_v2), fill = "lightsteelblue3", color = "darkslategrey",
                                                                                  alpha = 0.6)
p <- p + facet_wrap(~ nrlm60Exp$stock_m60)
p <- p + scale_x_continuous(expand = c(0.02,0),limits = c(30,74), breaks = seq(30,74,2))
#p <- p + scale_y_discrete(expand = c(0.01,0), limits=rev)
p <- p + theme_classic() + theme(axis.title = element_blank(), panel.spacing.x = unit(2, "lines"),
                                 legend.position = "none", panel.background = element_rect(fill = "grey100"))
p



# nr of scripts over time*******************************************************
# excluding scripts of type G - gap fill
# for the 60 month window - 2016/05/01 to 2021/04/30

data <- dosDIA2[status != "G" & drug_group != "Antiobesity" & drug_group != "Surgery"]
data <- data[,to_dt := from_dt + dayssup]

nrScrpt <- data.frame()
for(i in 1:nrow(convertMonths)){
  cat(i)
  nrScrpt <- rbind(nrScrpt, cbind(month = i, data[from_dt <= convertMonths$max[i] & to_dt >= convertMonths$min[i], .(N = sum(weight))]))
}

# nr of unique pts over time****************************************************
nrpts <- data.frame()
for(i in 1:nrow(convertMonths)){
  cat(i)
  temp  <- unique(data[from_dt <= convertMonths$max[i] & to_dt >= convertMonths$min[i], .(pat_id,weight)])
  nrpts <- rbind(nrpts, cbind(month = i, temp[, .(N = sum(weight))]))
}

# avg. nr of scripts per patient by month***************************************
nrSptPts <- left_join(nrpts, nrScrpt, by = "month") 
names(nrSptPts)[2:3] <- c("nr_pts","nr_scripts")
nrSptPts$avg_nrScripts_perPat <- round(nrSptPts$nr_scripts/nrSptPts$nr_pts,2)
nrSptPts$month_60w <- ifelse(nrSptPts$month < 13, 0, ifelse(nrSptPts$month > 72, 0, nrSptPts$month-12))

#write.xlsx(nrSptPts,"R data/DIA_nrScript_per_pat_monthly.xlsx")



#T2D----------------------------------------------------------------------------
data <- flDIALong

stksOtime <- data[, .(N = sum(weight)), by = .(p1, s1)]
stksm60   <- data[p2 == 60, .(N = sum(weight)), by = .(p2, s2)]

names(stksOtime)[c(1:2)] <- c("month","Stock")
names(stksm60)[c(1:2)]   <- c("month","Stock")

stksOtime <- setDT(rbind(stksOtime, stksm60))
stksOtime <- setDT(unique(drgLkup[,c("box_code","box_name")]))[stksOtime, on = .(box_code = Stock)]
stksOtime <- stksOtime[box_code == "x", box_name := "Lapsed"] 
stksOtime <- stksOtime[box_code == "N", box_name := "Naive"]

#fwrite(stksOtime,"R data/T2D_Stocks_Over_time.csv")

#Obesity------------------------------------------------------------------------
data <- flOBELong

stksOtime <- data[, .(N = sum(weight)), by = .(p1, s1)]
stksm60   <- data[p2 == 60, .(N = sum(weight)), by = .(p2, s2)]

names(stksOtime)[c(1:2)] <- c("month","Stock")
names(stksm60)[c(1:2)]   <- c("month","Stock")

stksOtime <- setDT(rbind(stksOtime, stksm60))
stksOtime <- setDT(unique(drgLkup[,c("box_code","box_name")]))[stksOtime, on = .(box_code = Stock)]
stksOtime <- stksOtime[box_code == "x", box_name := "Lapsed"] 
stksOtime <- stksOtime[box_code == "N", box_name := "Naive"]

#fwrite(stksOtime,"R data/Obesity_Stocks_Over_time.csv")



# Stocks month 60

# T2D---------------------------------------------------------------------------
data <- flDIALong[p2 == 60, .(patient, weight, s2)]

# bring age and gender
data <- setDT(demo[,c("patid","gender","age")])[data, on = .(patid = patient)]
data <- data[,.(patid, weight, age, gender, stock_m60 = s2)]

length(unique(data$patid)) - nrow(data) # 0

# sample patients per stock
stksmpl <- data[,.(n = .N), by = .(stock_m60)]

# expand observations by their weight - for box_plot****************************
data$weight2 <- round(data$weight,0)
dataExp <- expandRows(data, count = "weight2")
dataExp$weight <- NULL

median(dataExp$age) # 67 -> median age regardless of stock
medians <- dataExp[,.(median = median(age), mean = mean(age)), by = .(stock_m60)][order(-median)]
dataExp$stock_m60 <- factor(dataExp$stock_m60, levels = c("g","S","G","b","x","I","D","d"))
levels(dataExp$stock_m60) <- c("GLP1 Oral","SGLT2","GLP1 Inj.","Biguanides","Lapsed","Insulins","DPP4","Antidiabetic")

p <- ggplot(dataExp, aes(x = stock_m60, y = age)) + geom_boxplot(outlier.shape = NA)
#p <- p + facet_wrap(~gender)
p <- p + scale_x_discrete(expand = c(0.08,0))
p <- p + scale_y_continuous(expand = c(0.01,0), breaks = seq(30,74,2), limits = c(30,74))
p <- p + coord_flip()
#p <- p + scale_y_reverse(expand = c(0.01,0), breaks = seq(30,74,2), limits = c(74,30))
p <- p + theme_classic() + theme(axis.title = element_blank())
p

#ggsave("R data/T2D_AgeDist_by_Stckm60.png",device = "png", plot = p, height = 3.5, width = 6)


# Bar chart distribution by age and gender**************************************

dataAgg <- data[,.(N = sum(weight)), by = .(age, gender, stock_m60)]
dataAgg <- dataAgg[, perc := (N/sum(N)), by = .(stock_m60)]

dataAgg$stock_m60 <- factor(dataAgg$stock_m60, levels = c("d","D","I","x","b","G","S","g")) 
levels(dataAgg$stock_m60) <- c("Antidiabetic","DPP4","Insulins","Lapsed","Biguanides","GLP1 Inj.","SGLT2","GLP1 Oral")

p <- ggplot(dataAgg, aes(x = age, y = perc, fill = gender)) + geom_col(position = "stack")
p <- p + facet_wrap(~stock_m60)
p <- p + scale_fill_manual(values = c("palevioletred1","lightblue"))
p <- p + scale_x_continuous(breaks = seq(18,74,4))
p <- p + scale_y_continuous(limits = c(0,0.11), breaks = seq(0,1,0.025))
p <- p + theme_classic() + theme(legend.position = "None")
p


# Obesity-----------------------------------------------------------------------
data <- flOBELong[p2 == 60, .(patient, weight, s2)]

# bring age and gender
data <- setDT(demo[,c("patid","gender","age")])[data, on = .(patid = patient)]
data <- data[,.(patid, weight, age, gender, stock_m60 = s2)]

length(unique(data$patid)) - nrow(data) # 0

# sample patients per stock
stksmpl <- data[,.(n = .N), by = .(stock_m60)]

# expand observations by their weight - for box_plot with weights***************
data$weight2 <- round(data$weight,0)
dataExp <- expandRows(data, count = "weight2")
dataExp$weight <- NULL

median(dataExp$age[dataExp$stock_m60 != "N"]) # 56 -> median age regardless of stock
medians <- dataExp[,.(median = median(age), mean = mean(age)), by = .(stock_m60)][order(-median)]
dataExp$stock_m60 <- factor(dataExp$stock_m60, levels = c("o","x","N","H","k","G"))
levels(dataExp$stock_m60) <- c("Anorectic","Lapsed","Naive","Surgery","Kampo","GLP1 Inj.")

p <- ggplot(dataExp[stock_m60 != "Surgery" & stock_m60 != "GLP1 Inj." & stock_m60 != "Naive",], aes(x = stock_m60, y = age)) + geom_boxplot(outlier.shape = NA)
#p <- p + facet_wrap(~gender)
p <- p + scale_x_discrete(expand = c(0.3,0))
p <- p + scale_y_continuous(expand = c(0.01,0), breaks = seq(18,74,4), limits = c(18,74))
p <- p + coord_flip()
p <- p + theme_classic() + theme(axis.title = element_blank())
p

#ggsave("R data/Obesity_AgeDist_by_Stckm60_weights.png",device = "png", plot = p, height = 3.2, width = 3.9)

# box_plot with samples*********************************************************
dataS <- data

median(dataS$age[dataS$stock_m60 != "N"]) # 50 -> median age regardless of stock
medians <- dataS[,.(median = median(age), mean = mean(age)), by = .(stock_m60)][order(-median)]
dataS$stock_m60 <- factor(dataS$stock_m60, levels = c("o","x","N","k","H","G"))
levels(dataS$stock_m60) <- c("Anorectic","Lapsed","Naive","Kampo","Surgery","GLP1 Inj.")

p <- ggplot(dataS[stock_m60 != "Surgery" & stock_m60 != "GLP1 Inj." & stock_m60 != "Naive",], aes(x = stock_m60, y = age)) + geom_boxplot(outlier.shape = NA)
#p <- p + facet_wrap(~gender)
p <- p + scale_x_discrete(expand = c(0.3,0))
p <- p + scale_y_continuous(expand = c(0.01,0), breaks = seq(18,74,4), limits = c(18,74))
p <- p + coord_flip()
p <- p + theme_classic() + theme(axis.title = element_blank())
p

#ggsave("R data/Obesity_AgeDist_by_Stckm60_samples.png",device = "png", plot = p, height = 3.2, width = 3.9)

# Bar chart distribution by age and gender**************************************

dataAgg <- data[,.(N = sum(weight)), by = .(age, gender, stock_m60)]
dataAgg <- dataAgg[, perc := (N/sum(N)), by = .(stock_m60)]

dataAgg$stock_m60 <- factor(dataAgg$stock_m60, levels = c("o","x","H","k","G")) 
levels(dataAgg$stock_m60) <- c("Anorectic","Lapsed","Surgery","Kampo","GLP1 Inj.")

p <- ggplot(dataAgg[stock_m60 != "Surgery" & stock_m60 != "GLP1 Inj.",], aes(x = age, y = perc, fill = gender)) + geom_col(position = "stack")
p <- p + facet_wrap(~stock_m60)
p <- p + scale_fill_manual(values = c("palevioletred1","lightblue"))
p <- p + scale_x_continuous(breaks = seq(18,74,4))
p <- p + scale_y_continuous(limits = c(0,0.11), breaks = seq(0,1,0.025))
p <- p + theme_classic() + theme(legend.position = "None")
p




data <- flDIALong[p2 == 60, .(patient,weight,p2,d2)]
data <- data[, GLP1_inj := str_detect(d2, string_GLP1Inj)*1]

sum(data$GLP1_inj) # 3876
sum(data$weight[data$GLP1_inj == 1]) # 259,963



danuBoxArch <- read.xlsx("Architecture/DANU Japan Architecture Stocks - 1.2.xlsx", rows = c(2:13), cols = c(1:3,5:6))
danuBoxArch <- rbind(danuBoxArch, c("N","Naive","Naive","S00","Naive")) # add Naive box specifications

#T2D---------------------------------------------------------------------------------------------------------------------------
flDIAL <- flDIALong
flDIAL <- flDIAL[p1 >= 48]

# Bring architecture box labels
setDT(danuBoxArch)

flDIAL <- danuBoxArch[,.(box_code, Stock.id)][flDIAL, on = .(box_code = s1)]
names(flDIAL)[c(1,2)] <- c("s1","sArch1")
flDIAL <- danuBoxArch[,.(box_code, Stock.id)][flDIAL, on = .(box_code = s2)]
names(flDIAL)[c(1,2)] <- c("s2","sArch2")
flDIAL <- flDIAL[,c(5:11,3,1,4,2,12:16)]

# Architecture stocks - month 60***********************************************************************************************
stocks <- flDIAL[p2 == 60,.(N = sum(weight)), by = .(sArch2)]
stocks <- left_join(stocks, danuBoxArch[,c("Stock.id","Stock.name")], by = c("sArch2" = "Stock.id"))
stocks <- stocks[,c(1,3,2)]
#fwrite(stocks,"R data/DIA_Arch._Stocks.csv")

# Architecture flows - last 12 months******************************************************************************************
flows <- flDIAL[flow == 1,.(N = sum(weight)), by = .(sArch1, sArch2)]
#fwrite(flows,"R data/DIA_Arch._Flows.csv")

# Architecture Secondary Intraflows - changes in drugs other than the stock drugs (primary drugs) - % share********************
# Hierarchy to classify primary vs secondary intraflows: Primary (P) > Secondary (S)

intrFlwL12 <- fread("R data/Intraflows_HighGranularity_table.csv", integer64 = "character", stringsAsFactors = F)
intrFlwL12 <- intrFlwL12[p1 >= 48]
intrFlwL12 <- data.frame(intrFlwL12, stringsAsFactors = F)

intrFlwL12$primary[intrFlwL12$s1 == "b"]   <- (intrFlwL12$big_flow_type[intrFlwL12$s1 == "b"] != "-")*1 # Biguanide
intrFlwL12$secondary[intrFlwL12$s1 == "b"] <- 0 # Biguanide

intrFlwL12$primary[intrFlwL12$s1 == "d"]   <- (intrFlwL12$antiD_flow_type[intrFlwL12$s1 == "d"] != "-")*1 # Antidiabetic
intrFlwL12$secondary[intrFlwL12$s1 == "d"] <- (intrFlwL12$big_flow_type[intrFlwL12$s1 == "d"] != "-")*1 # Antidiabetic

intrFlwL12$primary[intrFlwL12$s1 == "D"]   <- (intrFlwL12$DPP4_flow_type[intrFlwL12$s1 == "D"] != "-")*1 # DPP4
intrFlwL12$secondary[intrFlwL12$s1 == "D"] <- (apply(intrFlwL12[intrFlwL12$s1 == "D", c(10:11)],1,function(x) (sum((x != "-")*1)>0)*1)) # DPP4

intrFlwL12$primary[intrFlwL12$s1 == "S"]   <- (intrFlwL12$SGLT2_flow_type[intrFlwL12$s1 == "S"] != "-")*1 # SGLT2
intrFlwL12$secondary[intrFlwL12$s1 == "S"] <- (apply(intrFlwL12[intrFlwL12$s1 == "S", c(10:12)],1,function(x) (sum((x != "-")*1)>0)*1)) # SGLT2

intrFlwL12$primary[intrFlwL12$s1 == "g"]   <- (intrFlwL12$GLP1O_flow_type[intrFlwL12$s1 == "g"] != "-")*1 # GLP1 Oral
intrFlwL12$secondary[intrFlwL12$s1 == "g"] <- (apply(intrFlwL12[intrFlwL12$s1 == "g", c(10:13)],1,function(x) (sum((x != "-")*1)>0)*1)) # GLP1 Oral

intrFlwL12$primary[intrFlwL12$s1 == "G"]   <- (intrFlwL12$GLP1I_flow_type[intrFlwL12$s1 == "G"] != "-")*1 # GLP1 Injectable
intrFlwL12$secondary[intrFlwL12$s1 == "G"] <- (apply(intrFlwL12[intrFlwL12$s1 == "G", c(10:14)],1,function(x) (sum((x != "-")*1)>0)*1)) # GLP1 Injectable

intrFlwL12$primary[intrFlwL12$s1 == "I"]   <- (intrFlwL12$Ins_flow_type[intrFlwL12$s1 == "I"] != "-")*1 # Insulin
intrFlwL12$secondary[intrFlwL12$s1 == "I"] <- (apply(intrFlwL12[intrFlwL12$s1 == "I", c(10:15)],1,function(x) (sum((x != "-")*1)>0)*1)) # Insulin

any(is.na(intrFlwL12)) # FALSE
intrFlwL12$Prim_Second[intrFlwL12$primary == 1] <- "P"
intrFlwL12$Prim_Second[intrFlwL12$primary == 0 & intrFlwL12$secondary == 1] <- "S"

intrFlwPS  <- aggregate(list(N = intrFlwL12$weight), by = list(stock = intrFlwL12$s1, Prim_Second = intrFlwL12$Prim_Second), sum)
intrFlwPS  <- intrFlwPS[order(intrFlwPS$stock),]
intrFlwPS  <- setDT(intrFlwPS)[, Perc := N/sum(N), by = .(stock)]
intrFlwPS  <- left_join(intrFlwPS, danuBoxArch[,c("box_code","Stock.id","Stock.name")], by = c("stock" = "box_code"))
intrFlwPS  <- intrFlwPS[,.(Stock.id, Stock.name, Prim_Second, N, Perc)]

#fwrite(intrFlwPS,"R data/DIA_Arch._PrimSec_Share.csv")




# Static approach--------------------------------------------------------------------------------------------------
# fixed last 12 month observation window

# Profiling flags**************************************************************************************************

# nr of lines & nr of months rx by patient - last 12 months
data1                <- flDIALong[p1 >= 49, .(patient,weight,p1,d1)]
names(data1)[c(3,4)] <- c("month","drugs")  

data2                <- flDIALong[p2 == 60, .(patient,weight,p2,d2)]
names(data2)[c(3,4)] <- c("month","drugs") 

data <- setDT(data.frame(rbind(data1,data2)))

data$Rx <- (data$drugs != "-")*1 
data <- data[Rx == 1,  ':=' (nr_lines = length(unique(drugs)), months_rx = sum(Rx)), by = .(patient)]
data <- data[Rx == 1]

# nr of unique molecules by patient - last 12 months
data <- data[, nrUnqMols := lapply(.(drugs), function(x) length(unique(unlist(str_split(x,","))))), by = .(patient)]

proFL12 <- unique(data[,.(patient, weight, nr_lines, months_rx, nrUnqMols)])

# nr of flows - last 12 months
nrfl <- flDIALong[p1 >= 48, .(nrFl = sum(flow)), by = .(patient)] 
proFL12 <- nrfl[,.(patient, nrFl)][proFL12, on = .(patient)]
proFL12 <- proFL12[,.(patient, weight, nr_lines, months_rx, nrUnqMols, nrFl)]

# T2D pts switching from SGLT2 to GLPs vs T2D pts performing SGLT2 intraflows+++++++++++++++++++++++++++++++++++++++
# Last 12 months
# A patient may contribute as many times as the nr of flows during the last 12 months he performs

# Patients selection
SGLT2dd <- flDIALong[s1 == "S" & flow == 1 & p1 >= 48]

# Identify pts segments
SGLT2dd <- SGLT2dd[s1 == s2, pts_seg. := "Intra"]
SGLT2dd <- SGLT2dd[s2 == "g" | s2 == "G", pts_seg. := "to_GLP1"]
SGLT2dd <- SGLT2dd[s2 == "x", pts_seg. := "to_Lapsed"]
SGLT2dd <- SGLT2dd[is.na(pts_seg.), pts_seg. := "to_Other"]

SGLT2dd <- proFL12[,.(patient, nr_lines, months_rx, nrUnqMols, nrFl)][SGLT2dd, on = .(patient)]
SGLT2dd <- SGLT2dd[is.na(nr_lines), ':=' (nr_lines = 0, months_rx = 0, nrUnqMols = 0, nrFl = 0)] # These are patients that from m48 to m49 performed a flow from SGLT2 to Lapsed and remained lapsed throughout the last 12 months 
SGLT2dd <- SGLT2dd[,.(disease, patient, weight, p1, p2, d1, d2, s1, s2, pts_seg., nr_lines, months_rx, nrUnqMols, nrFl)]

SGLT2ddMean <- SGLT2dd[, .(mean_nr_lines = mean(nr_lines), mean_months_rx = mean(months_rx), mean_nrUnqMols = mean(nrUnqMols), mean_nrFl = mean(nrFl)),
                       by = .(pts_seg.)]

# T2D pts switching from DPP4 to SGLT2 vs T2D pts performing DPP4 intraflows+++++++++++++++++++++++++++++++++++++++++
# Last 12 months
# A patient may contribute as many times as the nr of flows during the last 12 months he performs

# Patients selection
DPP4dd <- flDIALong[s1 == "D" & flow == 1 & p1 >= 48]

# Identify pts segments
DPP4dd <- DPP4dd[s1 == s2, pts_seg. := "Intra"]
DPP4dd <- DPP4dd[s2 == "S", pts_seg. := "to_SGLT2"]
DPP4dd <- DPP4dd[s2 == "x", pts_seg. := "to_Lapsed"]
DPP4dd <- DPP4dd[is.na(pts_seg.), pts_seg. := "to_Other"]

DPP4dd <- proFL12[,.(patient, nr_lines, months_rx, nrUnqMols, nrFl)][DPP4dd, on = .(patient)]
DPP4dd <- DPP4dd[is.na(nr_lines), ':=' (nr_lines = 0, months_rx = 0, nrUnqMols = 0, nrFl = 0)] # These are patients that from m48 to m49 performed a flow from DPP4 to Lapsed and remained lapsed throughout the last 12 months 
DPP4dd <- DPP4dd[,.(disease, patient, weight, p1, p2, d1, d2, s1, s2, pts_seg., nr_lines, months_rx, nrUnqMols, nrFl)]


# Dynamic approach--------------------------------------------------------------------------------------------------
# entire history up to the moment of switch

# Profiling flags***************************************************************************************************

# nr of lines of therapy over time
nrLines <- fread("R data/DIA_nrLines_Histories.txt", integer64 = "character", stringsAsFactors = F)

# nr of total months Rx over time
mRx <- data.frame(drgDIA2[, lapply(.SD, function(x) (x!="-")*1), by = .(disease, patient, weight)], stringsAsFactors = F)

mRxTot <- mRx
for(i in 2:60){
  cat(i)
  mRxTot[,i+3] <- mRxTot[,i+3] + mRxTot[,i+3-1]
}

# nr of consecutive months Rx over time
mRxCons <- mRx
for(i in 2:60){
  cat(i)
  mRxCons[,i+3] <- ifelse(mRxCons[,i+3] == 0, 0, mRxCons[,i+3] + mRxCons[,i+3-1])
}

# nr of unique molecules over time
data <- data.frame(drgDIA2, stringsAsFactors = F)

nrUnqMol <- data[,c(1:3)]
nrUnqMol$month1 <- lapply(data$month1, function(x) length(unique(unlist(str_split(x[x != "-"], ",")))))

for(i in 2:60){
  cat(i)
  nrUnqMol[,i+3] <- apply(data[,(4:(i+3))], 1, function(x) sum(unique(unlist(str_split(str_c(x[x != "-"], collapse = ","),","))) != ""))
  names(nrUnqMol)[i+3] <- paste0("month",i)
}

# nr of flows over time
data <- data.frame(drgDIA2, stringsAsFactors = F)

nrflw <- data[,c(1:3)]
nrflw$month1 <- 0

for(i in 2:60){
  cat(i)
  nrflw[,i+3] <- (data[,i+3] != data[,i+3-1])*1
  names(nrflw)[i+3] <- paste0("month",i)
}

for(i in 2:60){
  cat(i)
  nrflw[,i+3] <- nrflw[,i+3] + nrflw[,i+3-1]
}

# Transform profiling flags tables to long format
nrLines             <- melt(nrLines, id = c("disease","patient","weight"))
names(nrLines)[4:5] <- c("month","nr_lines")
nrLines             <- nrLines[, month_nr := str_extract_all(month,"[:digit:]+")]
nrLines$month_nr    <- unlist(nrLines$month_nr)
nrLines$month_nr    <- as.integer(nrLines$month_nr)

mRxTot             <- setDT(mRxTot)
mRxTot             <- melt(mRxTot, id = c("disease","patient","weight"))
names(mRxTot)[4:5] <- c("month","Total_months_Rx")
mRxTot             <- mRxTot[, month_nr := str_extract_all(month,"[:digit:]+")]
mRxTot$month_nr    <- unlist(mRxTot$month_nr)
mRxTot$month_nr    <- as.integer(mRxTot$month_nr)

mRxCons             <- setDT(mRxCons)
mRxCons             <- melt(mRxCons, id = c("disease","patient","weight"))
names(mRxCons)[4:5] <- c("month","nr_consec.months_Rx")
mRxCons             <- mRxCons[, month_nr := str_extract_all(month,"[:digit:]+")]
mRxCons$month_nr    <- unlist(mRxCons$month_nr)
mRxCons$month_nr    <- as.integer(mRxCons$month_nr)

nrUnqMol                <- setDT(nrUnqMol)
nrUnqMol                <- melt(nrUnqMol, id = c("disease","patient","weight"))
names(nrUnqMol)[4:5]    <- c("month","nr_unique_mols")
nrUnqMol$nr_unique_mols <- unlist(nrUnqMol$nr_unique_mols)
nrUnqMol                <- nrUnqMol[, month_nr := str_extract_all(month,"[:digit:]+")]
nrUnqMol$month_nr       <- unlist(nrUnqMol$month_nr)
nrUnqMol$month_nr       <- as.integer(nrUnqMol$month_nr)

nrflw             <- setDT(nrflw)
nrflw             <- melt(nrflw, id = c("disease","patient","weight"))
names(nrflw)[4:5] <- c("month","nr_flows")
nrflw             <- nrflw[, month_nr := str_extract_all(month,"[:digit:]+")]
nrflw$month_nr    <- unlist(nrflw$month_nr)
nrflw$month_nr    <- as.integer(nrflw$month_nr)

# Bring profiling flags to the flows auxiliary table - The joining is made on patient and month in p1 ("from month" period)
data <- flDIALong[,.(disease, patient, weight, p1, p2, d1, d2, s1, s2)]

data <- nrLines[,.(patient,month_nr,nr_lines)][data, on = .(patient, month_nr = p1)]
data <- mRxTot[,.(patient,month_nr,Total_months_Rx)][data, on = .(patient, month_nr)]
data <- mRxCons[,.(patient,month_nr,nr_consec.months_Rx)][data, on = .(patient, month_nr)]
data <- nrUnqMol[,.(patient,month_nr,nr_unique_mols)][data, on = .(patient, month_nr)]
data <- nrflw[,.(patient,month_nr,nr_flows)][data, on = .(patient, month_nr)]

data <- data[,.(disease, patient, weight, p1 = month_nr, p2, d1, d2, s1, s2, nr_flows, nr_unique_mols, Total_months_Rx, nr_consec.months_Rx, nr_lines)]

#fwrite(data,"R data/DIA_Flows_&_Pts_Profile.txt")

# Table with Profile flags and stocks - month1 to month 60 - long format
pflg <- data[,.(disease, patient, weight, p1, d1, s1, nr_flows, nr_unique_mols, Total_months_Rx, nr_consec.months_Rx, nr_lines)]

temp <- data[p2 == 60, .(disease, patient, weight, p2, d2, s2)]
temp <- nrLines[,.(patient,month_nr,nr_lines)][temp, on = .(patient, month_nr = p2)]
temp <- mRxTot[,.(patient,month_nr,Total_months_Rx)][temp, on = .(patient, month_nr)]
temp <- mRxCons[,.(patient,month_nr,nr_consec.months_Rx)][temp, on = .(patient, month_nr)]
temp <- nrUnqMol[,.(patient,month_nr,nr_unique_mols)][temp, on = .(patient, month_nr)]
temp <- nrflw[,.(patient,month_nr,nr_flows)][temp, on = .(patient, month_nr)]

temp <- temp[,.(disease, patient, weight, month = month_nr, drug = d2, stock = s2, nr_flows, nr_unique_mols, Total_months_Rx, nr_consec.months_Rx, nr_lines)]
pflg <- pflg[,.(disease, patient, weight, month = p1, drug = d1, stock = s1, nr_flows, nr_unique_mols, Total_months_Rx, nr_consec.months_Rx, nr_lines)] 

pflg <- rbind(pflg, temp)

#fwrite(pflg,"R data/DIA_Stocks_&_Pts_Profile.txt")

# Profilling of T2D pts switching out and within SGLT2****************************************************
# Last 12 months
# A patient may contribute as many times as the nr of flows during the last 12 months he performs

# Patients selection
SGLT2d <- data[s1 == "S" & d1 != d2 & p1 >= 48]

# Identify pts segments
SGLT2d <- SGLT2d[s1 == s2, pts_seg. := "Intra"]
SGLT2d <- SGLT2d[s2 == "g" | s2 == "G", pts_seg. := "to_GLP1"]
SGLT2d <- SGLT2d[s2 == "x", pts_seg. := "to_Lapsed"]
SGLT2d <- SGLT2d[is.na(pts_seg.), pts_seg. := "to_Other"]


SGLT2dMean <- SGLT2d[, .(avg_nr_lines = sum(weight*nr_lines)/sum(weight), avg_Tot_mRx = sum(weight*Total_months_Rx)/sum(weight), 
                         avg_Cons_mRx = sum(weight*nr_consec.months_Rx)/sum(weight), avg_nrUnqMols = sum(weight*nr_unique_mols)/sum(weight), 
                         avg_nrFl = sum(weight*nr_flows)/sum(weight)), by = .(pts_seg.)]

SGLT2dMedian <- SGLT2d[, .(median_nr_lines = median(nr_lines), median_Tot_mRx = median(Total_months_Rx), median_Cons_mRx = median(nr_consec.months_Rx), 
                         median_nrUnqMols = median(nr_unique_mols), median_nrFl = median(nr_flows)), by = .(pts_seg.)]

length(unique(SGLT2d$patient)) # 13,119 patient samples
temp <- unique(SGLT2d[,.(patient,weight)])
sum(temp$weight) # 799,146

#fwrite(SGLT2dMean,"R data/SGLT2_Outflows_Pts_Profiling_L12m.csv")

# Plotting distribution of pts performing SGLT2 Intraflows vs pts performing SGLT2 Outflows to GLP1, on three profile metrics: Nr Flows; nr lines; nr Consecutive months rx;

SGLT2d[pts_seg. == "Intra" | pts_seg. == "to_GLP1", .N] # 12,324

dataFlw <- SGLT2d[pts_seg. == "Intra" | pts_seg. == "to_GLP1", .(patient, weight, nr_flows, pts_seg.)]
dataFlw <- dataFlw[, weight2 := round(weight,0)]
dataFlw <- dataFlw[, .(patient, weight = weight2, nr_flows, pts_seg.)]
dataFlw <- expandRows(dataFlw, count = "weight")

min(dataFlw$nr_flows) # 0
max(dataFlw$nr_flows) # 41

p <- ggplot(dataFlw, aes(x = nr_flows, color = pts_seg., fill = pts_seg.))
p <- p + geom_density(adjust = 3, alpha = 0.2) 
p <- p + scale_fill_manual(values = c("royalblue4","darkcyan"))
p <- p + scale_color_manual(values = c("royalblue4","darkcyan"))
p <- p + scale_x_continuous(expand = c(0.01,0),limits = c(0,42), breaks = seq(0,42,2))
p <- p + scale_y_continuous(expand = c(0.01,0), limits = c(0,0.16))
p <- p + theme_classic() + theme(axis.title = element_blank(), legend.position = "none")
p

#ggsave("R Data/SGLT2ptsDist_Intra_vs_outGLP1_nrFlw.png",device = "png", plot = p, height = 3.5, width = 4.5)

dataLine <- SGLT2d[pts_seg. == "Intra" | pts_seg. == "to_GLP1", .(patient, weight, nr_lines, pts_seg.)]
dataLine <- dataLine[, weight2 := round(weight,0)]
dataLine <- dataLine[, .(patient, weight = weight2, nr_lines, pts_seg.)]
dataLine <- expandRows(dataLine, count = "weight")

min(dataLine$nr_lines) # 0
max(dataLine$nr_lines) # 24

p <- ggplot(dataLine, aes(x = nr_lines, color = pts_seg., fill = pts_seg.))
p <- p + geom_density(adjust = 5, alpha = 0.2) 
p <- p + scale_fill_manual(values = c("royalblue4","darkcyan"))
p <- p + scale_color_manual(values = c("royalblue4","darkcyan"))
p <- p + scale_x_continuous(expand = c(0.01,0),limits = c(0,24), breaks = seq(0,24,2))
p <- p + scale_y_continuous(expand = c(0.01,0), limits = c(0,0.20))
p <- p + theme_classic() + theme(axis.title = element_blank(), legend.position = "none")
p

#ggsave("R Data/SGLT2ptsDist_Intra_vs_outGLP1_nrLines.png",device = "png", plot = p, height = 3.5, width = 4.5)

datamRx <- SGLT2d[pts_seg. == "Intra" | pts_seg. == "to_GLP1", .(patient, weight, nr_consec.months_Rx, pts_seg.)]
datamRx <- datamRx[, weight2 := round(weight,0)]
datamRx <- datamRx[, .(patient, weight = weight2, nr_consec.months_Rx, pts_seg.)]
datamRx <- expandRows(datamRx, count = "weight")

min(datamRx$nr_consec.months_Rx) # 1
max(datamRx$nr_consec.months_Rx) # 59

p <- ggplot(datamRx, aes(x = nr_consec.months_Rx, color = pts_seg., fill = pts_seg.))
p <- p + geom_density(adjust = 2.5, alpha = 0.2) 
p <- p + scale_fill_manual(values = c("royalblue4","darkcyan"))
p <- p + scale_color_manual(values = c("royalblue4","darkcyan"))
p <- p + scale_x_continuous(expand = c(0.01,0),limits = c(1,59), breaks = seq(1,59,3))
p <- p + scale_y_continuous(expand = c(0.01,0), limits = c(0,0.08))
p <- p + theme_classic() + theme(axis.title = element_blank(), legend.position = "none")
p

#ggsave("R Data/SGLT2ptsDist_Intra_vs_outGLP1_nrmRx.png",device = "png", plot = p, height = 3.5, width = 4.5)

# Profilling of T2D pts switching out and within DDP4****************************************************
# Last 12 months
# A patient may contribute as many times as the nr of flows during the last 12 months he performs

# Patients selection
DPP4d <- data[s1 == "D" & d1 != d2 & p1 >= 48]

# Identify pts segments
DPP4d <- DPP4d[s1 == s2, pts_seg. := "Intra"]
DPP4d <- DPP4d[s2 == "S", pts_seg. := "to_SGLT2"]
DPP4d <- DPP4d[s2 == "x", pts_seg. := "to_Lapsed"]
DPP4d <- DPP4d[is.na(pts_seg.), pts_seg. := "to_Other"]

DPP4dMean <- DPP4d[, .(avg_nr_lines = sum(weight*nr_lines)/sum(weight), avg_Tot_mRx = sum(weight*Total_months_Rx)/sum(weight), 
                         avg_Cons_mRx = sum(weight*nr_consec.months_Rx)/sum(weight), avg_nrUnqMols = sum(weight*nr_unique_mols)/sum(weight), 
                         avg_nrFl = sum(weight*nr_flows)/sum(weight)), by = .(pts_seg.)]

DPP4dMedian <- DPP4d[, .(median_nr_lines = median(nr_lines), median_Tot_mRx = median(Total_months_Rx), median_Cons_mRx = median(nr_consec.months_Rx), 
                         median_nrUnqMols = median(nr_unique_mols), median_nrFl = median(nr_flows)), by = .(pts_seg.)]

length(unique(DPP4d$patient)) # 13,415 patient samples
temp <- unique(DPP4d[,.(patient,weight)])
sum(temp$weight) # 1,193,828

#fwrite(DPP4dMean,"R data/DPP4_Outflows_Pts_Profiling_L12m.csv")


# determine time rx on insulin
data <- boxDIA2
data <- data.frame(data[, lapply(.SD, function(x) (x == "I")*1), by = .(disease, patient, weight)], stringsAsFactors = F)

for(i in 2:60){
  cat(i)
  data[,i+3] <- ifelse(data[,i+3] == 0, 0, data[,i+3] + data[,i+3-1])
}


data <- setDT(data)
data <- melt(data, id = c("disease","patient","weight"))
data <- data[,month := str_extract_all(variable,"[:digit:]+")]
names(data)[5] <- "Insulin_time_Rx"
data$month <- unlist(data$month)
data$month <- as.integer(data$month)

# Add time rx on Insulins to the flows auxiliary table
InsOut <- flDIALong[,.(patient,weight,p1,p2,d1,d2,s1,s2)]
InsOut <- data[,.(patient,month,Insulin_time_Rx)][InsOut, on = .(patient, month = p1)]
InsOut <- InsOut[,.(patient,weight,p1 = month,p2,d1,d2,s1,s2,Insulin_time_Rx)]

# Determine avg time Rx on Insulin when switching out from Insulin, by destiny therapy
# Since Insulin therapy is sitting at the top of the hierarchy this can be done at stock level, otherwise it had to be at molecule level
InsOut <- InsOut[s1 == "I" & s2 != "I",]

length(unique(InsOut$patient)) # n = 18,585
temp <- unique(InsOut[,.(patient, weight)])
sum(temp$weight) # N = 1,651,732

# 5 year window
InsOut_5Y <- InsOut[,.(avg_tim_rx = sum(weight*Insulin_time_Rx)/sum(weight)), by = .(s2)]

# last 12 months
InsOut_L12m <- InsOut[p1 >= 48,.(avg_tim_rx = sum(weight*Insulin_time_Rx)/sum(weight)), by = .(s2)]

dataset <- list(Insulin_Outfl_5y = InsOut_5Y, Insulin_Outfl_L12m = InsOut_L12m)
#write.xlsx(dataset,"R data/InsOutfl_Pts_timeInsulinRx.xlsx")



# T2D---------------------------------------------------------------------------

# Stocks & profile flags
data <- fread("R data/DIA_Stocks_&_Pts_Profile.txt", integer64 = "character", stringsAsFactors = F)

# Additional flag: Nr months lapsed
lps <- rbind(flDIALong[,.(disease,patient,weight,month = p1,stock = s1)], flDIALong[p2 == 60, .(disease,patient,weight,month = p2,stock = s2)])
lps <- dcast(lps, formula = disease + patient + weight ~ month, value.var = "stock")
lps <- data.frame(lps, stringsAsFactors = F)

for(i in 1:60){
  cat(i)
  lps[,i+3] <- (lps[,i+3] == "x")*1
}

for(i in 2:60){
  cat(i)
  lps[,i+3] <- lps[,i+3] + lps[,i+3-1] 
}

lps <- setDT(lps)
lps <- melt(lps, id = c("disease", "patient", "weight"))
names(lps)[c(4,5)] <- c("m","nr_months_Lapsed")
lps$month <- unlist(str_extract_all(lps$m,"[:digit:]+"))
lps$m <- NULL
lps$month <- as.integer(lps$month)

data <- setDT(lps[,c("patient","month","nr_months_Lapsed")])[data, on = .(patient, month)]
data <- data[,.(disease,patient,weight,month,drug,stock,nr_flows,nr_unique_mols,Total_months_Rx,nr_consec.months_Rx,nr_lines,nr_months_Lapsed)]

# patients profile by stock in month60
stkProf <- data[month == 60, .(avg_nr_lines = sum(weight*nr_lines)/sum(weight), avg_Tot_mRx = sum(weight*Total_months_Rx)/sum(weight), 
                         avg_Cons_mRx = sum(weight*nr_consec.months_Rx)/sum(weight), avg_nrUnqMols = sum(weight*nr_unique_mols)/sum(weight), 
                         avg_nrFl = sum(weight*nr_flows)/sum(weight), avg_nr_months_Lapsed = sum(weight*nr_months_Lapsed)/sum(weight)), by = .(stock)]

#fwrite(stkProf,"R data/T2D_Pts_profile_by_Stockm60.csv")



#T2D-----------------------------------------------------------------------------------------

#T2D patients
pts <- boxDIA2[,.(patient, weight, month60)]

# Comorbidities history (72m history) - Identify comorbidities for each patient (if a patient has several instances of the same DISEASE throughout the 74m, that DISEASE will show just once). However if a patient has more than one DISEASE that patient will show as many times as different Diseases he has 
# Grouped at DISEASE level (14 unique records), not at standard_disease_name level (321 unique records) 
# Last month of the 60month window of analysis = 15/04/2021 (month 72)

cmHist <- unique(cmbdtDIA[month_and_year_of_medical_care <= ymd("2021-04-15"),.(member_id, Disease)])
any(is.na(cmHist)) # FALSE

cmHist <- cmHist[pts, on = .(member_id = patient)]
cmHist[is.na(cmHist)] <- "-"
cmHist <- cmHist[,.(member_id,weight,stock_m60 = month60,Disease)]
cmHist <- cmHist[, comorbid := ifelse(Disease == "-", 0, 1)]

# Penetration of each comorbidity in the T2D patient population - NON-MECE*******************
DIAcmbPen <- cmHist[,.(N = sum(weight)), by = .(Disease)]
DIAcmbPen <- DIAcmbPen[, TOT_DIApts := sum(pts$weight)]
DIAcmbPen <- DIAcmbPen[Disease == "-", Disease := "NOT Comorbid"]



# Penetration of each comorbidity across each stock (month60)********************************
DIAcmbStkPen <- cmHist[, .(N = sum(weight)), by = .(stock_m60, Disease)]
stkPts       <- pts[, .(TOT_DIApts = sum(weight)), by = .(month60)]
stkPts       <- stkPts[, .(stock_m60 = month60, TOT_DIApts)]
DIAcmbStkPen <- stkPts[DIAcmbStkPen, on = .(stock_m60)]
DIAcmbStkPen <- DIAcmbStkPen[,.(stock_m60, Disease, N, TOT_DIApts)]
DIAcmbStkPen <- DIAcmbStkPen[Disease == "-", Disease := "No Co-morbidity"]

stkPtsSmpl   <- pts[, .(DIApts_smples = .N), by = .(month60)]
#fwrite(stkPtsSmpl,"R data/DIA_Stkm60_ptsSamples.csv")

# For each comorbidity distribution of patients by stock (month60)***************************
# A patient may count severl times depending on the nr of comorbidities he has but within each comorbidity a patient counts only once  
DIAcmbPatSh  <- cmHist[, .(N = sum(weight)), by = .(Disease, stock_m60)][order(Disease)]
cmbdtPts     <- cmHist[, .(Tot_cmbdtPts = sum(weight)), by = .(Disease)]
DIAcmbPatSh  <- cmbdtPts[DIAcmbPatSh, on = .(Disease)]
DIAcmbPatSh  <- DIAcmbPatSh[,.(Disease, stock_m60, N, Tot_cmbdtPts)]
DIAcmbPatSh  <- DIAcmbPatSh[Disease == "-", Disease := "No Co-morbidity"]


dataset <- list(Cmbdt_Penetration = DIAcmbPen, Cmbdt_Stock_Penetration = DIAcmbStkPen, Cmbdt_Pts_Stock_Share = DIAcmbPatSh)
#write.xlsx(dataset, "R data/DIA_Comorbidities_Stocks.xlsx")


#Obesity-----------------------------------------------------------------------------------------

#Obesity patients
pts <- boxOBE[,.(patient, weight, month60)]
pts$month60 <- unlist(lapply(pts$month60, function(x) str_extract_all(x,"[:alpha:]"))) 

temp <- data.frame(boxOBE, stringsAsFactors = F)
temp$Ever_Rx <- apply(temp[,c(4:63)], 1, function(x) (sum(str_detect(x,"x")*1) != 60)*1)

pts <- setDT(temp[,c("patient", "Ever_Rx")])[pts, on = .(patient)]
pts <- pts[Ever_Rx == 0, month60 := "N"]

# Comorbidities history (72m history) - Identify comorbidities for each patient (if a patient has several instances of the same DISEASE throughout the 74m, that DISEASE will show just once). However if a patient has more than one DISEASE that patient will show as many times as different Diseases he has 
# Grouped at DISEASE level (14 unique records), not at standard_disease_name level (321 unique records) 
# Last month of the 60month window of analysis = 15/04/2021 (month 72)

cmHist <- unique(cmbdtOBE[month_and_year_of_medical_care <= ymd("2021-04-15"),.(member_id, Disease)])
any(is.na(cmHist)) # FALSE

cmHist <- cmHist[pts, on = .(member_id = patient)]
cmHist[is.na(cmHist)] <- "-"
cmHist <- cmHist[,.(member_id, weight, stock_m60 = month60,Disease)]
cmHist <- cmHist[, comorbid := ifelse(Disease == "-", 0, 1)]

# Penetration of each comorbidity in the Obesity patient population - NON-MECE*******************
OBEcmbPen <- cmHist[,.(N = sum(weight)), by = .(Disease)]
OBEcmbPen <- OBEcmbPen[, TOT_OBEpts := sum(pts$weight)]
OBEcmbPen <- OBEcmbPen[Disease == "-", Disease := "NOT Comorbid"]


# Penetration of each comorbidity across each stock (month60)********************************
OBEcmbStkPen <- cmHist[, .(N = sum(weight)), by = .(stock_m60, Disease)]
stkPts       <- pts[, .(TOT_OBEpts = sum(weight)), by = .(month60)]
stkPts       <- stkPts[, .(stock_m60 = month60, TOT_OBEpts)]
OBEcmbStkPen <- stkPts[OBEcmbStkPen, on = .(stock_m60)]
OBEcmbStkPen <- OBEcmbStkPen[,.(stock_m60, Disease, N, TOT_OBEpts)]
OBEcmbStkPen <- OBEcmbStkPen[Disease == "-", Disease := "No Co-morbidity"]

stkPtsSmpl   <- pts[, .(OBEpts_smples = .N), by = .(month60)]
#fwrite(stkPtsSmpl,"R data/OBE_Stkm60_ptsSamples.csv")

# For each comorbidity distribution of patients by stock (month60)***************************
# A patient may count several times depending on the nr of comorbidities he has but within each comorbidity a patient counts only once  
OBEcmbPatSh  <- cmHist[, .(N = sum(weight)), by = .(Disease, stock_m60)][order(Disease)]
cmbdtPts     <- cmHist[, .(Tot_cmbdtPts = sum(weight)), by = .(Disease)]
OBEcmbPatSh  <- cmbdtPts[OBEcmbPatSh, on = .(Disease)]
OBEcmbPatSh  <- OBEcmbPatSh[,.(Disease, stock_m60, N, Tot_cmbdtPts)]
OBEcmbPatSh  <- OBEcmbPatSh [Disease == "-", Disease := "No Co-morbidity"]

dataset <- list(Cmbdt_Penetration = OBEcmbPen, Cmbdt_Stock_Penetration = OBEcmbStkPen, Cmbdt_Pts_Stock_Share = OBEcmbPatSh)
#write.xlsx(dataset, "R data/OBE_Comorbidities_Stocks.xlsx")



# patients: T2D patients
# flows period: last 12 months (month 49 to month 60)
# flows: outflows from Insulin
# comorbidity history: 
#    - history up to previous month of the last 12 months (month 48 => dx's up to 2020-04-15)
#    - Comorbidities history - Identify comorbidities for each patient (if a patient has several instances of the same DISEASE throughout the lookback window, that         DISEASE will show just once). However if a patient has more than one DISEASE that patient will show as many times as different Diseases he has. 
#    - Grouped at DISEASE level (14 unique records), not at standard_disease_name level (321 unique records) 


# Outflows from Insulins - last 12m
fl <- flDIALong[p1 >= 48 & s1 == "I" & s2 != "I", .(patient, weight, p1, p2, d1, d2, s1, s2)]

# Comorbidities history
cmHist <- unique(cmbdtDIA[month_and_year_of_medical_care <= ymd("2020-04-15"),.(member_id, Disease)])
any(is.na(cmHist)) # FALSE

cmHist <- cmHist[str_detect(Disease,"Cardiovascular"), Disease_group := "Cardiovascular"]
cmHist <- cmHist[!str_detect(Disease, "Cardiovascular"), Disease_group := Disease]
any(is.na(cmHist)) # FALSE

cmHist <- dcast(cmHist, formula = member_id ~ Disease_group, value.var = "Disease", fill = 0, fun.aggregate = length)
cmHist$Cardiovascular[cmHist$Cardiovascular > 0] <- 1 

# Bring comorbidities flag to the flows table
fl <- cmHist[fl, on = .(member_id = patient)]
fl <- fl[,.(patient = member_id,weight,p2,s2,Atherosclerosis,CKD,Cardiovascular,`Heart Failure`,Hyperlipidemia,Hypertension,NASH,`Sleep apnoea`)]
fl[is.na(fl)] <- 0
fl <- fl[, NOT_comorbid := (rowSums(.SD) == 0)*1,
         .SDcols = c("Atherosclerosis","CKD","Cardiovascular","Heart Failure","Hyperlipidemia","Hypertension","NASH","Sleep apnoea")]

fl <- melt(fl, id = c("patient","weight","p2","s2"))
names(fl)[c(5,6)] <- c("Disease","Disease_flag")
fl <- fl[Disease_flag == 1]

# Distribution of Insulin 'Outflowers' patients by comorbidity - Non-Mece****************************************
insOutPts <- unique(fl[,.(patient, weight, Disease)])
insOutPts <- insOutPts[,.(N = sum(weight)), by = Disease]

insOutflwers <- unique(fl[,.(patient, weight)])[,.(sum(weight))]
insOutPts <- insOutPts[,TOT_InsOutflowers := insOutflwers]

#fwrite(insOutPts, "R data/Insulin_Outflowers_comorbidities.csv")


# Distribution of insulin outflows by destiny stock and comorbidity - Non-Mece****************************************
# a patient may contribute as many times as the nr of flows he performs x his nr of different comorbidities, and may also be accounted for in many different destiny stocks

TotFlwStk <- unique(fl[,.(patient,weight,p2,s2)])[,.(TOT_InsOutfl = sum(weight)), by = .(s2)]
insOflcmb <- fl[,.(N = sum(weight)), by = .(s2, Disease)]
insOflcmb <- TotFlwStk[insOflcmb, on = .(s2)]
insOflcmb <- insOflcmb[,.(s2,Disease,N, TOT_InsOutfl)][order(s2,Disease)]

#fwrite(insOflcmb, "R data/Insulin_Outflows_by_stock_comorbidity.csv")

