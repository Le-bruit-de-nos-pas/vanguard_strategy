
# Demographics
DANU_Demographics <- fread("DANU Demographics.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Demographics <- DANU_Demographics[ diagnosis!="-" , c("patid", "weight", "diagnosis", "obesity_condition")]
DANU_Demographics[ ,list(sum=sum(weight)), by=diagnosis]

#MAX  BMI
DANU_Measures <- fread("DANU Measures.txt",  integer64 = "character", stringsAsFactors = F)
DANU_Measures$claimed <- as.Date(DANU_Measures$claimed)
range(DANU_Measures$claimed)
DANU_Measures <- DANU_Measures[ test=="BMI" , c("patid", "test", "value", "claimed")]

#*
# setorder(DANU_Measures, cols = "patid", "test", -"claimed")  # If using LAST, ignore
# DANU_Measures <- DANU_Measures[, head(.SD, 1), by = "patid"]  # If using LAST, ignore
#*
#*
DANU_Measures <- DANU_Measures[DANU_Measures[, .I[which.max(value)], by=patid]$V1]   # MAX record per patient

DANU_Demographics <- DANU_Demographics[DANU_Measures, on = .(patid), nomatch = NULL]

DANU_Demographics[ ,list(sum=sum(weight)), by=diagnosis]

DANU_Demographics[ value >= 25 ,list(sum=sum(weight)), by=diagnosis]
 
DANU_Demographics[ value >= 27 ,list(sum=sum(weight)), by=diagnosis]

DANU_Demographics[ value >= 30 ,list(sum=sum(weight)), by=diagnosis]




# The Obesity Only patient to be kept and projected later on ->

Pats_25Plus <- DANU_Demographics[  value >= 25 & diagnosis=="Obesity" ,  c("patid", "weight")]
length(unique(Pats_25Plus$patid))

Pats_25_30 <- DANU_Demographics[  value >= 25 & value < 30 & diagnosis=="Obesity" ,  c("patid", "weight")]
length(unique(Pats_25_30$patid))
sum(Pats_25_30$weight)

Pats_30Plus <- DANU_Demographics[  value >= 30 & diagnosis=="Obesity" ,  c("patid", "weight")]
length(unique(Pats_30Plus$patid))
sum(Pats_30Plus$weight)





# Pats_27_30 <- DANU_Demographics[ value < 30 & value >= 27 & diagnosis=="Obesity" ,  c("patid")]


# Comorbidities 

Pats_25Plus <- DANU_Demographics[  value >= 25 & diagnosis=="Obesity" ,  c("patid", "weight")]

OBE_Comorbidity_Inventories <- fread("OBE Comorbidity Inventories.txt")

OBE_Comorbidity_Inventories <- OBE_Comorbidity_Inventories[Pats_25Plus, on = .(patid), nomatch = NULL]

CKD <- unique(OBE_Comorbidity_Inventories[grepl("N18",diagnosis), c("patid","weight")])
POS <- unique(OBE_Comorbidity_Inventories[grepl("E28",diagnosis), c("patid","weight")])
PAD <- unique(OBE_Comorbidity_Inventories[grepl("I70",diagnosis)|grepl("I73",diagnosis), c("patid","weight")])
SLEEPAPNEA <- unique(OBE_Comorbidity_Inventories[grepl("G47",diagnosis), c("patid","weight")])
HF <- unique(OBE_Comorbidity_Inventories[grepl("I5",diagnosis), c("patid","weight")])
DISLIPIDEMIA <- unique(OBE_Comorbidity_Inventories[grepl("E78",diagnosis), c("patid","weight")])
HTN <- unique(OBE_Comorbidity_Inventories[grepl("I10",diagnosis), c("patid","weight")])
OA <- unique(OBE_Comorbidity_Inventories[grepl("M15", diagnosis)|grepl("M16", diagnosis)|grepl("M17", diagnosis)|
                                     grepl("M18", diagnosis)| grepl("M19", diagnosis), c("patid","weight")])
NASH <- unique(OBE_Comorbidity_Inventories[grepl("K75",diagnosis), c("patid","weight")])
PREDIABETES <- unique(OBE_Comorbidity_Inventories[grepl("R73",diagnosis), c("patid","weight")])
IHD <- unique(OBE_Comorbidity_Inventories[grepl("I20", diagnosis)|grepl("I21", diagnosis)| grepl("I22", diagnosis)|
                                   grepl("I23", diagnosis)| grepl("I24", diagnosis)| grepl("I25", diagnosis), c("patid","weight")])


Comorb <- Reduce(function (...) { merge(..., all = TRUE) },    list(CKD, POS, PAD, HF, SLEEPAPNEA, DISLIPIDEMIA, HTN, OA, NASH, IHD, PREDIABETES)) 

length(unique(Comorb$patid))

Comorb <- Comorb[Pats_25Plus, on = .(patid), nomatch = NULL]

sum(Comorb$weight) 


